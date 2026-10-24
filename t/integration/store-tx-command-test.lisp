;;;;
;;;; Transaction dispatch, outcomes, diffs, and rebase edge cases.
(in-package #:aitools.store.test)

(defun run-cli (root &rest arguments)
  "(values exit-code envelope) of `aitools --root ROOT ARGUMENTS...` through
the composition root; the envelope is parsed from stdout, else stderr."
  (multiple-value-bind (app registry) (aitools/cli:build-app)
    (let* ((out (make-string-output-stream))
           (err (make-string-output-stream))
           (code (aitools/cli:dispatch app registry (list* "aitools" "--root" root arguments)
                                       :stdout out :stderr err))
           (stdout (get-output-stream-string out))
           (stderr (get-output-stream-string err))
           (envelope (json-kit:parse (if (plusp (length stdout)) stdout stderr))))
      (expect envelope :to-match-envelope '())
      (values code envelope))))



(defun envelope-field (envelope &rest keys)
  (reduce (lambda (value key) (and value (gethash key value))) keys :initial-value envelope))

(defun call-with-cli-store (function)
  "Call FUNCTION with (ROOT STORE) for a fresh workspace, XDG_STATE_HOME
redirected to a scratch directory so the CLI opens its own store there."
  (call-with-temp-store (lambda (scratch) (let* ((root (store-root scratch))
           (xdg (concatenate 'string root "/../xdg"))
           (previous (sb-posix:getenv "XDG_STATE_HOME")))
      (sb-posix:mkdir xdg #o700)
      (unwind-protect
           (progn (sb-posix:setenv "XDG_STATE_HOME" xdg 1)
                  (funcall function root (aitools.store.infrastructure:make-posix-store root)))
        (if previous
            (sb-posix:setenv "XDG_STATE_HOME" previous 1)
            (sb-posix:unsetenv "XDG_STATE_HOME"))))) :io nil))

(describe "aitools chmod --tx through dispatch"
  (it "stages the mode, leaves the disk alone until tx commit, then changes only the mode"
    (call-with-cli-store
     (lambda (root store)
       (put-file store "cfg.txt" "secret" :mode #o644)
       (let ((tx (envelope-field (nth-value 1 (run-cli root "tx" "begin")) "tx")))
         (multiple-value-bind (code envelope)
             (run-cli root "chmod" "--mode" "600"
                      (disk-path store "cfg.txt")
                      "--tx" tx)
           (expect (list code (envelope-field envelope "error" "code")) :to-equal '(0 nil))
           (expect (envelope-field envelope "tx") :to-equal tx)
           (expect (envelope-field envelope "tx_op") :to-be 1))
         (expect (disk-mode store "cfg.txt") :to-be #o644)
         (multiple-value-bind (code envelope) (run-cli root "tx" "commit" tx)
           (expect code :to-be 0)
           (expect (envelope-field (aref (envelope-field envelope "changes") 0) "action") :to-equal "mode-changed"))
         (expect (disk-mode store "cfg.txt") :to-be #o600)
         (expect (disk-text store "cfg.txt") :to-equal "secret")))))

  (it "drops the staged chmod so the commit leaves the mode as it was"
    (call-with-cli-store
     (lambda (root store)
       (put-file store "cfg.txt" "secret" :mode #o644)
       (let ((tx (envelope-field (nth-value 1 (run-cli root "tx" "begin")) "tx")))
         (expect (run-cli root "chmod" "--mode" "600" (disk-path store "cfg.txt") "--tx" tx) :to-be 0)
         (expect (run-cli root "tx" "drop" tx "1") :to-be 0)
         (expect (run-cli root "tx" "commit" tx) :to-be 0)
         (expect (disk-mode store "cfg.txt") :to-be #o644)
         (expect (disk-text store "cfg.txt") :to-equal "secret"))))))

(defun stage-outcome (store tx-id validate)
  (tx-stage/k store tx-id '("test") validate
              :on-staged (lambda (tx-op results) (list :staged tx-op (actions results)))
              :on-rejected (lambda (code message &rest keys) (declare (ignore keys)) (list :rejected code message))
              :on-not-found (lambda () :not-found)
              :on-busy (lambda () :busy)))

(defun committing (&rest requests)
  (lambda (view commit reject)
    (declare (ignore view reject))
    (funcall commit requests)))

(defun rebase-outcome (store tx-id replay)
  (tx-rebase/k store tx-id replay
               :on-rebased (lambda (paths) (list :rebased paths))
               :on-conflict (lambda (conflicts)
                              (list :conflict
                                    (mapcar (lambda (c)
                                              (list (conflict-path c) (conflict-kind c)))
                                            conflicts)))
               :on-not-found (lambda () :not-found) :on-busy (lambda () :busy)))

(describe "aitools.store tx-stage/k outcomes"
  (it-each (("mkdir of an existing directory" :noop (:staged nil ()))
            ("a whole-directory move" :directory-move
             (:rejected "argument.invalid" "directory moves in tx must be staged per path"))
            ("a delete of a missing path" :missing (:rejected "input.not-found" "missing.txt does not exist"))
            ("a validation refusal" :refused (:rejected "refusal.custom" "no")))
      "answers ~A"
      (label case expected)
    (declare (ignore label))
    (call-with-temp-store (lambda (store) (put-file store "d/f.txt" "f")
      (let ((tx (begin store)))
        (expect (stage-outcome store tx
                               (ecase case
                                 (:noop (committing (mkdir-request "d")))
                                 (:directory-move (committing (move-request "d" "e")))
                                 (:missing (committing (delete-request "missing.txt")))
                                 (:refused (lambda (view commit reject)
                                             (declare (ignore view commit))
                                             (funcall reject "refusal.custom" "no")))))
                :to-equal expected)
        (expect (tx-status-ops (status-of store tx)) :to-equal '()))) :io nil))

  (it "signals when VALIDATE calls neither continuation"
    (call-with-temp-store (lambda (store) (let ((tx (begin store)))
        (expect (handler-case (stage-outcome store tx (lambda (view commit reject)
                                                        (declare (ignore view commit reject))))
                  (error (condition) (princ-to-string condition)))
                :to-equal "tx-stage/k: VALIDATE returned without calling COMMIT or REJECT"))) :io nil))

  (it "refuses to record a read of a path that is not workspace-relative"
    (call-with-temp-store (lambda (store) (let ((tx (begin store)))
        (expect (handler-case (tx-record-read/k store tx "../x" :on-recorded #'identity
                                                                :on-not-found (lambda ()) :on-busy (lambda ()))
                  (error (condition) (princ-to-string condition)))
                :to-equal "tx-record-read/k: \"../x\" is not a workspace-relative path"))) :io nil)))

(describe "aitools.store tx diff and commit of every staged kind"
  (it "names each change's action and commits them all"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a" :mode #o644)
      (put-file store "gone.txt" "g")
      (put-file store "m.txt" "m")
      (sb-posix:mkdir (disk-path store "d") #o755)
      (let ((tx (begin store)))
        (stage store tx (list (delete-request "gone.txt")
                              (symlink-request "ln" "a.txt")
                              (chmod-request "a.txt" #o600)
                              (mtime-request "m.txt" 1577934245)
                              (chmod-request "d" #o700)))
        (expect (tx-diff/k store tx :on-diff #'actions :on-not-found (lambda () :not-found))
                :to-equal '(("a.txt" :mode-changed) ("d" :mode-changed) ("gone.txt" :deleted)
                            ("ln" :linked) ("m.txt" :modified)))
        (expect (commit-tx store tx) :to-be :committed)
        (expect (list (disk-mode store "a.txt") (disk-mode store "d")) :to-equal '(#o600 #o700))
        (expect (disk-text store "gone.txt") :to-be :absent)
        (expect (sb-posix:readlink (disk-path store "ln")) :to-equal "a.txt")
        (expect (sb-posix:stat-mtime (sb-posix:lstat (disk-path store "m.txt"))) :to-be 1577934245))) :io nil))

  (it "lists only real transactions, skipping other names under the tx root"
    (call-with-temp-store (lambda (store) (let ((tx (begin store))
            (root (tx-root-directory (store-state-directory store))))
        (sb-posix:mkdir (join-path root "junk") #o700)
        (sb-posix:mkdir (join-path root (format-tx-id 3964000000 "00000000")) #o700)
        (expect (mapcar #'tx-status-id (tx-list store)) :to-equal (list tx)))) :io nil)))

(describe "aitools.store tx rebase and drop edge cases"
  (it "restages an op on an undrifted path and replays the drifted one"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (put-file store "b.txt" "b")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1" :replayable nil)
        (stage-append store tx "b.txt" "+2")
        (put-file store "b.txt" "external")
        (expect (rebase-outcome store tx #'replay-append) :to-equal '(:rebased ("b.txt")))
        (expect (tx-view-text store tx "a.txt") :to-equal "a+1")
        (expect (tx-view-text store tx "b.txt") :to-equal "external+2")
        (expect (mapcar #'tx-op-record-replayable (tx-status-ops (status-of store tx))) :to-equal '(nil t))
        (expect (commit-tx store tx) :to-be :committed)
        (expect (list (disk-text store "a.txt") (disk-text store "b.txt")) :to-equal '("a+1" "external+2")))) :io nil))

  (it "reports a conflict when the replay rejects, leaving the tx as it was"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (put-file store "a.txt" "external")
        (expect (rebase-outcome store tx (lambda (record view commit reject)
                                           (declare (ignore record view commit))
                                           (funcall reject "selection.not-found" "gone")))
                :to-equal '(:conflict (("a.txt" :write))))
        (expect (rebase-outcome store tx (lambda (record view commit reject)
                                           (declare (ignore record view reject))
                                           (funcall commit (list (move-request "a.txt" "a.txt")))))
                :to-equal '(:conflict (("a.txt" :write))))
        (expect (tx-view-text store tx "a.txt") :to-equal "a+1"))) :io nil))

  (it "signals when REPLAY calls neither continuation"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (put-file store "a.txt" "external")
        (expect (handler-case (rebase-outcome store tx (lambda (record view commit reject)
                                                         (declare (ignore record view commit reject))))
                  (error (condition) (princ-to-string condition)))
                :to-equal "tx-rebase/k: REPLAY returned without calling COMMIT or REJECT"))) :io nil))

  (it-each ((0) ("1") (2))
      "answers not-found for tx drop of op ~S"
      (tx-op)
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (expect (tx-drop/k store tx tx-op :on-dropped #'identity :on-not-found (lambda () :not-found)
                                          :on-busy (lambda () :busy))
                :to-be :not-found)
        (expect (tx-view-text store tx "a.txt") :to-equal "a+1"))) :io nil)))
