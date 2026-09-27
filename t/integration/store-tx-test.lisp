;;;; t/integration/store-tx-test.lisp
;;;;
;;;; Transaction storage behaviour (docs/src/reference/transactions.md)
;;;; against a real
;;;; temporary workspace: overlay reads, write-set and read-set conflicts,
;;;; rebase, drop, abort, and interruption of tx ops and of commit.
(in-package #:aitools.store.test)

(defun begin (store &optional name)
  (tx-begin/k store :name name :on-begun (lambda (tx-id name created) (declare (ignore name created)) tx-id)
                    :on-busy (lambda () :busy)))

(defun append-requests (view argv)
  "The requests for the test's content-based op (\"append\" path text):
append TEXT to PATH as the view shows it."
  (destructuring-bind (command path text) argv
    (declare (ignore command))
    (let ((current (view-read-file view path)))
      (list (write-file-request path (bytes (concatenate 'string (if current (octets-string current) "") text)))))))

(defun stage-append (store tx-id path text &key (replayable t))
  (let ((argv (list "append" path text)))
    (tx-stage/k store tx-id argv
                (lambda (view commit reject)
                  (declare (ignore reject))
                  (funcall commit (append-requests view argv)))
                :replayable replayable
                :on-staged (lambda (tx-op results) (declare (ignore results)) tx-op)
                :on-rejected (lambda (code &rest rest) (declare (ignore rest)) code)
                :on-not-found (lambda () :not-found)
                :on-busy (lambda () :busy))))

(defun stage (store tx-id requests)
  (tx-stage/k store tx-id '("test")
              (lambda (view commit reject)
                (declare (ignore view reject))
                (funcall commit requests))
              :on-staged (lambda (tx-op results) (declare (ignore results)) tx-op)
              :on-rejected (lambda (code &rest rest) (declare (ignore rest)) code)
              :on-not-found (lambda () :not-found)
              :on-busy (lambda () :busy)))

(defun tx-view-text (store tx-id path)
  (call-with-tx-view/k store tx-id
                       :on-view (lambda (view)
                                  (let ((octets (view-read-file view path)))
                                    (if octets (octets-string octets) (entry-state-kind (view-path-state view path)))))
                       :on-not-found (lambda () :not-found)))

(defun status-of (store tx-id)
  (tx-status/k store tx-id :on-status #'identity :on-not-found (lambda () :not-found)))

(defun commit-tx (store tx-id &key ignore-stale-reads)
  (tx-commit/k store tx-id '("tx" "commit")
               :ignore-stale-reads ignore-stale-reads
               :on-committed (lambda (op-id results) (values :committed op-id results))
               :on-rejected (lambda (code message &rest keys) (values :rejected code message keys))
               :on-not-found (lambda () :not-found)
               :on-busy (lambda () :busy)))

(defun replay-append (record view commit reject)
  (declare (ignore reject))
  (funcall commit (append-requests view (tx-op-record-argv record))))

(defun blob-names (store)
  (funcall (store-io-list-directory (store-io-port store)) (blobs-directory (store-state-directory store))))

(describe "aitools.store tx overlay"
  (it "stages writes without touching the working tree and reads them back through the tx"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (put-file store "gone.txt" "g")
      (let ((tx (begin store "demo")))
        (expect (valid-tx-id-p tx) :to-be-truthy)
        (expect (stage-append store tx "a.txt" "+1") :to-be 1)
        (expect (stage store tx (list (delete-request "gone.txt") (write-file-request "dir/new.txt" (bytes "n"))))
                :to-be 2)
        (expect (disk-text store "a.txt") :to-equal "a")
        (expect (disk-text store "gone.txt") :to-equal "g")
        (expect (disk-text store "dir") :to-be :absent)
        (expect (tx-view-text store tx "a.txt") :to-equal "a+1")
        (expect (tx-view-text store tx "gone.txt") :to-be :absent)
        (expect (tx-view-text store tx "dir/new.txt") :to-equal "n")
        (call-with-tx-view/k store tx
                             :on-view (lambda (view)
                                        (expect (view-directory-entries view "")
                                                :to-equal '(("a.txt" . :file) ("dir" . :directory)))
                                        ;; --expect-hash inside a tx compares with the tx state.
                                        (expect (entry-state-hash (view-path-state view "a.txt"))
                                                :to-equal (aitools.kernel.domain:content-hash (bytes "a+1"))))
                             :on-not-found (lambda () (expect :not-found :to-be nil)))
        (let ((status (status-of store tx)))
          (expect (tx-status-name status) :to-equal "demo")
          (expect (mapcar #'tx-op-record-tx-op (tx-status-ops status)) :to-equal '(1 2))
          (expect (mapcar #'tx-path-path (tx-status-paths status)) :to-equal '("a.txt" "dir" "dir/new.txt" "gone.txt"))
          (expect (tx-status-drift status) :to-equal '()))
        (expect (mapcar #'tx-status-id (tx-list store)) :to-equal (list tx)))))

  (it "diffs base to staged with both contents"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (stage store tx (list (write-file-request "b.txt" (bytes "b"))))
        (tx-diff/k store tx
                   :on-diff (lambda (results)
                              (expect (actions results) :to-equal '(("a.txt" :modified) ("b.txt" :created)))
                              (expect (octets-string (change-result-before-content (first results))) :to-equal "a")
                              (expect (octets-string (change-result-after-content (first results))) :to-equal "a+1"))
                   :on-not-found (lambda () (expect :not-found :to-be nil))))))

  (it "answers not-found for an unknown or malformed tx id"
    (with-temp-store (store)
      (expect (stage-append store (format-tx-id 3964000000 "00000000") "a" "x") :to-be :not-found)
      (expect (stage-append store "../../x" "a" "x") :to-be :not-found)
      (expect (status-of store "tx-nope") :to-be :not-found))))

(describe "aitools.store tx commit"
  (it "applies the whole tx as one journal op, removes the tx, and undoes as a unit"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (put-file store "gone.txt" "g")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (stage-append store tx "a.txt" "+2")
        (stage store tx (list (delete-request "gone.txt") (write-file-request "dir/new.txt" (bytes "n"))))
        (multiple-value-bind (status op-id results) (commit-tx store tx)
          (expect status :to-be :committed)
          (expect (actions results)
                  :to-equal '(("a.txt" :modified) ("dir" :created) ("dir/new.txt" :created) ("gone.txt" :deleted)))
          (expect (length (read-journal store)) :to-be 1)
          (expect (disk-text store "a.txt") :to-equal "a+1+2")
          (expect (disk-text store "dir/new.txt") :to-equal "n")
          (expect (disk-text store "gone.txt") :to-be :absent)
          (expect (status-of store tx) :to-be :not-found)
          (expect (undo store op-id) :to-be :committed)
          (expect (disk-text store "a.txt") :to-equal "a")
          (expect (disk-text store "gone.txt") :to-equal "g")
          (expect (disk-text store "dir") :to-be :absent)))))

  (it "refuses a write-set conflict with kind write and writes nothing"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (put-file store "b.txt" "b")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+tx")
        (stage-append store tx "b.txt" "+tx")
        (put-file store "a.txt" "external")
        (expect (tx-status-drift (status-of store tx)) :to-equal '("a.txt"))
        (multiple-value-bind (status code message keys) (commit-tx store tx)
          (declare (ignore message))
          (expect status :to-be :rejected)
          (expect code :to-equal "refusal.target-changed")
          (expect (mapcar (lambda (c) (list (conflict-path c) (conflict-kind c))) (getf keys :conflicts))
                  :to-equal '(("a.txt" :write))))
        (expect (disk-text store "a.txt") :to-equal "external")
        (expect (disk-text store "b.txt") :to-equal "b")
        (expect (read-journal store) :to-equal '()))))

  (it "refuses a stale read with kind read, until re-read or ignored"
    (with-temp-store (store)
      (put-file store "config.txt" "v1")
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (flet ((record-read ()
                 (tx-record-read/k store tx "config.txt"
                                   :on-recorded (lambda (state) (declare (ignore state)) :recorded)
                                   :on-not-found (lambda () :not-found) :on-busy (lambda () :busy))))
          (expect (record-read) :to-be :recorded)
          (stage-append store tx "a.txt" "+tx")
          (put-file store "config.txt" "v2")
          (expect (tx-status-stale-reads (status-of store tx)) :to-equal '("config.txt"))
          (multiple-value-bind (status code message keys) (commit-tx store tx)
            (declare (ignore message))
            (expect status :to-be :rejected)
            (expect code :to-equal "refusal.target-changed")
            (expect (mapcar #'conflict-kind (getf keys :conflicts)) :to-equal '(:read)))
          (expect (record-read) :to-be :recorded)
          (expect (commit-tx store tx) :to-be :committed)
          (expect (disk-text store "a.txt") :to-equal "a+tx")))))

  (it "commits over a stale read with --ignore-stale-reads"
    (with-temp-store (store)
      (put-file store "config.txt" "v1")
      (let ((tx (begin store)))
        (tx-record-read/k store tx "config.txt" :on-recorded #'identity
                                                :on-not-found (lambda ()) :on-busy (lambda ()))
        (stage store tx (list (write-file-request "out.txt" (bytes "o"))))
        (put-file store "config.txt" "v2")
        (expect (commit-tx store tx) :to-be :rejected)
        (expect (commit-tx store tx :ignore-stale-reads t) :to-be :committed))))

  (it "rolls forward a commit interrupted after its commit point and removes the tx"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (stage store tx (list (write-file-request "b.txt" (bytes "b"))))
        (with-fault-at (:after-apply-step :nth 1) (commit-tx store tx))
        (expect (mapcar #'cdr (nth-value 1 (recover store))) :to-equal '("rolled-forward"))
        (expect (disk-text store "a.txt") :to-equal "a+1")
        (expect (disk-text store "b.txt") :to-equal "b")
        (expect (status-of store tx) :to-be :not-found)
        (expect (length (read-journal store)) :to-be 1))))

  (it "keeps the tx intact when a commit is interrupted before its commit point"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (with-fault-at (:after-prepare) (commit-tx store tx))
        (expect (mapcar #'cdr (nth-value 1 (recover store))) :to-equal '("discarded"))
        (expect (disk-text store "a.txt") :to-equal "a")
        (expect (commit-tx store tx) :to-be :committed)
        (expect (disk-text store "a.txt") :to-equal "a+1")))))

(describe "aitools.store tx rebase and drop"
  (it "replays content-based ops onto the new base so the tx commits"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+tx")
        (put-file store "a.txt" "external")
        (expect (tx-rebase/k store tx #'replay-append
                             :on-rebased #'identity
                             :on-conflict (lambda (conflicts) (list :conflict conflicts))
                             :on-not-found (lambda () :not-found) :on-busy (lambda () :busy))
                :to-equal '("a.txt"))
        (expect (tx-view-text store tx "a.txt") :to-equal "external+tx")
        (expect (commit-tx store tx) :to-be :committed)
        (expect (disk-text store "a.txt") :to-equal "external+tx"))))

  (it "refuses to rebase a position-based op and leaves the tx unchanged"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+tx" :replayable nil)
        (put-file store "a.txt" "external")
        (let ((outcome (tx-rebase/k store tx #'replay-append
                                    :on-rebased (lambda (paths) (list :rebased paths))
                                    :on-conflict (lambda (conflicts) (list :conflict conflicts))
                                    :on-not-found (lambda () :not-found) :on-busy (lambda () :busy))))
          (expect (first outcome) :to-be :conflict)
          (expect (mapcar #'conflict-path (second outcome)) :to-equal '("a.txt")))
        (expect (tx-view-text store tx "a.txt") :to-equal "a+tx")
        (expect (tx-status-drift (status-of store tx)) :to-equal '("a.txt")))))

  (it "drops the named op and every later one, restoring the earlier state"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (stage-append store tx "a.txt" "+2")
        (stage store tx (list (write-file-request "b.txt" (bytes "b"))))
        (expect (tx-drop/k store tx 2 :on-dropped #'identity :on-not-found (lambda () :not-found)
                                      :on-busy (lambda () :busy))
                :to-equal '(2 3))
        (expect (tx-view-text store tx "a.txt") :to-equal "a+1")
        (expect (tx-view-text store tx "b.txt") :to-be :absent)
        (expect (mapcar #'tx-op-record-tx-op (tx-status-ops (status-of store tx))) :to-equal '(1))
        (expect (tx-drop/k store tx 5 :on-dropped #'identity :on-not-found (lambda () :not-found)
                                      :on-busy (lambda () :busy))
                :to-be :not-found)
        (expect (stage-append store tx "a.txt" "+3") :to-be 2)
        (expect (tx-view-text store tx "a.txt") :to-equal "a+1+3")))))

(describe "aitools.store tx abort and interruption"
  (it "removes the tx and its blobs without touching the working tree"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+unique-content")
        (expect (member (aitools.kernel.domain:content-hash (bytes "a+unique-content")) (blob-names store)
                        :test #'string=)
                :to-be-truthy)
        (expect (tx-abort/k store tx :on-aborted #'identity :on-not-found (lambda () :not-found)
                                     :on-busy (lambda () :busy))
                :to-equal '("a.txt"))
        (expect (status-of store tx) :to-be :not-found)
        (expect (tx-list store) :to-equal '())
        (expect (blob-names store) :to-equal '())
        (expect (disk-text store "a.txt") :to-equal "a"))))

  (it "stays at the previous op's state when a tx op is interrupted"
    (dolist (point '(:tx-after-blobs :tx-after-ops))
      (with-temp-store (store)
        (put-file store "a.txt" "a")
        (let ((tx (begin store)))
          (stage-append store tx "a.txt" "+1")
          (expect (with-fault-at (point) (stage-append store tx "a.txt" "+2")) :to-be-truthy)
          (expect (tx-view-text store tx "a.txt") :to-equal "a+1")
          (expect (mapcar #'tx-op-record-tx-op (tx-status-ops (status-of store tx))) :to-equal '(1))
          (expect (stage-append store tx "a.txt" "+3") :to-be 2)
          (expect (tx-view-text store tx "a.txt") :to-equal "a+1+3")))))

  (it "serialises two processes writing into one tx"
    (with-temp-store (store)
      (put-file store "log.txt" "")
      (let* ((tx (begin store))
             (pids (loop for n from 0 below 2
                         collect (let ((n n))
                                   (call-in-child-process
                                    (lambda ()
                                      (loop repeat 5
                                            do (unless (integerp (stage-append store tx "log.txt"
                                                                               (format nil "~D" n)))
                                                 (error "stage failed")))))))))
        (dolist (pid pids)
          (expect (multiple-value-list (wait-for-child pid)) :to-equal '(:exited 0)))
        (expect (mapcar #'tx-op-record-tx-op (tx-status-ops (status-of store tx)))
                :to-equal '(1 2 3 4 5 6 7 8 9 10))
        (expect (length (tx-view-text store tx "log.txt")) :to-be 10)))))

(defun replay-chmod (record view commit reject)
  "Re-run a recorded (\"chmod\" path mode) op against VIEW."
  (declare (ignore view reject))
  (destructuring-bind (command path mode) (tx-op-record-argv record)
    (declare (ignore command))
    (funcall commit (list (chmod-request path (parse-integer mode :radix 8))))))

(defun stage-chmod (store tx-id path mode)
  (tx-stage/k store tx-id (list "chmod" path (format nil "~O" mode))
              (lambda (view commit reject)
                (declare (ignore view reject))
                (funcall commit (list (chmod-request path mode))))
              :replayable t
              :on-staged (lambda (tx-op results) (list tx-op (actions results)))
              :on-rejected (lambda (code &rest rest) (declare (ignore rest)) code)
              :on-not-found (lambda () :not-found)
              :on-busy (lambda () :busy)))

(defun tx-view-state (store tx-id path)
  (call-with-tx-view/k store tx-id
                       :on-view (lambda (view)
                                  (let ((state (view-path-state view path)))
                                    (list (entry-state-kind state) (entry-state-hash state) (entry-state-mode state))))
                       :on-not-found (lambda () :not-found)))

(describe "aitools.store mode-only changes inside a tx"
  (it "stages a chmod keeping the content hash, and commits and undoes it as one op"
    (with-temp-store (store)
      (put-file store "cfg.txt" "secret" :mode #o644)
      (let ((tx (begin store))
            (hash (aitools.kernel.domain:content-hash (bytes "secret"))))
        (expect (stage-chmod store tx "cfg.txt" #o600) :to-equal '(1 (("cfg.txt" :mode-changed))))
        (expect (disk-mode store "cfg.txt") :to-be #o644)
        (expect (tx-view-state store tx "cfg.txt") :to-equal (list :file hash #o600))
        (expect (tx-view-text store tx "cfg.txt") :to-equal "secret")
        (multiple-value-bind (status op-id results) (commit-tx store tx)
          (expect status :to-be :committed)
          (expect (actions results) :to-equal '(("cfg.txt" :mode-changed)))
          (expect (disk-mode store "cfg.txt") :to-be #o600)
          (expect (disk-text store "cfg.txt") :to-equal "secret")
          (expect (status-of store tx) :to-be :not-found)
          (expect (undo store op-id) :to-be :committed)
          (expect (disk-mode store "cfg.txt") :to-be #o644)
          (expect (disk-text store "cfg.txt") :to-equal "secret")))))

  (it "stages a chmod on content the tx already rewrote"
    (with-temp-store (store)
      (put-file store "cfg.txt" "old" :mode #o644)
      (let ((tx (begin store)))
        (stage store tx (list (write-file-request "cfg.txt" (bytes "new"))))
        (expect (stage-chmod store tx "cfg.txt" #o600) :to-equal '(2 (("cfg.txt" :mode-changed))))
        (expect (tx-view-state store tx "cfg.txt")
                :to-equal (list :file (aitools.kernel.domain:content-hash (bytes "new")) #o600))
        (expect (commit-tx store tx) :to-be :committed)
        (expect (disk-text store "cfg.txt") :to-equal "new")
        (expect (disk-mode store "cfg.txt") :to-be #o600))))

  (it "drops a staged chmod back to the disk mode"
    (with-temp-store (store)
      (put-file store "cfg.txt" "secret" :mode #o644)
      (let ((tx (begin store)))
        (stage-chmod store tx "cfg.txt" #o600)
        (expect (tx-drop/k store tx 1 :on-dropped #'identity :on-not-found (lambda () :not-found)
                                      :on-busy (lambda () :busy))
                :to-equal '(1))
        (expect (tx-view-state store tx "cfg.txt")
                :to-equal (list :file (aitools.kernel.domain:content-hash (bytes "secret")) #o644))
        (expect (commit-tx store tx) :to-be :committed)
        (expect (disk-mode store "cfg.txt") :to-be #o644))))

  (it "replays a staged chmod onto drifted content at rebase"
    (with-temp-store (store)
      (put-file store "cfg.txt" "v1" :mode #o644)
      (let ((tx (begin store)))
        (stage-chmod store tx "cfg.txt" #o600)
        (put-file store "cfg.txt" "v2" :mode #o644)
        (expect (tx-rebase/k store tx #'replay-chmod
                             :on-rebased #'identity
                             :on-conflict (lambda (conflicts) (list :conflict conflicts))
                             :on-not-found (lambda () :not-found) :on-busy (lambda () :busy))
                :to-equal '("cfg.txt"))
        (expect (tx-view-state store tx "cfg.txt")
                :to-equal (list :file (aitools.kernel.domain:content-hash (bytes "v2")) #o600))
        (expect (commit-tx store tx) :to-be :committed)
        (expect (disk-text store "cfg.txt") :to-equal "v2")
        (expect (disk-mode store "cfg.txt") :to-be #o600)))))

(defun run-cli (root &rest arguments)
  "(values exit-code envelope) of `aitools --root ROOT ARGUMENTS...` through
the composition root; the envelope is parsed from stdout, else stderr."
  (multiple-value-bind (app registry) (aitools/cli:build-app)
    (let* ((out (make-string-output-stream))
           (err (make-string-output-stream))
           (code (aitools/cli:dispatch app registry (list* "aitools" "--root" root arguments)
                                       :stdout out :stderr err))
           (stdout (get-output-stream-string out)))
      (values code (json-kit:parse (if (plusp (length stdout)) stdout (get-output-stream-string err)))))))

(defun envelope-field (envelope &rest keys)
  (reduce (lambda (value key) (and value (gethash key value))) keys :initial-value envelope))

(defun call-with-cli-store (function)
  "Call FUNCTION with (ROOT STORE) for a fresh workspace, XDG_STATE_HOME
redirected to a scratch directory so the CLI opens its own store there."
  (with-temp-store (scratch)
    (let* ((root (store-root scratch))
           (xdg (concatenate 'string root "/../xdg"))
           (previous (sb-posix:getenv "XDG_STATE_HOME")))
      (sb-posix:mkdir xdg #o700)
      (unwind-protect
           (progn (sb-posix:setenv "XDG_STATE_HOME" xdg 1)
                  (funcall function root (aitools.store.infrastructure:make-posix-store root)))
        (if previous
            (sb-posix:setenv "XDG_STATE_HOME" previous 1)
            (sb-posix:unsetenv "XDG_STATE_HOME"))))))

(describe "aitools chmod --tx through dispatch"
  (it "stages the mode, leaves the disk alone until tx commit, then changes only the mode"
    (call-with-cli-store
     (lambda (root store)
       (put-file store "cfg.txt" "secret" :mode #o644)
       (let ((tx (envelope-field (nth-value 1 (run-cli root "tx" "begin")) "tx")))
         (multiple-value-bind (code envelope) (run-cli root "chmod" "--mode" "600" (disk-path store "cfg.txt") "--tx" tx)
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
                              (list :conflict (mapcar (lambda (c) (list (conflict-path c) (conflict-kind c))) conflicts)))
               :on-not-found (lambda () :not-found) :on-busy (lambda () :busy)))

(describe "aitools.store tx-stage/k outcomes"
  (it-each (("mkdir of an existing directory" :noop (:staged nil ()))
            ("a whole-directory move" :directory-move
             (:rejected "argument.invalid" "a directory move inside a tx must be staged per path"))
            ("a delete of a missing path" :missing (:rejected "input.not-found" "missing.txt does not exist"))
            ("a validation refusal" :refused (:rejected "refusal.custom" "no")))
      "answers ~A"
      (label case expected)
    (declare (ignore label))
    (with-temp-store (store)
      (put-file store "d/f.txt" "f")
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
        (expect (tx-status-ops (status-of store tx)) :to-equal '()))))

  (it "signals when VALIDATE calls neither continuation"
    (with-temp-store (store)
      (let ((tx (begin store)))
        (expect (handler-case (stage-outcome store tx (lambda (view commit reject)
                                                        (declare (ignore view commit reject))))
                  (error (condition) (princ-to-string condition)))
                :to-equal "tx-stage/k: VALIDATE returned without calling COMMIT or REJECT"))))

  (it "refuses to record a read of a path that is not workspace-relative"
    (with-temp-store (store)
      (let ((tx (begin store)))
        (expect (handler-case (tx-record-read/k store tx "../x" :on-recorded #'identity
                                                                :on-not-found (lambda ()) :on-busy (lambda ()))
                  (error (condition) (princ-to-string condition)))
                :to-equal "tx-record-read/k: \"../x\" is not a workspace-relative path")))))

(describe "aitools.store tx diff and commit of every staged kind"
  (it "names each change's action and commits them all"
    (with-temp-store (store)
      (put-file store "a.txt" "a" :mode #o644)
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
        (expect (sb-posix:stat-mtime (sb-posix:lstat (disk-path store "m.txt"))) :to-be 1577934245))))

  (it "lists only real transactions, skipping other names under the tx root"
    (with-temp-store (store)
      (let ((tx (begin store))
            (root (tx-root-directory (store-state-directory store))))
        (sb-posix:mkdir (join-path root "junk") #o700)
        (sb-posix:mkdir (join-path root (format-tx-id 3964000000 "00000000")) #o700)
        (expect (mapcar #'tx-status-id (tx-list store)) :to-equal (list tx))))))

(describe "aitools.store tx rebase and drop edge cases"
  (it "restages an op on an undrifted path and replays the drifted one"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
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
        (expect (list (disk-text store "a.txt") (disk-text store "b.txt")) :to-equal '("a+1" "external+2")))))

  (it "reports a conflict when the replay rejects, leaving the tx as it was"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
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
        (expect (tx-view-text store tx "a.txt") :to-equal "a+1"))))

  (it "signals when REPLAY calls neither continuation"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (put-file store "a.txt" "external")
        (expect (handler-case (rebase-outcome store tx (lambda (record view commit reject)
                                                         (declare (ignore record view commit reject))))
                  (error (condition) (princ-to-string condition)))
                :to-equal "tx-rebase/k: REPLAY returned without calling COMMIT or REJECT"))))

  (it-each ((0) ("1") (2))
      "answers not-found for tx drop of op ~S"
      (tx-op)
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (expect (tx-drop/k store tx tx-op :on-dropped #'identity :on-not-found (lambda () :not-found)
                                          :on-busy (lambda () :busy))
                :to-be :not-found)
        (expect (tx-view-text store tx "a.txt") :to-equal "a+1")))))
