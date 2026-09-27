;;;; t/integration/store-security-test.lisp
;;;;
;;;; The store's own defences against a workspace an agent can write into:
;;;; forged intent records, parent directories swapped for symlinks
;;;; mid-write, hardlinked targets of in-place steps, state file modes, and
;;;; the failure paths after the write protocol's commit point.
(in-package #:aitools.store.test)

(defun base-directory (store)
  "The scratch directory WITH-TEMP-STORE made, holding `work` (the root)."
  (let ((root (store-root store)))
    (subseq root 0 (position #\/ root :from-end t))))

(defun state-home-of (store)
  (let ((state (store-state-directory store)))
    (subseq state 0 (position #\/ state :from-end t))))

(defun file-mode (path)
  (logand (sb-posix:stat-mode (sb-posix:lstat path)) #o7777))

(defun exists-p (path)
  (handler-case (progn (sb-posix:lstat path) t)
    (sb-posix:syscall-error () nil)))

(defun write-raw (path text)
  (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :if-exists :supersede
                                                           :element-type '(unsigned-byte 8))
    (write-sequence (bytes text) out)))

(defun forge-intent (store steps)
  "Write a complete, checksummed intent record for STEPS into `commit/`, as
an agent able to write the state directory could. Returns its op id."
  (let* ((op-id (format-op-id (get-universal-time) "0badc0de"))
         (intent (make-intent :op-id op-id :steps steps
                              :journal-entry (make-journal-entry :op-id op-id :argv '("forged")
                                                                 :time "2026-01-01T00:00:00Z"
                                                                 :changes '())))
         (header (encode-intent-header intent)))
    (write-raw (intent-file-path (store-state-directory store) op-id)
               (format nil "~A~%~A~%" header (encode-intent-checksum header)))
    op-id))

(defun seed-state (store)
  "Create the state directories through an ordinary commit."
  (expect (commit store (list (write-file-request "seed.txt" (bytes "s")))) :to-be :committed))

(defun posix-store-under (root state-home)
  (aitools.store.infrastructure:make-posix-store
   root :environment (cl-boundary-kit:make-environment
                      :get-fn (lambda (name) (and (string= name "XDG_STATE_HOME") state-home)))))

(describe "aitools.store recovery of forged intent records"
  (it "discards a complete record whose step writes into .git and leaves the workspace alone"
    (with-temp-store (store)
      (seed-state store)
      (sb-posix:mkdir (disk-path store ".git") #o755)
      (sb-posix:mkdir (disk-path store ".git/hooks") #o755)
      (put-file store ".aitools-evil-1.tmp" "#!/bin/sh")
      (let ((op-id (forge-intent store (list (make-intent-step :op :replace :path ".git/hooks/post-commit"
                                                               :temp ".aitools-evil-1.tmp" :kind :file :mode #o755)))))
        (multiple-value-bind (status entries) (recover store)
          (expect status :to-be :recovered)
          (expect entries :to-equal (list (cons op-id "discarded")))))
      (expect (exists-p (disk-path store ".git/hooks/post-commit")) :to-be nil)
      (expect (disk-text store ".aitools-evil-1.tmp") :to-equal "#!/bin/sh")
      (expect (intent-files store) :to-equal '())))

  (it "discards a record whose step case-folds to .git, climbs a symlink out, or targets the state directory"
    (with-temp-store (store)
      (let* ((base (base-directory store))
             (inside (make-store (store-io-port store) (store-root store) (disk-path store "state"))))
        (seed-state inside)
        (sb-posix:mkdir (concatenate 'string base "/outside") #o755)
        (sb-posix:symlink "../outside" (disk-path store "link"))
        (dolist (path (list ".GIT/config" "link/pwn.txt" "state/marker.txt"
                            (format nil "state/~A/commit/x.json"
                                    (subseq (store-state-directory inside)
                                            (1+ (position #\/ (store-state-directory inside) :from-end t))))))
          (put-file store ".aitools-payload-1.tmp" "x")
          (let ((op-id (forge-intent inside (list (make-intent-step :op :replace :path path
                                                                    :temp ".aitools-payload-1.tmp" :kind :file
                                                                    :mode #o644)))))
            (multiple-value-bind (status entries) (recover inside)
              (expect status :to-be :recovered)
              (expect entries :to-equal (list (cons op-id "discarded"))))))
        (expect (exists-p (concatenate 'string base "/outside/pwn.txt")) :to-be nil)
        (expect (exists-p (disk-path store "state/marker.txt")) :to-be nil)
        (expect (exists-p (disk-path store ".GIT")) :to-be nil)
        (expect (intent-files inside) :to-equal '()))))

  (it "refuses a write request into .git or the state directory before anything is prepared"
    (with-temp-store (store)
      (let ((inside (make-store (store-io-port store) (store-root store) (disk-path store "state"))))
        (seed-state inside)
        (expect (nth-value 1 (commit inside (list (write-file-request ".git/config" (bytes "x")))))
                :to-equal "refusal.outside-workspace")
        (expect (nth-value 1 (commit inside (list (write-file-request "state/x" (bytes "x")))))
                :to-equal "refusal.outside-workspace")
        (expect (exists-p (disk-path store ".git")) :to-be nil)
        (expect (exists-p (disk-path store "state/x")) :to-be nil)))))

(describe "aitools.store parent pinning against TOCTOU"
  (it "refuses a write whose parent directory is swapped for a symlink after validation"
    (with-temp-store (plain)
      (let* ((base (base-directory plain))
             (outside (concatenate 'string base "/outside"))
             (store (posix-store-under (store-root plain) (concatenate 'string base "/state"))))
        (put-file plain "sub/keep.txt" "k")
        (sb-posix:mkdir outside #o755)
        (let ((*fault-hook* (lambda (point &rest details)
                              (declare (ignore details))
                              (when (eq point :after-intent-header)
                                (sb-posix:rename (disk-path plain "sub") (disk-path plain "sub-old"))
                                (sb-posix:symlink "../outside" (disk-path plain "sub"))))))
          (multiple-value-bind (status code) (commit store (list (write-file-request "sub/a.txt" (bytes "pwn"))))
            (expect status :to-be :rejected)
            (expect code :to-equal "environment.io")))
        (expect (funcall (store-io-list-directory (store-io-port plain)) outside) :to-equal '())
        (expect (disk-text plain "sub-old/keep.txt") :to-equal "k")
        (expect (intent-files store) :to-equal '()))))

  (it "applies every step kind through pinned parents"
    (with-temp-store (plain)
      (let ((store (posix-store-under (store-root plain) (concatenate 'string (base-directory plain) "/state"))))
        (put-file plain "edit.txt" "old")
        (put-file plain "gone.txt" "bye")
        (put-file plain "mv.txt" "moving")
        (put-file plain "exec.sh" "#!/bin/sh")
        (put-file plain "old.txt" "t")
        (sb-posix:mkdir (disk-path plain "empty") #o755)
        (expect (commit store (list (write-file-request "new/deep.txt" (bytes "fresh") :mode #o600)
                                    (write-file-request "edit.txt" (bytes "new"))
                                    (delete-request "gone.txt")
                                    (delete-request "empty")
                                    (move-request "mv.txt" "moved.txt")
                                    (chmod-request "exec.sh" #o755)
                                    (mtime-request "old.txt" 1000000000)
                                    (symlink-request "link" "edit.txt")))
                :to-be :committed)
        (expect (disk-text plain "new/deep.txt") :to-equal "fresh")
        (expect (disk-mode plain "new/deep.txt") :to-be #o600)
        (expect (disk-text plain "edit.txt") :to-equal "new")
        (expect (disk-text plain "gone.txt") :to-be :absent)
        (expect (disk-text plain "empty") :to-be :absent)
        (expect (disk-text plain "moved.txt") :to-equal "moving")
        (expect (disk-mode plain "exec.sh") :to-be #o755)
        (expect (workspace-mtime plain "old.txt") :to-be 1000000000)
        (expect (entry-state-target (workspace-state plain "link")) :to-equal "edit.txt")
        (expect (temp-files plain) :to-equal '())))))

(describe "aitools.store in-place steps on hardlinked files"
  (it "refuses chmod and touch of a file with another hard link"
    (with-temp-store (store)
      (let ((outside (concatenate 'string (base-directory store) "/outside.txt")))
        (write-raw outside "secret")
        (sb-posix:chmod outside #o644)
        (sb-posix:link outside (disk-path store "hl.txt"))
        (multiple-value-bind (status code) (commit store (list (chmod-request "hl.txt" #o4777)))
          (expect status :to-be :rejected)
          (expect code :to-equal "refusal.not-a-file"))
        (multiple-value-bind (status code) (commit store (list (mtime-request "hl.txt" 1000000000)))
          (expect status :to-be :rejected)
          (expect code :to-equal "refusal.not-a-file"))
        (expect (file-mode outside) :to-be #o644)
        (expect (read-journal store) :to-equal '()))))

  (it "has the production adapter refuse them too, and never follow a symlink"
    (with-temp-store (store)
      (let ((outside (concatenate 'string (base-directory store) "/outside.txt"))
            (io (make-posix-store-io :root (store-root store))))
        (write-raw outside "secret")
        (sb-posix:chmod outside #o644)
        (sb-posix:link outside (disk-path store "hl.txt"))
        (sb-posix:symlink outside (disk-path store "sl.txt"))
        (dolist (path (list (disk-path store "hl.txt") (disk-path store "sl.txt")))
          (signals store-io-error (funcall (store-io-chmod io) path #o4777))
          (signals store-io-error (funcall (store-io-set-mtime io) path 1000000000)))
        (expect (file-mode outside) :to-be #o644)))))

(describe "aitools.store state file modes"
  (it "creates the state directories 0700 and state files 0600 whatever the umask"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((old (sb-posix:umask 0)))
        (unwind-protect
             (expect (commit store (list (write-file-request "a.txt" (bytes "b")))) :to-be :committed)
          (sb-posix:umask old)))
      (let ((state (store-state-directory store)))
        (expect (file-mode (state-home-of store)) :to-be #o700)
        (expect (file-mode state) :to-be #o700)
        (expect (file-mode (commit-directory state)) :to-be #o700)
        (expect (file-mode (blobs-directory state)) :to-be #o700)
        (expect (file-mode (lock-file-path state)) :to-be #o600)
        (expect (file-mode (journal-file-path state)) :to-be #o600)
        (let ((blobs (funcall (store-io-list-directory (store-io-port store)) (blobs-directory state))))
          (expect (length blobs) :to-be 1)
          (expect (file-mode (join-path (blobs-directory state) (first blobs))) :to-be #o600)))
      (expect (disk-mode store "a.txt") :to-be #o644)))

  (it "tightens an existing looser state directory it owns"
    (with-temp-store (store)
      (seed-state store)
      (sb-posix:chmod (store-state-directory store) #o755)
      (sb-posix:chmod (state-home-of store) #o755)
      (seed-state store)
      (expect (file-mode (store-state-directory store)) :to-be #o700)
      (expect (file-mode (state-home-of store)) :to-be #o700))))

(describe "aitools.store failures after the commit point"
  (flet ((failing-apply-store (store)
           ;; Every rename onto a workspace path fails: the first one is the
           ;; apply step after the commit point.
           (let* ((io (store-io-port store))
                  (rename (store-io-rename io))
                  (root (concatenate 'string (store-root store) "/")))
             (make-store (copy-store-io io :rename (lambda (from to)
                                                     (if (eql 0 (search root to))
                                                         (error 'store-io-error :operation "rename" :path to
                                                                                :detail "injected")
                                                         (funcall rename from to))))
                         (store-root store) (state-home-of store)))))
    (it "signals store-committed-error naming the op and path, and keeps the intent for recovery"
      (with-temp-store (store)
        (seed-state store)
        (let* ((broken (failing-apply-store store))
               (condition (handler-case (progn (commit broken (list (write-file-request "a.txt" (bytes "a")))) nil)
                            (store-committed-error (condition) condition))))
          (expect condition :to-be-truthy)
          (expect (store-io-error-path condition) :to-equal (disk-path store "a.txt"))
          (expect (store-io-error-operation condition) :to-equal "rename")
          (expect (intent-files store)
                  :to-equal (list (concatenate 'string (store-committed-error-op-id condition) ".json")))
          (expect (recover store) :to-be :recovered)
          (expect (disk-text store "a.txt") :to-equal "a"))))

    (it "reports a record recovery cannot complete through ON-FAILED instead of wedging"
      (with-temp-store (store)
        (seed-state store)
        (let ((broken (failing-apply-store store)))
          (handler-case (commit broken (list (write-file-request "a.txt" (bytes "a"))))
            (store-committed-error () nil))
          (let ((reported (recover/k broken
                                     :on-rolled-forward (lambda (op-id) (declare (ignore op-id)))
                                     :on-discarded (lambda (op-id) (declare (ignore op-id)))
                                     :on-none (lambda () :none)
                                     :on-busy (lambda () :busy)
                                     :on-failed (lambda (condition entries)
                                                  (list :failed (store-committed-error-op-id condition)
                                                        (store-io-error-path condition) entries)))))
            (expect (first reported) :to-be :failed)
            (expect (third reported) :to-equal (disk-path store "a.txt"))
            (expect (fourth reported) :to-equal '()))
          (signals store-committed-error
            (recover/k broken
                       :on-rolled-forward (lambda (op-id) (declare (ignore op-id)))
                       :on-discarded (lambda (op-id) (declare (ignore op-id)))
                       :on-none (lambda () :none)
                       :on-busy (lambda () :busy)))
          (expect (length (intent-files store)) :to-be 1))
        (expect (recover store) :to-be :recovered)
        (expect (disk-text store "a.txt") :to-equal "a")))))

(describe "aitools.store stream errors"
  (it "reports a failed write as store-io-error, not a stream error"
    (with-temp-store (store)
      (put-file store "ro.txt" "r")
      (let* ((path (disk-path store "ro.txt"))
             (fd (sb-posix:open path sb-posix:o-rdonly)))
        (signals store-io-error
          (aitools.store.infrastructure::%write-and-close path fd (bytes "data") nil))))))

(describe "aitools.store tx blobs and rebase atomicity"
  (it "keeps a blob only an earlier tx op references when another write collects garbage"
    (with-temp-store (store)
      (let ((tx (begin store)))
        (expect (stage store tx (list (write-file-request "a.txt" (bytes "X")))) :to-be 1)
        (expect (stage store tx (list (write-file-request "a.txt" (bytes "Y")))) :to-be 2)
        (expect (commit store (list (write-file-request "other.txt" (bytes "o")))) :to-be :committed)
        (expect (tx-drop/k store tx 2 :on-dropped #'identity :on-not-found (lambda () :not-found)
                                      :on-busy (lambda () :busy))
                :to-equal '(2))
        (expect (tx-view-text store tx "a.txt") :to-equal "X"))))

  (it "leaves ops and index on one timeline when a rebase is interrupted between its writes"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+tx")
        (put-file store "a.txt" "external")
        (expect (with-fault-at (:tx-after-ops)
                  (tx-rebase/k store tx #'replay-append
                               :on-rebased #'identity :on-conflict #'identity
                               :on-not-found (lambda () :not-found) :on-busy (lambda () :busy)))
                :to-be-truthy)
        (let* ((status (status-of store tx))
               (after (cdr (assoc "a.txt" (tx-op-record-after (first (tx-status-ops status))) :test #'string=)))
               (staged (tx-path-staged (find "a.txt" (tx-status-paths status) :key #'tx-path-path :test #'string=))))
          (expect (entry-state-hash after) :to-equal (entry-state-hash staged)))
        (expect (tx-rebase/k store tx #'replay-append
                             :on-rebased #'identity :on-conflict #'identity
                             :on-not-found (lambda () :not-found) :on-busy (lambda () :busy))
                :to-equal '("a.txt"))
        (expect (tx-view-text store tx "a.txt") :to-equal "external+tx")))))

(describe "aitools.store writes into the mktemp area"
  (it "neither journals them nor creates a second state directory"
    (with-temp-store (plain)
      (let* ((state-home (concatenate 'string (base-directory plain) "/state"))
             (workspace (posix-store-under (store-root plain) state-home))
             (tmp (tmp-directory (store-state-directory workspace))))
        (seed-state workspace)
        (sb-posix:mkdir tmp #o700)
        (let ((temporary (posix-store-under tmp state-home)))
          (expect (commit temporary (list (write-file-request "scratch.txt" (bytes "one")))) :to-be :committed)
          (expect (commit temporary (list (write-file-request "scratch.txt" (bytes "two")))) :to-be :committed))
        (expect (octets-string (funcall (store-io-read-file (store-io-port plain)) (join-path tmp "scratch.txt")))
                :to-equal "two")
        (expect (funcall (store-io-list-directory (store-io-port plain)) (concatenate 'string state-home "/aitools"))
                :to-equal (list (subseq (store-state-directory workspace)
                                        (1+ (position #\/ (store-state-directory workspace) :from-end t)))))
        (expect (length (read-journal workspace)) :to-be 1)))))

(describe "aitools.store rename failures name the blocking destination"
  (it "reports the destination of a failed posix rename, not its source"
    (with-temp-store (store)
      (put-file store "src.txt" "s")
      (put-file store "blocker/inside.txt" "i")
      (let ((condition (handler-case (progn (funcall (store-io-rename (store-io-port store))
                                                     (disk-path store "src.txt") (disk-path store "blocker"))
                                            nil)
                         (store-io-error (condition) condition))))
        (expect (store-io-error-operation condition) :to-equal "rename")
        (expect (store-io-error-path condition) :to-equal (disk-path store "blocker"))
        (expect (search (disk-path store "src.txt") (store-io-error-detail condition)) :to-be-truthy))
      (expect (disk-text store "src.txt") :to-equal "s")))

  (it "points recovery's failed journal rewrite at journal/ops.jsonl, not the vanished temp"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((journal (journal-file-path (store-state-directory store))))
        (expect (with-fault-at (:after-apply) (commit store (list (write-file-request "a.txt" (bytes "b")))))
                :to-be-truthy)
        (when (exists-p journal)
          (sb-posix:unlink journal))
        (ensure-directories-exist (sb-ext:parse-native-namestring (concatenate 'string journal "/keep/")))
        (let ((reported (recover/k store
                                   :on-rolled-forward (lambda (op-id) (declare (ignore op-id)))
                                   :on-discarded (lambda (op-id) (declare (ignore op-id)))
                                   :on-none (lambda () :none)
                                   :on-busy (lambda () :busy)
                                   :on-failed (lambda (condition entries)
                                                (list (store-io-error-operation condition)
                                                      (store-io-error-path condition)
                                                      entries)))))
          (expect reported :to-equal (list "rename" journal '())))
        (expect (length (intent-files store)) :to-be 1)))))

(defun blob-listing (store)
  (sort (copy-list (funcall (store-io-list-directory (store-io-port store))
                            (blobs-directory (store-state-directory store))))
        #'string<))

(defun plant-blob-files (store &rest names)
  (dolist (name names)
    (with-open-file (out (sb-ext:parse-native-namestring
                          (join-path (blobs-directory (store-state-directory store)) name))
                         :direction :output :if-exists :supersede)
      (write-string "x" out))))

(defun referenced-store (store)
  "Commit a rewrite of a.txt so the journal references one blob, the
content before it; returns that blob's name."
  (put-file store "a.txt" "before")
  (commit store (list (write-file-request "a.txt" (bytes "after"))))
  (aitools.kernel.domain:content-hash (bytes "before")))

(defparameter *stray-hash* "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")

(describe "aitools.store garbage collection"
  (it "deletes stray temps and unreferenced blobs, keeping referenced blobs and foreign names"
    (with-temp-store (store)
      (let ((kept (referenced-store store))
            (tx-root (tx-root-directory (store-state-directory store))))
        (plant-blob-files store ".stale.tmp" "notes" *stray-hash*)
        (sb-posix:mkdir (join-path tx-root ".tx-x.deleting") #o700)
        (sb-posix:mkdir (join-path tx-root "junk") #o700)
        (collect-garbage store)
        (expect (blob-listing store) :to-equal (sort (list kept "notes") #'string<))
        (expect (funcall (store-io-list-directory (store-io-port store)) tx-root) :to-equal '("junk")))))

  (it-each (("an unreadable tx index" :tx-index)
            ("an unreadable journal line" :journal))
      "collects nothing when ~A leaves the references unknown"
      (label case)
    (declare (ignore label))
    (with-temp-store (store)
      (referenced-store store)
      (plant-blob-files store *stray-hash*)
      (ecase case
        (:tx-index
         (let ((tx (join-path (tx-root-directory (store-state-directory store)) (format-tx-id 3964000000 "00000000"))))
           (sb-posix:mkdir tx #o700)
           (with-open-file (out (sb-ext:parse-native-namestring (join-path tx "index.json")) :direction :output)
             (write-string "{" out))))
        (:journal
         (with-open-file (out (sb-ext:parse-native-namestring (journal-file-path (store-state-directory store)))
                              :direction :output :if-exists :append)
           (format out "{\"op_id\":\"x\"}~%"))))
      (collect-garbage store)
      (expect (member *stray-hash* (blob-listing store) :test #'string=) :to-be-truthy)
      (when (eq case :journal)
        ;; A write still succeeds; its own collection also keeps the blob.
        (expect (commit store (list (write-file-request "b.txt" (bytes "b")))) :to-be :committed)
        (expect (member *stray-hash* (blob-listing store) :test #'string=) :to-be-truthy))))

  (it "compacts the journal once it holds twice the retained entries"
    (with-temp-store (store)
      (flet ((journal-lines ()
               (count #\Newline (octets-string (funcall (store-io-read-file (store-io-port store))
                                                        (journal-file-path (store-state-directory store)))))))
        (loop for n from 1 to 40
              do (commit store (list (write-file-request "a.txt" (bytes (format nil "~D" n))))))
        (expect (journal-lines) :to-be 40)
        (commit store (list (write-file-request "a.txt" (bytes "41"))))
        (expect (journal-lines) :to-be 20)
        (expect (length (read-journal store)) :to-be 20))))

  (it "tolerates a removal that fails because the entry already vanished, and reports any other"
    (with-temp-store (store)
      (referenced-store store)
      (flet ((store-with-unlink (unlink)
               (make-store (copy-store-io (store-io-port store) :unlink unlink)
                           (store-root store) (state-home-of store))))
        (plant-blob-files store ".vanishing.tmp")
        (collect-garbage (store-with-unlink (lambda (path)
                                              (sb-posix:unlink path)
                                              (error 'store-io-error :operation "unlink" :path path
                                                                     :detail "raced"))))
        (expect (member ".vanishing.tmp" (blob-listing store) :test #'string=) :to-be nil)
        (plant-blob-files store ".stuck.tmp")
        (let ((condition (handler-case
                             (progn (collect-garbage (store-with-unlink
                                                      (lambda (path)
                                                        (error 'store-io-error :operation "unlink" :path path
                                                                               :detail "denied"))))
                                    nil)
                           (store-io-error (condition) condition))))
          (expect (store-io-error-detail condition) :to-equal "denied")
          (expect (store-io-error-path condition)
                  :to-equal (join-path (blobs-directory (store-state-directory store)) ".stuck.tmp")))))))

(describe "aitools.store commit-changes/k continuations"
  (it "reports a dry-run planner refusal without writing"
    (with-temp-store (store)
      (expect (multiple-value-list (commit store (list (delete-request "none.txt")) :dry-run t))
              :to-equal '(:rejected "input.not-found" "none.txt does not exist" ()))))

  (it "signals when VALIDATE calls neither continuation"
    (with-temp-store (store)
      (expect (handler-case (commit-changes/k store '("x") (lambda (commit reject) (declare (ignore commit reject)))
                                              :on-committed #'list :on-rejected #'list :on-busy #'list)
                (error (condition) (princ-to-string condition)))
              :to-equal "commit-changes/k: VALIDATE returned without calling COMMIT or REJECT")))

  (it "prints a store refusal as its message"
    (expect (princ-to-string (make-condition 'aitools.store.application::store-refusal
                                             :code "refusal.not-a-file" :message "a.txt has 2 hard links"))
            :to-equal "a.txt has 2 hard links"))

  (it "creates the state directories only under a directory, reporting the file in the way"
    (with-temp-store (plain)
      (let* ((blocker (concatenate 'string (base-directory plain) "/blocker"))
             (store (make-store (make-posix-store-io) (store-root plain) (concatenate 'string blocker "/state"))))
        (with-open-file (out (sb-ext:parse-native-namestring blocker) :direction :output) (write-string "f" out))
        (let ((condition (handler-case (progn (commit store (list (write-file-request "a.txt" (bytes "a")))) nil)
                           (store-io-error (condition) condition))))
          (expect (list (store-io-error-operation condition) (store-io-error-path condition))
                  :to-equal (list "mkdir" blocker)))
        (expect (disk-text plain "a.txt") :to-be :absent)))))

(defun io-failure (thunk)
  "(operation detail) of the STORE-IO-ERROR THUNK signals, or :OK."
  (handler-case (progn (funcall thunk) :ok)
    (store-io-error (condition) (list (store-io-error-operation condition) (store-io-error-detail condition)))))

(describe "aitools.store posix store-io primitives"
  (it "classifies a FIFO as :other and reports an unreadable parent instead of calling it absent"
    (with-temp-store (store)
      (let ((io (store-io-port store))
            (locked (disk-path store "locked")))
        (sb-posix:mkfifo (disk-path store "pipe") #o600)
        (expect (funcall (store-io-lstat io) (disk-path store "pipe")) :to-be :other)
        (sb-posix:mkdir locked #o700)
        (sb-posix:chmod locked #o000)
        (unwind-protect
             (expect (io-failure (lambda () (funcall (store-io-lstat io) (concatenate 'string locked "/x"))))
                     :to-equal '("lstat" "Permission denied"))
          (sb-posix:chmod locked #o700)))))

  (it-each ((65535) (65536) (65537) (131072))
      "reads a ~D-octet file whole"
      (size)
    (with-temp-store (store)
      (let ((path (disk-path store "data.bin")))
        (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :element-type '(unsigned-byte 8))
          (write-sequence (make-array size :element-type '(unsigned-byte 8) :initial-element 9) out))
        (let ((octets (funcall (store-io-read-file (store-io-port store)) path)))
          (expect (length octets) :to-be size)
          (expect (count 9 octets) :to-be size)))))

  (it "changes the mode and time of a file its owner cannot open, by name"
    (with-temp-store (store)
      (let ((io (store-io-port store))
            (path (disk-path store "sealed.txt")))
        (put-file store "sealed.txt" "s" :mode #o000)
        (funcall (store-io-set-mtime io) path 1577934245)
        (funcall (store-io-chmod io) path #o640)
        (expect (disk-mode store "sealed.txt") :to-be #o640)
        (expect (sb-posix:stat-mtime (sb-posix:lstat path)) :to-be 1577934245))))

  (it "refuses a time change on a directory and a mode change of a missing path"
    (with-temp-store (store)
      (let ((io (store-io-port store)))
        (sb-posix:mkdir (disk-path store "d") #o755)
        (expect (io-failure (lambda () (funcall (store-io-set-mtime io) (disk-path store "d") 1)))
                :to-equal '("utimes" "not a regular file"))
        (expect (io-failure (lambda () (funcall (store-io-chmod io) (disk-path store "none") #o600)))
                :to-equal '("open" "No such file or directory")))))

  (it "walks pinned parents below a root given with a trailing slash"
    (with-temp-store (store)
      (let ((io (make-posix-store-io :root (concatenate 'string (store-root store) "/"))))
        (funcall (store-io-mkdir io) (disk-path store "a"))
        (funcall (store-io-mkdir io) (disk-path store "a/b") :mode #o700)
        (funcall (store-io-create-file io) (disk-path store "a/b/c.txt") (bytes "deep"))
        (expect (disk-text store "a/b/c.txt") :to-equal "deep")
        (expect (disk-mode store "a/b/c.txt") :to-be #o600)
        (expect (disk-mode store "a/b") :to-be #o700)
        (sb-posix:symlink "a" (disk-path store "via"))
        ;; Darwin checks O_DIRECTORY first (ENOTDIR), Linux O_NOFOLLOW (ELOOP).
        (expect (member (io-failure (lambda () (funcall (store-io-create-file io) (disk-path store "via/b/x") (bytes "x"))))
                        '(("open" "Not a directory") ("open" "Too many levels of symbolic links"))
                        :test #'equal)
                :to-be-truthy)
        (expect (disk-text store "a/b/x") :to-be :absent)))))
