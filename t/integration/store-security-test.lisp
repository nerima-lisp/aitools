;;;;
;;;; Tests for forged records, path pinning, hardlinks, and state file modes.
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
        (let ((aitools.store.application::*fault-hook* (lambda (point &rest details)
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

