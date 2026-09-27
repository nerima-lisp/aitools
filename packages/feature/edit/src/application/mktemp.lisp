;;;; packages/feature/edit/src/application/mktemp.lisp
;;;;
;;;; `mktemp`: a file or directory in the workspace's tmp/ area, which
;;;; alone of the file operations bypasses the journal.
(in-package #:aitools.edit.application)

;;; ------------------------------------------------------------------ mktemp

(defparameter +mktemp-max-age-seconds+ (* 7 24 60 60)
  "Entries of tmp/ older than this are removed when mktemp runs.")

(defun %io (store accessor &rest arguments)
  (apply (funcall accessor (aitools.store.application:store-io-port store)) arguments))

(defun %ensure-directory (store path)
  (unless (eq (%io store #'aitools.store.application:store-io-lstat path) :directory)
    (let ((parent (aitools.workspace.domain:path-parent path)))
      (when parent (%ensure-directory store parent)))
    (handler-case (%io store #'aitools.store.application:store-io-mkdir path)
      (aitools.store.application:store-io-error (condition)
        (unless (eq (%io store #'aitools.store.application:store-io-lstat path) :directory)
          (error condition))))))

(defun %remove-tree (store path)
  (handler-case
      (case (%io store #'aitools.store.application:store-io-lstat path)
        (:directory
         (dolist (name (%io store #'aitools.store.application:store-io-list-directory path))
           (%remove-tree store (aitools.workspace.domain:join-path path name)))
         (%io store #'aitools.store.application:store-io-rmdir path))
        (:absent)
        (t (%io store #'aitools.store.application:store-io-unlink path)))
    (aitools.store.application:store-io-error () nil)))

(defun mktemp-flow (ports options &key root on-ok on-error)
  "Create a file (or with --dir a directory) in the workspace's
tmp/ area, after removing entries there older than 7 days. Not
journaled; the returned absolute path is a valid write target inside the workspace boundary."
  (declare (type function on-ok on-error))
  (let ((suffix (or (getf options :suffix) ""))
        (host (edit-ports-workspace-host ports)))
    (if (or (find #\/ suffix) (find (code-char 0) suffix))
        (funcall on-error "argument.invalid" "--suffix must not contain / or NUL")
        (aitools.workspace.application:call-with-resolved-root/k
         host :root root
         :on-error (lambda (reason path)
                     (funcall on-error (if (eq reason :not-found) "input.not-found" "argument.invalid")
                              (format nil "workspace root ~A is not a usable directory" path)))
         :on-resolved
         (lambda (workspace-root)
           (let* ((store (funcall (edit-ports-open-store ports)
                                  (aitools.workspace.application:workspace-root-real workspace-root)))
                  (tmp (aitools.store.domain:tmp-directory (aitools.store.application:store-state-directory store)))
                  (now (funcall (edit-ports-unix-now ports))))
             (handler-case
                 (progn
                   ;; The state home may sit behind a symlink ($XDG_STATE_HOME=/tmp/x,
                   ;; /tmp -> /private/tmp): create and answer the real path, the
                   ;; form the write boundary compares a mktemp path with.
                   (setf tmp (or (aitools.workspace.application:resolve-real-path host tmp) tmp))
                   (%ensure-directory store tmp)
                   (dolist (name (%io store #'aitools.store.application:store-io-list-directory tmp))
                     (let* ((path (aitools.workspace.domain:join-path tmp name))
                            (entry (aitools.workspace.application:host-stat host path)))
                       (when (and entry (< (aitools.workspace.application:workspace-entry-mtime entry)
                                           (- now +mktemp-max-age-seconds+)))
                         (%remove-tree store path))))
                   (loop
                     (let ((path (aitools.workspace.domain:join-path
                                  tmp (format nil "tmp.~A~A"
                                              (%io store #'aitools.store.application:store-io-random-hex 12) suffix))))
                       (when (eq (%io store #'aitools.store.application:store-io-lstat path) :absent)
                         (if (getf options :dir)
                             (progn (%io store #'aitools.store.application:store-io-mkdir path)
                                    (return (funcall on-ok (list (cons "path" path)))))
                             (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
                               (%io store #'aitools.store.application:store-io-create-file path empty)
                               ;; `info`'s hash of the new file, so `write --overwrite
                               ;; --expect-hash` needs no extra call.
                               (return (funcall on-ok (list (cons "path" path)
                                                            (cons "hash" (aitools.kernel.domain:content-hash empty)))))))))))
               (aitools.store.application:store-io-error (condition)
                 (funcall on-error "environment.io" (princ-to-string condition))))))))))
