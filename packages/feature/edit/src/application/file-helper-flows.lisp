(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
  (%not-found context path &key on-error)
  (aitools.kernel.domain:flow-continue
    :on-error
    "input.not-found"
    (format nil "~A does not exist" path)
    :candidates
    (path-candidates (write-context-view context) path)))
(defun %link-message (kind value)
  (format nil
          (ecase kind
            (:outside "the link would point outside the workspace: ~A")
            (:symlink
             "~A is already a symlink; pass --overwrite with --expect-hash to replace it")
            (:not-symlink
             "~A exists and is not a symlink; link --overwrite replaces only symlinks"))
          value))

(aitools.kernel.domain:define-flow
  (%destination-check/k context destination source-kind overwrite &key on-rejected on-ok)
  "The destination rules for move/copy: absent is fine; an existing file
or symlink may be replaced with --overwrite and --expect-hash when the
source is not a directory; anything else is refusal.exists."
  (let ((kind (%kind context destination)))
    (cond
      ((eq kind :absent)
       (aitools.kernel.domain:flow-continue
         :on-ok))
      ((and overwrite (member kind '(:file :symlink)) (not (eq source-kind :directory)))
       (aitools.kernel.domain:flow-call
         (require-expect-hash/k context destination)
         (:on-rejected (code message &key candidates diagnostics conflicts repairs path)
                       (declare (ignore path))
                       (flow-continue :on-rejected
                                      code
                                      message
                                      :candidates
                                      candidates
                                      :diagnostics
                                      diagnostics
                                      :conflicts
                                      conflicts
                                      :repairs
                                      repairs))
         (:on-ok ()
                 (aitools.kernel.domain:flow-continue
                   :on-ok))))
      (t
       (aitools.kernel.domain:flow-continue
         :on-rejected
         "refusal.exists"
         (format nil
                 (concatenate 'string
                              "~A already exists~:[~; (directories are never merged)~]~:[~; "
                              "(pass --overwrite with --expect-hash to replace a file)~]")
                 destination
                 (eq source-kind :directory)
                 (and (not overwrite) (member kind '(:file :symlink))))
         :path
         destination)))))

(aitools.kernel.domain:define-flow
 (%rm-requests/k context recursive max-entries max-bytes &key on-error on-committed)
 (let* ((view (write-context-view context))
        (host (write-context-host context))
        (root (write-context-root context))
        (pending (list (context-path context)))
        (requests '())
        (entries 0)
        (bytes 0)
        (failure nil))
   (loop while pending
         for path = (pop pending)
         for absolute = (%target-absolute host root path :root)
         for kind = (aitools.store.domain:entry-state-kind
                     (aitools.store.application:view-path-kind view path))
         for file-bytes = nil
         until failure
         do
         (when (or (zerop (length path))
                   (string= absolute "/")
                   (equal absolute
                          (aitools.workspace.application:workspace-root-real root))
                   (equal absolute
                          (aitools.workspace.application:resolve-real-path
                           host
                           (aitools.workspace.application:host-home-directory host)))
                   (loop for start = 0 then (1+ slash)
                         for slash = (position #\/ path :start start)
                         thereis (member (subseq path start slash)
                                         '(".git" ".jj")
                                         :test #'string-equal)
                         while slash))
           (setf failure
                 (list "refusal.outside-workspace"
                       "rm refuses roots, home, and version-control metadata")))
         ;; Check directories themselves and their parents without following leaf links.
         (unless failure
           (aitools.kernel.domain:flow-call
            (%boundary/k host
                         root
                         absolute
                         (write-context-temporary-root context)
                         (eq kind :directory))
            (:on-inside (real relative verdict)
                        (declare (ignore real relative))
                        (unless (eq verdict :inside)
                          (setf failure (list "refusal.outside-workspace" "rm requires a workspace target"))))
            (:on-outside (verdict lexical real)
                         (declare (ignore lexical real))
                         (setf failure (list "refusal.outside-workspace"
                                             (format nil "rm refuses a protected target (~A)" verdict)))))
           )
         (unless failure
           (when (eq kind :absent)
             (aitools.kernel.domain:flow-call
              (%not-found context path)
              (:on-error (code message &key candidates diagnostics conflicts repairs path)
                         (setf failure
                               (list code
                                     message
                                     :candidates candidates
                                     :diagnostics diagnostics
                                     :conflicts conflicts
                                     :repairs repairs
                                     :path path))))
             )
           (when (and (null failure)
                      (not (member kind '(:absent :file :directory :symlink))))
             (setf failure
                   (list "refusal.not-a-file"
                         "rm accepts only files, directories, and symlinks")))
           (when (null failure)
             (incf entries)
             (setf failure
                   (and (> entries max-entries)
                        (list "refusal.too-large" "rm exceeds --max-entries")))
             (when (and (null failure) (eq kind :file))
               ;; A tx may replace a large disk file with a small staged blob.
               ;; Its view bytes, not the stale disk size, define the limit.
               (let ((stat (and (not (write-context-tx context))
                                (aitools.workspace.application:host-stat host absolute))))
                 (setf failure
                       (and stat
                            (> (+ bytes
                                  (aitools.workspace.domain:workspace-entry-size stat))
                               max-bytes)
                            (list "refusal.too-large" "rm exceeds --max-bytes"))))
               (and (null failure)
                    (setf file-bytes
                          (length
                           (aitools.store.application:view-read-file
                            view path :max-bytes (- max-bytes bytes))))
                    (incf bytes file-bytes)
                    (setf failure
                          (and (> bytes max-bytes)
                               (list "refusal.too-large" "rm exceeds --max-bytes")))))
             (and (null failure)
                  (push (aitools.store.domain:delete-request path :max-bytes file-bytes)
                        requests))
             (and (null failure)
                  recursive
                  (eq kind :directory)
                  (mapc (lambda (entry)
                          (push (%child path (car entry)) pending))
                        (aitools.store.application:view-directory-entries
                         view path)))))) (if failure
               (destructuring-bind (code message &key candidates diagnostics conflicts repairs path)
                   failure
                 (flow-continue
                  :on-error
                  code
                  message
                  :candidates
                  candidates
                  :diagnostics
                  diagnostics
                  :conflicts
                  conflicts
                  :repairs
                  repairs
                  :path
                  path))
             (aitools.kernel.domain:flow-call
              (check-expect-count/k context entries)
              (:on-rejected (code message &key candidates diagnostics conflicts repairs path)
                            (flow-continue
                             :on-error
                             code
                             message
                             :candidates
                             candidates
                             :diagnostics
                             diagnostics
                             :conflicts
                             conflicts
                             :repairs
                             repairs
                             :path
                             path))
              (:on-ok ()
                      (aitools.kernel.domain:flow-continue :on-committed
                                                           requests
                                                           (list (cons "entries" entries)
                                                                 (cons "bytes" bytes)))))
             )))
