;;;; packages/feature/edit/src/application/scan.lisp
;;;;
;;;; SCAN-FILES/K: the workspace scan the multi-file writes (replace,
;;;; archive create) select their files with, honouring the common scan
;;;; options and, with --tx, the tx's staged state.
(in-package #:aitools.edit.application)

;;; ------------------------------------------------------------------ scans

(defun %scan-options/k (env options fail on-options)
  "The common scan options as CALL-WITH-WORKSPACE-SCAN/K keywords."
  (declare (type function fail on-options))
  (let* ((host (command-env-host env))
         (skip (getf options :skip-larger-than))
         (lang (getf options :lang))
         (newer (getf options :newer))
         (predicate (and lang (aitools.text.domain:language-path-predicate lang)))
         (limit (handler-case (if skip (aitools.kernel.domain:size-bytes (aitools.kernel.domain:parse-size skip))
                                  aitools.workspace.application:+default-skip-larger-than+)
                  (error () (return-from %scan-options/k
                              (funcall fail "argument.invalid" (format nil "--skip-larger-than ~S is not a size" skip)))))))
    (when (and lang (null predicate))
      (return-from %scan-options/k
        (funcall fail "input.unsupported-language" (format nil "unknown --lang ~S" lang))))
    (let ((newer-seconds
            (and newer
                 (handler-case
                     (- (floor (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))
                        (floor (aitools.kernel.domain:duration-milliseconds (aitools.kernel.domain:parse-duration newer))
                               1000))
                   (error ()
                     (let* ((absolute (aitools.workspace.application:user-path-absolute host newer))
                            (entry (aitools.workspace.application:host-stat host absolute)))
                       (if entry
                           (aitools.workspace.application:workspace-entry-mtime entry)
                           (return-from %scan-options/k
                             (funcall fail "argument.invalid"
                                      (format nil "--newer ~S is neither a duration nor an existing path" newer))))))))))
      (funcall on-options (list :glob (getf options :glob) :lang predicate :no-ignore (getf options :no-ignore)
                                :skip-larger-than limit :newer newer-seconds)))))

(defun %tx-overlay (env tx)
  "A WORKSPACE-OVERLAY showing TX's staged state to the scan, or NIL
when the tx does not exist (the write itself then reports it)."
  (aitools.store.application:call-with-tx-view/k
   (funcall (edit-ports-open-store (command-env-ports env))
            (aitools.workspace.application:workspace-root-real (command-env-root env)))
   tx
   :on-not-found (constantly nil)
   :on-view
   (lambda (view)
     (aitools.workspace.application:make-workspace-overlay
      :list-directory
      (lambda (relative disk-entries)
        (mapcar (lambda (entry)
                  (or (find (car entry) disk-entries :key #'aitools.workspace.application:workspace-entry-name
                                                      :test #'string=)
                      (aitools.workspace.application:make-workspace-entry
                       :name (coerce (car entry) 'simple-string) :kind (cdr entry)
                       :size (length (or (aitools.store.application:view-read-file view (%child relative (car entry))) #()))
                       :mtime 0 :mode #o644)))
                (aitools.store.application:view-directory-entries view relative)))
      :read-octets (lambda (relative) (values t (aitools.store.application:view-read-file view relative)))))))

(defun scan-files/k (env paths options fail on-files &key (kinds '(:file)))
  "Workspace-relative paths of the entries of KINDS below PATHS (the root
when NIL), in path order, honouring the scan options; with --tx the scan
sees the tx's staged files."
  (declare (type function fail on-files))
  (%scan-options/k env options fail
                   (lambda (keywords)
                     (when (getf options :tx)
                       (setf keywords (list* :overlay (%tx-overlay env (getf options :tx)) keywords)))
                     (let ((found '()))
                       (apply #'aitools.workspace.application:call-with-workspace-scan/k
                              (command-env-host env) (command-env-root env)
                              :paths paths
                              :emit (lambda (entry result)
                                      (declare (ignore result))
                                      (when (member (aitools.workspace.application:scan-entry-kind entry) kinds)
                                        (push entry found))
                                      nil)
                              :on-complete (lambda (source stopped)
                                             (declare (ignore source stopped))
                                             (funcall on-files (nreverse found)))
                              :on-error (lambda (reason path)
                                          (if (eq reason :not-found)
                                              (funcall fail "input.not-found" (format nil "~A does not exist" path) :path path)
                                              (funcall fail "refusal.outside-workspace"
                                                       (format nil "~A is outside the workspace" path) :path path)))
                              keywords)))))
