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
  (let ((host (command-env-host env)))
    (aitools.workspace.application:call-with-scan-options/k
     host
     :path-absolute (lambda (path) (aitools.workspace.application:user-path-absolute host path))
     :current-time (lambda () (funcall (edit-ports-unix-now (command-env-ports env))))
     :language-predicate #'aitools.text.domain:language-path-predicate
     :language-names (aitools.text.domain:language-names)
     :glob (getf options :glob)
     :lang (getf options :lang)
     :no-ignore (getf options :no-ignore)
     :skip-larger-than (getf options :skip-larger-than)
     :newer (getf options :newer)
     :on-error
     (lambda (kind value names)
       (declare (ignore names))
       ;; Keep the historical internal repair classification. RUN-EDIT-COMMAND
       ;; exposes this as argument.invalid while retaining the read-path repair.
       (funcall fail (if (eq kind :unknown-language) "scan.unknown-language" "argument.invalid")
                (case kind
                  (:unknown-language (format nil "unknown --lang ~S; known: ~{~A~^, ~}" value
                                             (aitools.text.domain:language-names)))
                  (:invalid-size (format nil "--skip-larger-than ~S is not a size" value))
                  (:invalid-newer (format nil "--newer ~S is neither a duration nor an existing path" value)))))
     :on-options on-options)))

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
