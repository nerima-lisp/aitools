;;;; packages/feature/edit/src/application/scan.lisp
;;;;
;;;; SCAN-FILES/K: the workspace scan the multi-file writes (replace,
;;;; archive create) select their files with, honouring the common scan
;;;; options and, with --tx, the tx's staged state.
(in-package #:aitools.edit.application)

;;; ------------------------------------------------------------------ scans

(aitools.kernel.domain:define-flow
  (%scan-options/k env options &key on-error on-options)
  "Validate common scan options and pass the resulting keywords."
  (let ((host (command-env-host env)))
    (aitools.workspace.application:call-with-scan-options/k
      host
      :path-absolute
      (lambda (path)
        (aitools.workspace.application:user-path-absolute host path))
      :current-time
      (lambda ()
        (funcall (edit-ports-unix-now (command-env-ports env))))
      :language-predicate
      #'aitools.text.domain:language-path-predicate
      :language-names
      (aitools.text.domain:language-names)
      :glob (getf options :glob)
      :lang (getf options :lang)
      :no-ignore (getf options :no-ignore)
      :skip-larger-than (getf options :skip-larger-than)
      :newer (getf options :newer)
      :on-error
      (lambda (kind value names)
        (aitools.kernel.domain:flow-continue
          :on-error
          (if (eq kind :unknown-language)
              "input.unsupported-language"
              "argument.invalid")
          (case kind
            (:unknown-language
             (format nil "unknown --lang ~S; known: ~{~A~^, ~}" value names))
            (:invalid-size
             (format nil "--skip-larger-than ~S is not a size" value))
            (:invalid-newer
             (format nil "--newer ~S is neither a duration nor an existing path" value)))))
      :on-options
      (lambda (keywords)
        (aitools.kernel.domain:flow-continue :on-options keywords)))))

(defun %tx-overlay (env tx)
  "A WORKSPACE-OVERLAY showing TX's staged state to the scan, or NIL
when the tx does not exist (the write itself then reports it)."
  (aitools.kernel.domain:flow-call
   (aitools.store.application:call-with-tx-view/k
    (funcall
     (edit-ports-open-store (command-env-ports env))
     (aitools.workspace.application:workspace-root-real
      (command-env-root env)))
    tx)
   (:on-not-found ()
     nil)
   (:on-view (view)
     (aitools.workspace.application:make-workspace-overlay
      :list-directory
      (lambda (relative disk-entries)
       (mapcar
        (lambda (entry)
         (or (find (car entry)
                   disk-entries
                   :key #'aitools.workspace.application:workspace-entry-name
                   :test #'string=)
             (aitools.workspace.application:make-workspace-entry
              :name (coerce (car entry) 'simple-string)
              :kind (cdr entry)
              :size (length
                     (or (aitools.store.application:view-read-file
                          view
                          (%child relative (car entry)))
                         #()))
              :mtime 0
              :mode #o644)))
        (aitools.store.application:view-directory-entries view relative)))
      :read-octets
      (lambda (relative)
       (values t
               (aitools.store.application:view-read-file view relative)))))))

(aitools.kernel.domain:define-flow
  (scan-files/k env paths options &key (kinds (list :file)) skip-handler on-error on-files)
  "Return workspace-relative entries of KINDS below PATHS in path order."
  (aitools.kernel.domain:flow-call
    (%scan-options/k env options)
    (:on-error (code message)
      (aitools.kernel.domain:flow-continue :on-error code message))
    (:on-options (keywords)
      (when (getf options :tx)
        (setf keywords
              (list* :overlay (%tx-overlay env (getf options :tx)) keywords)))
      (let ((found (list)))
        (apply #'aitools.workspace.application:call-with-workspace-scan/k
               (command-env-host env)
               (command-env-root env)
               :paths paths
               :emit
               (lambda (entry result)
                 (declare (ignore result))
                 (when (member (aitools.workspace.application:scan-entry-kind entry) kinds)
                   (push entry found))
                 nil)
               :on-skip
               (lambda (entry reason)
                 (when skip-handler
                   (funcall skip-handler entry reason)))
               :on-complete
               (lambda (source stopped)
                 (declare (ignore source stopped))
                 (aitools.kernel.domain:flow-continue :on-files (nreverse found)))
               :on-error
               (lambda (reason path)
                 (if (eq reason :not-found)
                     (aitools.kernel.domain:flow-continue
                       :on-error
                       "input.not-found"
                       (format nil "~A does not exist" path)
                       :path path)
                     (aitools.kernel.domain:flow-continue
                       :on-error
                       "refusal.outside-workspace"
                       (format nil "~A is outside the workspace" path)
                       :path path)))
               keywords)))))
