(in-package #:aitools.org.application)

(defun %org-workspace-path (boundaries root path)
  "PATH as a workspace-relative path, or its absolute form when it lies outside the workspace."
  (let* ((host (cl-boundary-kit:boundary-context-require boundaries :org-workspace-host))
         (absolute (aitools.workspace.application:user-path-absolute host path))
         (relative nil))
    (aitools.workspace.application:call-with-resolved-root/k
     host :root root
     :on-resolved (lambda (workspace-root)
                    (setf relative (aitools.workspace.application:workspace-relative-path host workspace-root absolute)))
     :on-error (lambda (reason root-path) (declare (ignore reason root-path)) nil))
    (or relative absolute)))

(defun %org-path-kind (host absolute)
  (let ((entry (aitools.workspace.application:host-stat host absolute)))
    (and entry (aitools.workspace.application:workspace-entry-kind entry))))

(defun %org-encrypted-path-p (path)
  (let ((start (- (length path) 4)))
    (and (>= start 0) (string-equal ".gpg" path :start2 start))))

(defun %org-classify-paths (host real-root paths)
  "(VALUES FILES DIRECTORIES FAILURE) for the explicitly named PATHS. FILES maps
workspace-relative paths to absolute ones; FAILURE is (CODE MESSAGE) for the
first path that cannot be read."
  (let ((files (make-hash-table :test #'equal))
        (directories nil))
    (flet ((fail (code message)
             (return-from %org-classify-paths (values nil nil (list code message)))))
      ;; Validate explicitly named children even when their parent is also named.
      (dolist (path paths)
        (let* ((absolute (aitools.workspace.application:user-path-absolute host path))
               (real (aitools.workspace.application:resolve-real-path host absolute)))
          (unless real
            (fail "input.not-found" (format nil "cannot resolve ~A" path)))
          (unless (aitools.kernel.domain:path-inside-p real-root real)
            (fail "refusal.outside-workspace"
                  (format nil "~A is outside the workspace" path)))
          (case (%org-path-kind host real)
            ((nil) (fail "input.not-found" (format nil "~A does not exist" path)))
            (:directory (push real directories))
            (:file
             (unless (or (%org-encrypted-path-p absolute) (%org-encrypted-path-p real))
               (setf (gethash (aitools.kernel.domain:path-relative-to real-root real) files)
                     real)))
            (otherwise
             (fail "refusal.not-a-file" (format nil "~A is not a regular file or directory" path)))))))
    (values files directories nil)))

(aitools.kernel.domain:define-flow
    (%org-collect-files/k host root paths command &key on-files on-error)
  (multiple-value-bind (files directories failure)
      (%org-classify-paths host (aitools.workspace.application:workspace-root-real root) paths)
    (flet ((fail (code message)
             (unless failure (setf failure (list code message)))))
      (when (and directories (not failure))
        (aitools.workspace.application:call-with-workspace-scan/k
         host root :paths directories :glob '("*.org") :no-ignore t :skip-larger-than nil
         :emit
         (lambda (entry result)
           (declare (ignore result))
           (when (eq (aitools.workspace.application:scan-entry-kind entry) :file)
             (setf (gethash (aitools.workspace.application:scan-entry-path entry) files)
                   (aitools.workspace.application:scan-entry-absolute entry)))
           (and failure :stop))
         :on-skip
         (lambda (entry reason)
           (declare (ignore reason))
           (fail "environment.io"
                 (format nil "cannot read directory ~A"
                         (aitools.workspace.application:scan-entry-path entry))))
         :on-complete
         (lambda (source stopped)
           (declare (ignore source stopped))
           nil)
         :on-error
         (lambda (reason path)
           (ecase reason
             (:not-found (fail "input.not-found" (format nil "~A does not exist" path)))
             (:outside-root
              (fail "refusal.outside-workspace" (format nil "~A is outside the workspace" path))))))))
    (if failure
        (aitools.kernel.domain:flow-continue
         :on-error (first failure) (second failure)
         :repairs (%schema-repair command "Pass readable paths inside the workspace."))
        (aitools.kernel.domain:flow-continue
         :on-files
         (sort (loop for relative being the hash-keys of files using (hash-value absolute)
                     collect (cons relative absolute))
               #'string< :key #'car)))))

(defun %org-failure-fields (failure command path)
  "(CODE MESSAGE DIAGNOSTICS REPAIRS) for FAILURE, with PATH prefixed to the message when given."
  (multiple-value-bind (code message diagnostics repairs) (%failure-response failure command)
    (list code (if path (format nil "~A: ~A" path message) message) diagnostics repairs)))

(defun %org-evaluate-document (read-result document path command)
  "(VALUES RESULT FAILURE-FIELDS): READ-RESULT applied to DOCUMENT, with a failure in %ORG-FAILURE-FIELDS form."
  (multiple-value-bind (result failure) (funcall read-result document)
    (if failure
        (values nil (%org-failure-fields failure command path))
        (values result nil))))

(defun %org-read-options-failure (parser read-result command)
  "The failure fields when READ-RESULT rejects an empty document, which validates its options before any file is read."
  (multiple-value-bind (document failure)
      (funcall parser (make-array 0 :element-type '(unsigned-byte 8)))
    (if failure
        (%org-failure-fields failure command nil)
        (nth-value 1 (%org-evaluate-document read-result document nil command)))))

(aitools.kernel.domain:define-flow
    (%org-map-files/k workspace-root host source max-input-bytes parser paths command read-result
                      &key on-results on-error)
  (flet ((reject (fields)
           (destructuring-bind (code message diagnostics repairs) fields
             (aitools.kernel.domain:flow-continue
              :on-error code message :diagnostics diagnostics :repairs repairs))))
    (let ((invalid (%org-read-options-failure parser read-result command)))
      (if invalid
          (reject invalid)
          (aitools.kernel.domain:flow-call
              (%org-collect-files/k host workspace-root paths command)
            (:on-error (code message &key repairs)
              (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))
            (:on-files (files)
              (let ((results nil)
                    (rejection nil))
                (loop for (relative . absolute) in files
                      until rejection
                      do (aitools.kernel.domain:flow-call
                             (%org-read-file/k
                              source max-input-bytes parser absolute relative command
                              :report-parse-path t)
                           (:on-error (code message &key diagnostics repairs)
                             (setf rejection (list code message diagnostics repairs)))
                           (:on-document (document)
                             (multiple-value-bind (result failure)
                                 (%org-evaluate-document read-result document relative command)
                               (if failure
                                   (setf rejection failure)
                                   (push (cons relative result) results))))))
                (if rejection
                    (reject rejection)
                    (aitools.kernel.domain:flow-continue :on-results (nreverse results))))))))))

(aitools.kernel.domain:define-flow
    (%org-map-documents/k boundaries paths root command read-result &key on-results on-error)
  (if (and paths (every #'%org-path-p paths))
      (aitools.kernel.domain:flow-call
          (%org-read-root/k boundaries root command)
        (:on-error (code message &key repairs)
          (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))
        (:on-root (workspace-root host source max-input-bytes parser)
          (aitools.kernel.domain:flow-call
              (%org-map-files/k
               workspace-root host source max-input-bytes parser paths command read-result)
            (:on-error (code message &key diagnostics repairs)
              (aitools.kernel.domain:flow-continue
               :on-error code message :diagnostics diagnostics :repairs repairs))
            (:on-results (results)
              (aitools.kernel.domain:flow-continue :on-results results)))))
      (aitools.kernel.domain:flow-continue
       :on-error "argument.invalid" "an Org file path is required"
       :repairs (%schema-repair command "Pass an Org file or directory."))))
