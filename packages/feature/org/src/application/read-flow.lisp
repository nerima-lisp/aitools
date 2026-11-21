(in-package #:aitools.org.application)

(defun %schema-repair (command detail)
  (list (repair "show-command" detail
                (format nil "aitools schema ~A" command))))

(defun %failure-code (failure) (case (aitools.org.domain:org-failure-kind failure) (:invalid-utf8 "input.not-utf8") ((:syntax :query-syntax) "input.syntax-error") ((:unsupported :internal-link) "input.unsupported-format") (:too-large "refusal.too-large") ((:argument :invalid-time) "argument.invalid") (:selection-no-match "selection.no-match") (:selection-ambiguous "selection.ambiguous") (:not-found "input.not-found") (:io "environment.io") (otherwise "internal.unexpected")))

(defun %failure-diagnostics (failure)
  (let ((message (aitools.org.domain:org-failure-message failure))
        (line (aitools.org.domain:org-failure-line failure))
        (column (aitools.org.domain:org-failure-column failure))
        (offset (aitools.org.domain:org-failure-offset failure)))
    (cond
      ((and line column)
       (list (json-object "line" line "col" column "message" message)))
      (offset (list (json-object "offset" offset "message" message))))))

(defun %failure-response (failure command)
  (values (%failure-code failure)
          (aitools.org.domain:org-failure-message failure)
          (%failure-diagnostics failure)
          (%schema-repair command "Check the command arguments and Org input.")))

(defun %utf16-or-32-bom (octets)
  (let ((length (length octets)))
    (cond
      ((and (>= length 4) (equalp (subseq octets 0 4) #(255 254 0 0)))
       "UTF-32 little-endian")
      ((and (>= length 4) (equalp (subseq octets 0 4) #(0 0 254 255)))
       "UTF-32 big-endian")
      ((and (>= length 2) (equalp (subseq octets 0 2) #(255 254)))
       "UTF-16 little-endian")
      ((and (>= length 2) (equalp (subseq octets 0 2) #(254 255)))
       "UTF-16 big-endian"))))

(defun %org-path-p (path)
  (and (stringp path) (plusp (length path))))

(aitools.kernel.domain:define-flow
    (%org-read-root/k boundaries root command &key on-root on-error)
  (let ((host (cl-boundary-kit:boundary-context-require boundaries :org-workspace-host))
        (source (cl-boundary-kit:boundary-context-require boundaries :org-text-source))
        (max-input-bytes (cl-boundary-kit:boundary-context-require boundaries :org-max-input-bytes))
        (parser (cl-boundary-kit:boundary-context-require boundaries :org-parse)))
    (unless (and host source (integerp max-input-bytes) parser)
      (return-from %org-read-root/k
        (aitools.kernel.domain:flow-continue
         :on-error "internal.unexpected" "the org context is missing a required boundary capability"
         :repairs (%schema-repair command "Show this command's requirements."))))
    (aitools.workspace.application:call-with-resolved-root/k
     host :root root
     :on-resolved
     (lambda (workspace-root)
       (aitools.kernel.domain:flow-continue
        :on-root workspace-root host source max-input-bytes parser))
     :on-error
     (lambda (reason root-path)
       (aitools.kernel.domain:flow-continue
        :on-error "input.not-found"
        (format nil "workspace root ~A ~A" root-path
                (if (eq reason :not-a-directory)
                    "is not a directory" "does not exist"))
        :repairs (%schema-repair command "Use an existing workspace root."))))))

(aitools.kernel.domain:define-flow
    (%org-read-file/k source max-input-bytes parser absolute path command
                      &key report-parse-path on-document on-error)
  (aitools.text.application:call-with-text-file/k
   source absolute
   :max-bytes max-input-bytes
   :on-text
   (lambda (octets layout)
     (declare (ignore layout))
     (multiple-value-bind (document failure)
         (if (find 0 octets)
             (values nil
                     (aitools.org.domain:make-org-failure
                      :kind :unsupported :message "Org input contains a NUL byte"))
             (funcall parser octets :source-name absolute))
       (if failure
           (multiple-value-bind (code message diagnostics repairs)
               (%failure-response failure command)
             (aitools.kernel.domain:flow-continue
              :on-error code (if report-parse-path (format nil "~A: ~A" path message) message)
              :diagnostics diagnostics :repairs repairs))
           (aitools.kernel.domain:flow-continue :on-document document))))
   :on-binary
   (lambda (prefix size)
     (declare (ignore size))
     (let ((encoding (%utf16-or-32-bom prefix)))
       (aitools.kernel.domain:flow-continue
        :on-error "input.unsupported-format"
        (if encoding
            (format nil "~A uses unsupported ~A encoding" path encoding)
            (format nil "~A is a binary file, not a UTF-8 Org document" path))
        :repairs (%schema-repair command "Use a UTF-8 Org document."))))
   :on-missing
   (lambda (missing)
     (declare (ignore missing))
     (aitools.kernel.domain:flow-continue
      :on-error "input.not-found" (format nil "file ~A does not exist" path)
      :repairs (%schema-repair command "Pass an existing Org file.")))
   :on-unreadable
   (lambda (unreadable)
     (declare (ignore unreadable))
     (aitools.kernel.domain:flow-continue
      :on-error "environment.io" (format nil "file ~A cannot be read" path)
      :repairs (%schema-repair command "Check file permissions.")))
   :on-too-large
   (lambda (size)
     (aitools.kernel.domain:flow-continue
      :on-error "refusal.too-large"
      (format nil "file ~A exceeds the Org input limit of ~D bytes" path size)
      :repairs (%schema-repair command "Use a smaller Org file.")))))

(aitools.kernel.domain:define-flow
    (org-read-document/k boundaries path root command &key on-document on-error)
  (unless (%org-path-p path)
    (return-from org-read-document/k
      (aitools.kernel.domain:flow-continue
       :on-error "argument.invalid" "an Org file path is required"
       :repairs (%schema-repair command "Pass an Org file path."))))
  (%org-read-root/k
   boundaries root command :on-error on-error
   :on-root
   (lambda (workspace-root host source max-input-bytes parser)
     (declare (ignore workspace-root))
     (%org-read-file/k
      source max-input-bytes parser
      (aitools.workspace.application:user-path-absolute host path)
      path command :on-document on-document :on-error on-error))))

(defun %org-path-kind (host absolute)
  (let ((entry (aitools.workspace.application:host-stat host absolute)))
    (and entry (aitools.workspace.application:workspace-entry-kind entry))))

(defun %org-encrypted-path-p (path)
  (let ((start (- (length path) 4)))
    (and (>= start 0) (string-equal ".gpg" path :start2 start))))

(aitools.kernel.domain:define-flow
    (%org-collect-files/k host root paths command &key on-files on-error)
  (let ((real-root (aitools.workspace.application:workspace-root-real root))
        (files (make-hash-table :test #'equal))
        (directories nil))
    (flet ((fail (code message)
             (return-from %org-collect-files/k
               (aitools.kernel.domain:flow-continue
                :on-error code message
                :repairs (%schema-repair command "Pass readable paths inside the workspace.")))))
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
             (fail "refusal.not-a-file" (format nil "~A is not a regular file or directory" path))))))
      (when directories
        (aitools.workspace.application:call-with-workspace-scan/k
         host root :paths directories :glob '("*.org") :no-ignore t :skip-larger-than nil
         :emit
         (lambda (entry result)
           (declare (ignore result))
           (when (eq (aitools.workspace.application:scan-entry-kind entry) :file)
             (setf (gethash (aitools.workspace.application:scan-entry-path entry) files)
                   (aitools.workspace.application:scan-entry-absolute entry)))
           nil)
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
              (fail "refusal.outside-workspace" (format nil "~A is outside the workspace" path)))))))
      (aitools.kernel.domain:flow-continue
       :on-files
       (sort (loop for relative being the hash-keys of files using (hash-value absolute)
                   collect (cons relative absolute))
             #'string< :key #'car)))))

(aitools.kernel.domain:define-flow
    (%org-map-documents/k boundaries paths root command read-result &key on-results on-error)
  (unless (and paths (every #'%org-path-p paths))
    (return-from %org-map-documents/k
      (aitools.kernel.domain:flow-continue
       :on-error "argument.invalid" "an Org file path is required"
       :repairs (%schema-repair command "Pass an Org file or directory."))))
  (%org-read-root/k
   boundaries root command :on-error on-error
   :on-root
   (lambda (workspace-root host source max-input-bytes parser)
     (labels ((fail (code message &key diagnostics repairs)
                (return-from %org-map-documents/k
                  (aitools.kernel.domain:flow-continue
                   :on-error code message :diagnostics diagnostics :repairs repairs)))
              (report-failure (failure path)
                (multiple-value-bind (code message diagnostics repairs)
                    (%failure-response failure command)
                  (fail code (if path (format nil "~A: ~A" path message) message)
                        :diagnostics diagnostics :repairs repairs)))
              (evaluate (document path)
                (multiple-value-bind (result failure) (funcall read-result document)
                  (when failure
                    (report-failure failure path))
                  result)))
       ;; Evaluate an empty document to validate options before collecting files.
       (multiple-value-bind (document failure)
           (funcall parser (make-array 0 :element-type '(unsigned-byte 8)))
         (when failure
           (report-failure failure nil))
         (evaluate document nil))
       (%org-collect-files/k
        host workspace-root paths command :on-error on-error
        :on-files
        (lambda (files)
          (let ((results nil))
            (loop for (relative . absolute) in files
                  do (%org-read-file/k
                      source max-input-bytes parser absolute relative command
                      :report-parse-path t
                      :on-error #'fail
                      :on-document
                      (lambda (document)
                        (push (cons relative (evaluate document relative)) results))))
            (aitools.kernel.domain:flow-continue :on-results (nreverse results)))))))))
