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
   (%org-read-file/k source max-input-bytes parser absolute path command &key on-document on-error)
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
              :on-error code message :diagnostics diagnostics :repairs repairs))
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
  (let ((files (make-hash-table :test #'equal))
        (directories nil))
    (labels ((fail (code message)
               (return-from %org-collect-files/k
                 (aitools.kernel.domain:flow-continue
                  :on-error code message
                  :repairs (%schema-repair command "Pass readable paths inside the workspace."))))
             (with-path (path display handle)
               (aitools.workspace.application:call-with-workspace-boundary/k
                host root path
                :on-inside
                (lambda (resolved verdict)
                  (declare (ignore verdict))
                  (funcall handle (aitools.kernel.domain:workspace-path-real resolved)
                          (aitools.kernel.domain:workspace-path-relative resolved)))
                :on-outside
                (lambda (reason lexical real)
                  (declare (ignore lexical real))
                  (if (eq reason :unresolvable)
                      (fail "input.not-found" (format nil "cannot resolve ~A" display))
                      (fail "refusal.outside-workspace"
                            (format nil "~A is outside the workspace" display))))))
             (remember-file (absolute relative)
               (unless (%org-encrypted-path-p absolute)
                 (setf (gethash relative files) absolute))))
      ;; Check every explicit path before expanding any directory, including
      ;; paths nested under another argument.
      (dolist (path paths)
        (with-path
         (aitools.workspace.application:user-path-absolute host path) path
         (lambda (absolute relative)
           (case (%org-path-kind host absolute)
             ((nil) (fail "input.not-found" (format nil "~A does not exist" path)))
             (:directory (pushnew absolute directories :test #'string=))
             (:file (unless (%org-encrypted-path-p path)
                      (remember-file absolute relative)))
             (otherwise (fail "refusal.not-a-file"
                              (format nil "~A is not a regular file or directory" path)))))))
      (dolist (directory directories)
        (aitools.workspace.application:call-with-workspace-scan/k
         host root :paths (list directory) :glob '("*.org")
         :no-ignore t :skip-larger-than nil
         :emit
         (lambda (entry result)
           (declare (ignore result))
           (when (eq (aitools.workspace.application:scan-entry-kind entry) :file)
             (with-path
              (aitools.workspace.application:scan-entry-absolute entry)
              (aitools.workspace.application:scan-entry-path entry)
              #'remember-file))
           nil)
         :on-skip
         (lambda (entry reason)
           (declare (ignore reason))
           (fail "environment.io"
                 (format nil "cannot read directory ~A"
                         (aitools.workspace.application:scan-entry-path entry))))
         :on-complete (lambda (source stopped)
                        (declare (ignore source stopped))
                        nil)
         :on-error
         (lambda (reason path)
           (ecase reason
             (:not-found (fail "input.not-found" (format nil "~A does not exist" path)))
             (:outside-root
              (fail "refusal.outside-workspace"
                    (format nil "~A is outside the workspace" path)))))))
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
     (let ((results nil))
       (labels ((fail (path code message &key diagnostics repairs)
                  (return-from %org-map-documents/k
                    (aitools.kernel.domain:flow-continue
                     :on-error code (if path (format nil "~A: ~A" path message) message)
                     :diagnostics diagnostics :repairs repairs)))
                (fail-result (path failure)
                  (multiple-value-bind (code message diagnostics repairs)
                      (%failure-response failure command)
                    (fail path code message :diagnostics diagnostics :repairs repairs)))
                (collect-result (document path)
                  (multiple-value-bind (result failure) (funcall read-result document)
                    (when failure
                      (fail-result path failure))
                    (push (cons path result) results)))
                (read-file (absolute display path)
                  (%org-read-file/k
                   source max-input-bytes parser absolute display command
                   :on-document (lambda (document) (collect-result document path))
                   :on-error (lambda (code message &key diagnostics repairs)
                               (fail path code message :diagnostics diagnostics :repairs repairs))))
                (collect-files (files)
                  (if files
                      (loop for (relative . absolute) in files
                            do (read-file absolute relative relative))
                      ;; The adapters validate command options even for an empty selection.
                      (multiple-value-bind (document failure)
                          (funcall parser (make-array 0 :element-type '(unsigned-byte 8)))
                        (when failure
                          (fail-result nil failure))
                        (collect-result document nil)))))
         (let* ((single (null (rest paths)))
                (path (first paths))
                (absolute (aitools.workspace.application:user-path-absolute host path))
                (real (and single (aitools.workspace.application:resolve-real-path host absolute)))
                (kind (and real (%org-path-kind host real))))
           (if (and single (not (eq kind :directory))
                    (not (and kind (or (%org-encrypted-path-p path)
                                       (%org-encrypted-path-p real)))))
               (read-file absolute path nil)
               (%org-collect-files/k
                host workspace-root paths command :on-files #'collect-files
                :on-error (lambda (code message &key repairs)
                            (fail nil code message :repairs repairs)))))
         (aitools.kernel.domain:flow-continue :on-results (nreverse results)))))))
