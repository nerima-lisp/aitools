(in-package #:aitools.org.application)

(defun %schema-repair (command detail)
  (list (repair "show-command" detail
                (format nil "aitools schema ~A" command))))

(defun %failure-code (failure)
  (case (aitools.org.domain:org-failure-kind failure)
    (:invalid-utf8 "input.not-utf8")
    ((:syntax :query-syntax) "input.syntax-error")
    ((:unsupported :internal-link) "input.unsupported-format")
    (:too-large "refusal.too-large")
    ((:argument :invalid-time) "argument.invalid")
    (:not-found "input.not-found")
    (:io "environment.io")
    (otherwise "internal.unexpected")))

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

(aitools.kernel.domain:define-flow
    (org-read-document/k ports path root command &key on-document on-error)
  (declare (type function on-document on-error))
  (let ((host (org-ports-workspace-host ports))
        (source (org-ports-text-source ports))
        (parser (org-ports-parse-org ports)))
    (cond
      ((or (not (stringp path)) (zerop (length path)))
       (aitools.kernel.domain:flow-continue
        :on-error "argument.invalid" "an Org file path is required"
        :repairs (%schema-repair command "Pass an Org file path.")))
      ((or (null host) (null source) (null parser))
       (aitools.kernel.domain:flow-continue
        :on-error "internal.unexpected" "the org context is missing a required port"
        :repairs (%schema-repair command "Show this command's requirements.")))
      (t
       (aitools.workspace.application:call-with-resolved-root/k
        host :root root
        :on-error
        (lambda (reason root-path)
          (aitools.kernel.domain:flow-continue
           :on-error "input.not-found"
           (format nil "workspace root ~A ~A" root-path
                   (if (eq reason :not-a-directory)
                       "is not a directory" "does not exist"))
           :repairs (%schema-repair command "Use an existing workspace root.")))
        :on-resolved
        (lambda (workspace-root)
          (declare (ignore workspace-root))
          (let ((absolute (aitools.workspace.application:user-path-absolute host path))
                (outcome nil))
            (aitools.text.application:call-with-text-file/k
             source absolute
             :max-bytes (org-ports-max-input-bytes ports)
             :on-text (lambda (octets layout)
                        (declare (ignore layout))
                        (setf outcome (list :octets octets)))
             :on-binary (lambda (prefix size)
                          (declare (ignore size))
                          (setf outcome (list :binary prefix)))
             :on-missing (lambda (missing)
                           (declare (ignore missing))
                           (setf outcome (list :missing)))
             :on-unreadable (lambda (unreadable)
                              (declare (ignore unreadable))
                              (setf outcome (list :unreadable)))
             :on-too-large (lambda (size)
                             (setf outcome (list :too-large size))))
            (case (first outcome)
              (:octets
               (labels ((report (failure)
                          (multiple-value-bind (code message diagnostics repairs)
                              (%failure-response failure command)
                            (aitools.kernel.domain:flow-continue
                             :on-error code message
                             :diagnostics diagnostics :repairs repairs))))
                 (let ((octets (second outcome)))
                   (multiple-value-bind (document failure)
                       (funcall parser octets :source-name absolute)
                     (if failure
                         (report failure)
                         (aitools.kernel.domain:flow-continue
                          :on-document document))))))
              (:binary
               (let ((encoding (%utf16-or-32-bom (second outcome))))
                 (aitools.kernel.domain:flow-continue
                  :on-error "input.unsupported-format"
                  (if encoding
                      (format nil "~A uses unsupported ~A encoding" path encoding)
                      (format nil "~A is a binary file, not a UTF-8 Org document" path))
                  :repairs (%schema-repair command "Use a UTF-8 Org document."))))
              (:missing
               (aitools.kernel.domain:flow-continue
                :on-error "input.not-found" (format nil "file ~A does not exist" path)
                :repairs (%schema-repair command "Pass an existing Org file.")))
              (:unreadable
               (aitools.kernel.domain:flow-continue
                :on-error "environment.io" (format nil "file ~A cannot be read" path)
                :repairs (%schema-repair command "Check file permissions.")))
              (:too-large
               (aitools.kernel.domain:flow-continue
                :on-error "refusal.too-large"
                (format nil "file ~A exceeds the Org input limit of ~D bytes"
                        path (second outcome))
                :repairs (%schema-repair command "Use a smaller Org file.")))))))))))
