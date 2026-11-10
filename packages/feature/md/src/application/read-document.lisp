(in-package #:aitools.md.application)

(defconstant +md-max-input-bytes+
  (* 16 1024 1024))
(defun %md-parse-octets (parser octets)
  (when (> (length octets) +md-max-input-bytes+)
    (return-from %md-parse-octets (values nil "refusal.too-large" nil nil)))
  (let ((source nil)
        (invalid-position nil))
    (aitools.text.domain:decode-utf8-strict/k
     octets
     :on-decoded (lambda (decoded) (setf source decoded))
     :on-invalid (lambda (position) (setf invalid-position position)))
    (when invalid-position
      (return-from %md-parse-octets
        (values nil
                "input.not-utf8"
                (format nil "Markdown input is not UTF-8 at byte ~D" invalid-position)
                invalid-position)))
    (funcall parser source octets)))

(defun %md-schema-repair (command detail)
  (list (repair "inspect-schema" detail (format nil "aitools schema ~A" command))))

(aitools.kernel.domain:define-flow (%md-read-document/k boundaries path root command &key on-document on-error)
                                   (declare (type function on-document on-error))
                                   (let ((host (cl-boundary-kit:boundary-context-require boundaries :md-workspace-host))
                                         (text-context (cl-boundary-kit:boundary-context-require boundaries :md-text-context))
                                         (parser (cl-boundary-kit:boundary-context-require boundaries :md-parse-markdown)))
                                     (cond
                                       ((or (not (stringp path)) (zerop (length path)))
                                        (aitools.kernel.domain:flow-continue :on-error
                                                                             "argument.invalid"
                                                                             "a Markdown file path is required"
                                                                             :repairs
                                                                             (%md-schema-repair command
                                                                                                "Pass a Markdown file path.")))
                                       ((or (null host) (null text-context) (null parser))
                                        (aitools.kernel.domain:flow-continue :on-error
                                                                             "internal.unexpected"
                                                                             "the md context is missing a required capability"
                                                                             :repairs
                                                                             (%md-schema-repair command
                                                                                                "Show this command's requirements.")))
                                       (t
                                        (aitools.workspace.application:call-with-resolved-root/k host
                                                                                                 :root
                                                                                                 root
                                                                                                 :on-error
                                                                                                 (lambda
                                                                                                     (reason root-path)
                                                                                                   (aitools.kernel.domain:flow-continue
                                                                                                    :on-error
                                                                                                    (if (eq reason
                                                                                                            :not-found)
                                                                                                        "input.not-found"
                                                                                                        "argument.invalid")
                                                                                                    (format nil
                                                                                                            "workspace root ~A ~A"
                                                                                                            root-path
                                                                                                            (if (eq
                                                                                                                 reason
                                                                                                                 :not-a-directory)
                                                                                                                "is not a directory"
                                                                                                                "does not exist"))
                                                                                                    :repairs
                                                                                                    (%md-schema-repair
                                                                                                     command
                                                                                                     "Use an existing workspace root.")))
                                                                                                 :on-resolved
                                                                                                 (lambda
                                                                                                     (workspace-root)
                                                                                                   (declare (ignore
                                                                                                             workspace-root))
                                                                                                   (let ((absolute
                                                                                                          (aitools.workspace.application:user-path-absolute
                                                                                                           host
                                                                                                           path))
                                                                                                         (outcome nil))
                                                                                                     (aitools.text.application:call-with-text-file/k
                                                                                                      text-context
                                                                                                      absolute
                                                                                                      :max-bytes
                                                                                                      +md-max-input-bytes+
                                                                                                      :on-text
                                                                                                      (lambda
                                                                                                          (octets
                                                                                                           layout)
                                                                                                        (declare (ignore
                                                                                                                  layout))
                                                                                                        (setf outcome (list
                                                                                                                       :octets
                                                                                                                       octets)))
                                                                                                      :on-binary
                                                                                                      (lambda
                                                                                                          (octets size)
                                                                                                        (declare (ignore
                                                                                                                  size))
                                                                                                        (setf outcome (list
                                                                                                                       :binary
                                                                                                                       octets)))
                                                                                                      :on-missing
                                                                                                      (lambda (missing)
                                                                                                        (declare (ignore
                                                                                                                  missing))
                                                                                                        (setf outcome (list
                                                                                                                       :missing)))
                                                                                                      :on-unreadable
                                                                                                      (lambda
                                                                                                          (unreadable)
                                                                                                        (declare (ignore
                                                                                                                  unreadable))
                                                                                                        (setf outcome (list
                                                                                                                       :unreadable)))
                                                                                                      :on-too-large
                                                                                                      (lambda (size)
                                                                                                        (setf outcome (list
                                                                                                                       :too-large
                                                                                                                       size))))
                                                                                                     (case (first
                                                                                                            outcome)
                                                                                                       (:octets
                                                                                                        (multiple-value-bind (document
                                                                                                                              code
                                                                                                                              message
                                                                                                                              offset)
                                                                                                            (%md-parse-octets parser (second
                                                                                                              outcome))
                                                                                                          (if document
                                                                                                              (aitools.kernel.domain:flow-continue
                                                                                                               :on-document
                                                                                                               document)
                                                                                                              (aitools.kernel.domain:flow-continue
                                                                                                               :on-error
                                                                                                               code
                                                                                                               message
                                                                                                               :diagnostics
                                                                                                               (and
                                                                                                                offset
                                                                                                                (list
                                                                                                                 (json-object
                                                                                                                  "offset"
                                                                                                                  offset
                                                                                                                  "message"
                                                                                                                  message)))
                                                                                                               :repairs
                                                                                                               (%md-schema-repair
                                                                                                                command
                                                                                                                "Check the UTF-8 Markdown input.")))))
                                                                                                       (:binary
                                                                                                        (aitools.kernel.domain:flow-continue
                                                                                                         :on-error
                                                                                                         "input.unsupported-format"
                                                                                                         (format nil
                                                                                                                 "~A is a binary file, not a UTF-8 Markdown document"
                                                                                                                 path)
                                                                                                         :repairs
                                                                                                         (%md-schema-repair
                                                                                                          command
                                                                                                          "Use a UTF-8 Markdown document.")))
                                                                                                       (:missing
                                                                                                        (aitools.kernel.domain:flow-continue
                                                                                                         :on-error
                                                                                                         "input.not-found"
                                                                                                         (format nil
                                                                                                                 "file ~A does not exist"
                                                                                                                 path)
                                                                                                         :repairs
                                                                                                         (%md-schema-repair
                                                                                                          command
                                                                                                          "Pass an existing Markdown file.")))
                                                                                                       (:unreadable
                                                                                                        (aitools.kernel.domain:flow-continue
                                                                                                         :on-error
                                                                                                         "environment.io"
                                                                                                         (format nil
                                                                                                                 "file ~A cannot be read"
                                                                                                                 path)
                                                                                                         :repairs
                                                                                                         (%md-schema-repair
                                                                                                          command
                                                                                                          "Check file permissions.")))
                                                                                                       (:too-large
                                                                                                        (aitools.kernel.domain:flow-continue
                                                                                                         :on-error
                                                                                                         "refusal.too-large"
                                                                                                         (format nil
                                                                                                                 "file ~A exceeds the Markdown input limit of ~D bytes"
                                                                                                                 path
                                                                                                                 (second
                                                                                                                  outcome))
                                                                                                         :repairs
                                                                                                         (%md-schema-repair
                                                                                                          command
                                                                                                          "Use a smaller Markdown file.")))))))))))
