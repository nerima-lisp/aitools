(in-package #:aitools.md.application)

(aitools.kernel.domain:define-flow (%md-read-replacement/k ports command &key on-replacement on-error)
                                   (declare (type function on-replacement on-error))
                                   (let ((reader (md-ports-read-stdin-octets ports))
                                         (parser (md-ports-parse-markdown ports)))
                                     (cond
                                       ((or (null reader) (null parser))
                                        (aitools.kernel.domain:flow-continue :on-error
                                                                             "internal.unexpected"
                                                                             "the md context cannot read replacement input"
                                                                             :repairs
                                                                             (%md-schema-repair command "Show this command's requirements.")))
                                       (t
                                        (let ((result (funcall reader +md-max-input-bytes+)))
                                          (case (first result)
                                            (:octets
                                             (multiple-value-bind (document code message offset) (funcall parser (second result))
                                               (if document
                                                   (aitools.kernel.domain:flow-continue :on-replacement (second result))
                                                   (aitools.kernel.domain:flow-continue :on-error
                                                                                        code
                                                                                        message
                                                                                        :diagnostics
                                                                                        (and offset
                                                                                             (list (json-object "offset" offset "message" message)))
                                                                                        :repairs
                                                                                        (%md-schema-repair command
                                                                                                           "Check replacement UTF-8 Markdown.")))))
                                            (:too-large
                                             (aitools.kernel.domain:flow-continue :on-error
                                                                                  "refusal.too-large"
                                                                                  (format nil
                                                                                          "replacement input exceeds ~D bytes"
                                                                                          +md-max-input-bytes+)
                                                                                  :repairs
                                                                                  (%md-schema-repair command "Use a smaller replacement section.")))
                                            (:failure
                                             (aitools.kernel.domain:flow-continue :on-error
                                                                                  "environment.io"
                                                                                  (second result)
                                                                                  :repairs
                                                                                  (%md-schema-repair command "Check standard input.")))))))))

(defun %md-write-plan (ports path section replacement expect-hash)
  (aitools.edit.application:make-write-plan :command
                                            "md.replace"
                                            :targets
                                            (list (aitools.edit.application:make-write-target path))
                                            :inputs
                                            (list replacement)
                                            :max-target-bytes
                                            +md-max-input-bytes+
                                            :guard-requirements
                                            (list (list :expect-hash path))
                                            :expect-hashes
                                            (list expect-hash)
                                            :replayable
                                            nil
                                            :record-options
                                            (lambda (paths)
                                              (declare (ignore paths))
                                              (list :expect-hash expect-hash))
                                            :record-positionals
                                            (lambda (paths)
                                              (list (first paths) (princ-to-string section)))
                                            :plan
                                            (lambda (context &key on-committed on-rejected)
                                              (aitools.kernel.domain:flow-call
                                               (%md-plan-write/k ports context section replacement)
                                               (:on-committed (requests fields)
                                                              (funcall on-committed requests fields))
                                               (:on-rejected (code message &key path diagnostics repairs)
                                                             (funcall on-rejected code message
                                                                      :path path
                                                                      :diagnostics diagnostics
                                                                      :repairs repairs))))))

(aitools.kernel.domain:define-flow (md-replace/k ports path section &key root lock-timeout dry-run tx display-argv expect-hash stdin on-ok on-partial on-error)
                                   (declare (type function on-ok on-partial on-error))
                                   (let ((number (%md-section-number section)))
                                     (cond
                                       ((or (not (stringp path)) (zerop (length path)))
                                        (aitools.kernel.domain:flow-continue :on-error
                                                                             "argument.invalid"
                                                                             "a Markdown file path is required"
                                                                             :repairs
                                                                             (%md-schema-repair "md replace" "Pass a Markdown file path.")))
                                       ((not stdin)
                                        (aitools.kernel.domain:flow-continue :on-error
                                                                             "argument.invalid"
                                                                             "md replace requires --stdin"
                                                                             :repairs
                                                                             (%md-schema-repair "md replace"
                                                                                                "Pass --stdin with a complete replacement section.")))
                                       ((or (null number) (not (stringp expect-hash)) (zerop (length expect-hash)))
                                        (aitools.kernel.domain:flow-continue :on-error
                                                                             "argument.invalid"
                                                                             (if number
                                                                                 "md replace needs --expect-hash"
                                                                                 "section must be a positive heading ordinal")
                                                                             :repairs
                                                                             (%md-schema-repair "md replace"
                                                                                                "Pass a positive section ordinal and --expect-hash HASH.")))
                                       ((or (null (md-ports-workspace-host ports)) (null (md-ports-open-store ports)))
                                        (aitools.kernel.domain:flow-continue :on-error
                                                                             "internal.unexpected"
                                                                             "the md write ports are unavailable"
                                                                             :repairs
                                                                             (%md-schema-repair "md replace" "Show this command's requirements.")))
                                       (t
                                        (aitools.kernel.domain:flow-call (%md-read-replacement/k ports "md replace")
                                                                         (:on-replacement (replacement)
                                                                                          (let ((outcome nil))
                                                                                            (aitools.edit.application:run-write-command/k
                                                                                             (aitools.edit.application:make-write-edit-ports
                                                                                              :workspace-host
                                                                                              (md-ports-workspace-host ports)
                                                                                              :open-store
                                                                                              (md-ports-open-store ports))
                                                                                             (%md-write-plan ports
                                                                                                             path
                                                                                                             number
                                                                                                             replacement
                                                                                                             expect-hash)
                                                                                             :root
                                                                                             root
                                                                                             :lock-timeout
                                                                                             lock-timeout
                                                                                             :dry-run
                                                                                             dry-run
                                                                                             :tx
                                                                                             tx
                                                                                             :display-argv
                                                                                             display-argv
                                                                                             :on-ok
                                                                                             (lambda (fields)
                                                                                               (setf outcome (list :ok fields)))
                                                                                             :on-error
                                                                                             (lambda
                                                                                                 (code message
                                                                                                       &key
                                                                                                       candidates
                                                                                                       diagnostics
                                                                                                       conflicts
                                                                                                       repairs)
                                                                                               (setf outcome (list :error
                                                                                                                   code
                                                                                                                   message
                                                                                                                   candidates
                                                                                                                   diagnostics
                                                                                                                   conflicts
                                                                                                                   repairs))))
                                                                                            (ecase (first outcome)
                                                                                              (:ok
                                                                                               (aitools.kernel.domain:flow-continue :on-ok
                                                                                                                                    (second outcome)))
                                                                                              (:error
                                                                                               (aitools.kernel.domain:flow-continue :on-error
                                                                                                                                    (second outcome)
                                                                                                                                    (third outcome)
                                                                                                                                    :candidates
                                                                                                                                    (fourth outcome)
                                                                                                                                    :diagnostics
                                                                                                                                    (fifth outcome)
                                                                                                                                    :conflicts
                                                                                                                                    (sixth outcome)
                                                                                                                                    :repairs
                                                                                                                                    (seventh outcome))))))
                                                                         (:on-error (code message &key diagnostics repairs)
                                                                                    (aitools.kernel.domain:flow-continue :on-error
                                                                                                                         code
                                                                                                                         message
                                                                                                                         :diagnostics
                                                                                                                         diagnostics
                                                                                                                         :repairs
                                                                                                                         repairs)))))))
