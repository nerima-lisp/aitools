(in-package #:aitools.edit.application)

(define-write-command "json.set"
                      (ports env positionals options &key on-plan on-error)
                      (let ((path (first positionals)))
                        (flet ((plan (pointer value
                                              record-options
                                              record-positionals
                                              inputs)
                                 (multiple-value-bind (tokens refusal)
                                     (handler-case (values
                                                    (parse-json-pointer pointer)
                                                    nil)
                                       (edit-refusal (condition)
                                         (values nil condition)))
                                   (if refusal
                                       (flow-continue :on-error
                                                      (edit-refusal-code
                                                       refusal)
                                                      (edit-refusal-detail
                                                       refusal))
                                       (progn
                                         (flow-continue :on-plan
                                                        (make-write-plan
                                                         :command
                                                         "json.set"
                                                         :targets
                                                         (list
                                                          (make-write-target
                                                           path))
                                                         :inputs
                                                         inputs
                                                         :expect-hashes
                                                         (getf options
                                                               :expect-hash)
                                                         :replayable
                                                         (null
                                                          (getf options
                                                                :expect-hash))
                                                         :plan
                                                         (%json-plan
                                                          (lambda (document)
                                                            (json-add document
                                                                      tokens
                                                                      value
                                                                      :array-mode
                                                                      :replace))
                                                          :repair-pointer
                                                          tokens)
                                                         :record-options
                                                         record-options
                                                         :record-positionals
                                                         (lambda (paths)
                                                           (list* (first paths)
                                                                  record-positionals)))))))))
                          (cond
                            ((null path)
                             (flow-continue :on-error
                                            "argument.invalid"
                                            "json set needs PATH"))
                            ((or (getf options :stdin)
                                 (getf options :stdin-data))
                             (if (rest positionals)
                                 (flow-continue :on-error
                                                "argument.invalid"
                                                "json set --stdin reads {\"pointer\",\"value\"}; pass only PATH")
                                 (aitools.kernel.domain:flow-call
                                  (read-stdin-json/k ports options)
                                  (:on-error (code message)
                                             (flow-continue :on-error
                                                            code
                                                            message))
                                  (:on-json (input text)
                                            (let ((pointer
                                                   (cdr
                                                    (%json-member input
                                                                  "pointer")))
                                                  (value
                                                   (%json-member input "value")))
                                              (if (and (stringp pointer) value)
                                                  (plan pointer
                                                        (cdr value)
                                                        (inline-stdin-options
                                                         options
                                                         text)
                                                        '()
                                                        (list text))
                                                  (flow-continue :on-error
                                                                 "argument.invalid"
                                                                 (concatenate
                                                                  'string
                                                                  "json set --stdin reads {\"pointer\": "
                                                                  "string, \"value\": any}"))))))))
                            ((/= (length positionals) 3)
                             (flow-continue :on-error
                                            "argument.invalid"
                                            "json set takes PATH POINTER VALUE (or --stdin)"))
                            (t
                             (let ((text (third positionals)))
                               (aitools.kernel.domain:flow-call
                                (parse-json-text/k text)
                                (:on-value (value)
                                           (plan (second positionals)
                                                 value
                                                 options
                                                 (rest positionals)
                                                 (list text)))
                                (:on-invalid (message)
                                             (declare (ignore message))
                                             (flow-continue :on-error
                                                            "argument.invalid"
                                                            (format nil
                                                                    "VALUE ~S is not JSON; a string needs JSON quotes"
                                                                    text)
                                                            :repairs
                                                            (list
                                                             (repair
                                                              "quote-string"
                                                              "Pass the value as a JSON string."
                                                              (command-line
                                                               (list "json"
                                                                     "set"
                                                                     path
                                                                     (second
                                                                      positionals)
                                                                     (json-string-literal
                                                                      text))))))))))))))

(define-write-command "json.delete"
                      (ports env positionals options &key on-plan on-error)
                      (if (/= (length positionals) 2)
                          (flow-continue :on-error
                                         "argument.invalid"
                                         "json delete takes PATH POINTER")
                          (multiple-value-bind (tokens refusal)
                              (handler-case (values
                                             (parse-json-pointer
                                              (second positionals))
                                             nil)
                                (edit-refusal (condition)
                                  (values nil condition)))
                            (if refusal
                                (flow-continue :on-error
                                               (edit-refusal-code refusal)
                                               (edit-refusal-detail refusal))
                                (progn
                                  (flow-continue :on-plan
                                                 (make-write-plan :command
                                                                  "json.delete"
                                                                  :targets
                                                                  (list
                                                                   (make-write-target
                                                                    (first
                                                                     positionals)))
                                                                  :expect-hashes
                                                                  (getf options
                                                                        :expect-hash)
                                                                  :replayable
                                                                  (null
                                                                   (getf options
                                                                         :expect-hash))
                                                                  :plan
                                                                  (%json-plan
                                                                   (lambda
                                                                       (document)
                                                                     (json-remove
                                                                      document
                                                                      tokens))
                                                                   :repair-pointer
                                                                   tokens)
                                                                  :record-options
                                                                  options
                                                                  :record-positionals
                                                                  (lambda
                                                                      (paths)
                                                                    (list
                                                                     (first
                                                                      paths)
                                                                     (second
                                                                      positionals))))))))))
