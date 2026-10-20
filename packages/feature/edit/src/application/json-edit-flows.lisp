(in-package #:aitools.edit.application)

(define-write-command "json.set"
                      (ports env positionals options on-plan fail)
                      (let ((path (first positionals)))
                      (flet ((plan (pointer value
                                            record-options
                                            record-positionals
                                            inputs)
                             (let ((tokens
                                    (handler-case (parse-json-pointer pointer)
                                    (edit-refusal (condition)
                                    (return-from plan
                                    (funcall fail
                                             (edit-refusal-code condition)
                                             (edit-refusal-detail condition)))))))
                             (funcall on-plan
                                      (make-write-plan :command
                                                       "json.set"
                                                       :targets
                                                       (list
                                                        (make-write-target path))
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
                                                              record-positionals)))))))
                      (cond
                      ((null path)
                       (funcall fail "argument.invalid" "json set needs PATH"))
                      ((or (getf options :stdin) (getf options :stdin-data))
                       (if (rest positionals)
                       (funcall fail
                                "argument.invalid"
                                "json set --stdin reads {\"pointer\",\"value\"}; pass only PATH")
                       (read-stdin-json/k ports
                                          options
                                          :on-error
                                          fail
                                          :on-json
                                          (lambda (input text)
                                          (let ((pointer
                                                 (cdr
                                                  (%json-member input "pointer")))
                                                (value
                                                 (%json-member input "value")))
                                          (if (and (stringp pointer) value)
                                          (plan pointer
                                                (cdr value)
                                                (inline-stdin-options options
                                                                      text)
                                                '()
                                                (list text))
                                          (funcall fail
                                                   "argument.invalid"
                                                   (concatenate 'string
                                                                "json set --stdin reads "
                                                                "{\"pointer\": string, "
                                                                "\"value\": any}"))))))))
                      ((/= (length positionals) 3)
                       (funcall fail
                                "argument.invalid"
                                "json set takes PATH POINTER VALUE (or --stdin)"))
                      (t
                       (let* ((text (third positionals))
                              (value
                               (parse-json-text/k text
                                                  :on-value
                                                  #'identity
                                                  :on-invalid
                                                  (lambda (message)
                                                  (declare (ignore message))
                                                  (return-from prepare
                                                  (funcall fail
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
                                                                     text)))))))))))
                       (plan (second positionals)
                             value
                             options
                             (rest positionals)
                             (list text))))))))


(define-write-command "json.delete" (ports env positionals options on-plan fail)
  (if (/= (length positionals) 2)
      (funcall fail "argument.invalid" "json delete takes PATH POINTER")
      (let ((tokens (handler-case (parse-json-pointer (second positionals))
                      (edit-refusal (condition)
                        (return-from prepare
                          (funcall fail (edit-refusal-code condition) (edit-refusal-detail condition)))))))
        (funcall on-plan
                 (make-write-plan
                  :command "json.delete"
                  :targets (list (make-write-target (first positionals)))
                  :expect-hashes (getf options :expect-hash)
                  :replayable (null (getf options :expect-hash))
                  :plan (%json-plan (lambda (document) (json-remove document tokens)) :repair-pointer tokens)
                  :record-options options
                  :record-positionals (lambda (paths) (list (first paths) (second positionals))))))))
