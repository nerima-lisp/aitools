(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (json-select-flow ports
                   path
                   pointer
                   &key
                   root
                   tx
                   lock-timeout
                   where
                   pick
                   sort-by
                   desc
                   (output "items")
                   (limit 50)
                   on-ok
                   on-partial
                   on-error)
 "json select. WHERE and PICK are lists of texts. Calls ON-OK or ON-PARTIAL
(fields), or ON-ERROR."
 (declare (type function on-ok on-partial on-error))
 (labels ((report-error (code message
                              &key
                              candidates
                              diagnostics
                              conflicts
                              repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates
             :diagnostics diagnostics
             :conflicts conflicts
             :repairs repairs)))
   (let ((bad-pointer
          (find-if
           (lambda (text)
             (eq (parse-json-pointer text) :invalid))
           (append pick
                   (when sort-by
                     (list sort-by))))))
     (if bad-pointer
         (aitools.kernel.domain:flow-continue
          :on-error
          "argument.invalid"
          (format nil "~S is not a relative JSON pointer" bad-pointer)
          :repairs
          (list
           (repair
            "pointer-syntax"
            "Write members as /name or /a/b."
            "aitools schema json select")))
         (aitools.kernel.domain:flow-call
          (%pointer-tokens pointer)
          (:on-tokens (tokens)
            (aitools.kernel.domain:flow-call
             (%where-comparisons where)
             (:on-comparisons (comparisons)
               (aitools.kernel.domain:flow-call
                (%call-with-json-file
                 ports path
                 :root root
                 :tx tx
                 :lock-timeout lock-timeout
                 :record t)
                (:on-document (context target document)
                  (aitools.kernel.domain:flow-call
                   (%resolve-or-fail context target document tokens)
                   (:on-found (array)
                     (if (not (json-array-value-p array))
                         (aitools.kernel.domain:flow-continue
                          :on-error
                          "argument.invalid"
                          (format nil "~S in ~A is a ~A, not an array"
                                  pointer
                                  path
                                  (json-type-name array))
                          :repairs
                          (list
                           (repair
                            "list-keys"
                            "Find the array to select from."
                            (command-line
                             context
                             (list "json" "get" path pointer "--keys"))))
                         (let ((indexes nil)
                               (limit-message nil))
                           (aitools.kernel.domain:flow-call
                            (call-with-regex-limit/k
                             (lambda ()
                               (setf indexes
                                     (%select-indexes
                                      array
                                      comparisons
                                      (and sort-by
                                           (parse-json-pointer sort-by))
                                      desc))))
                            (:on-limit (message)
                              (setf limit-message message)))
                           (if limit-message
                               (aitools.kernel.domain:flow-continue
                                :on-error
                                "input.syntax-error"
                                (format nil
                                        (concatenate
                                         'string
                                         "a --where ~~ pattern is too "
                                         "complex to evaluate: ~A")
                                        limit-message)
                                :repairs
                                (list
                                 (repair
                                  "fix-pattern"
                                  "Simplify the regular expression."
                                  "aitools schema json select")))
                               (multiple-value-bind (fields truncated)
                                   (%select-fields
                                    context target pointer array indexes pick
                                    output limit :where where)
                                 (if truncated
                                     (aitools.kernel.domain:flow-continue
                                      :on-partial
                                      fields)
                                     (aitools.kernel.domain:flow-continue
                                      :on-ok
                                      fields))))))
                   (:on-error (code message
                                    &key candidates diagnostics
                                         conflicts repairs)
                     (report-error code message
                                   :candidates candidates
                                   :diagnostics diagnostics
                                   :conflicts conflicts
                                   :repairs repairs))))
                (:on-error (code message
                                 &key candidates diagnostics conflicts repairs)
                  (report-error code message
                                :candidates candidates
                                :diagnostics diagnostics
                                :conflicts conflicts
                                :repairs repairs))))
             (:on-error (code message
                              &key candidates diagnostics conflicts repairs)
               (report-error code message
                             :candidates candidates
                             :diagnostics diagnostics
                             :conflicts conflicts
                             :repairs repairs))))
          (:on-error (code message
                           &key candidates diagnostics conflicts repairs)
            (report-error code message
                          :candidates candidates
                          :diagnostics diagnostics
                          :conflicts conflicts
                          :repairs repairs)))))))))
