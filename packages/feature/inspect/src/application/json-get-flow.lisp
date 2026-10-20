(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (json-get-flow ports
                path
                pointer
                &key
                root
                tx
                lock-timeout
                (max-bytes "16KiB")
                keys
                raw
                on-ok
                on-partial
                on-error)
 "json get. Calls ON-OK or ON-PARTIAL (fields), or ON-ERROR."
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
   (aitools.kernel.domain:flow-call
    (%max-bytes max-bytes)
    (:on-bytes (limit)
      (aitools.kernel.domain:flow-call
       (%pointer-tokens pointer)
       (:on-tokens (tokens)
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
             (:on-found (value)
               (if (and keys
                        (not (member (json-type-name value)
                                     '("object" "array")
                                     :test #'string=)))
                   (aitools.kernel.domain:flow-continue
                    :on-error
                    "argument.invalid"
                    (format nil
                            (concatenate 'string
                                         "--keys needs an object or array; ~A is "
                                         "a ~A")
                            pointer
                            (json-type-name value))
                    :repairs
                    (list
                     (repair
                      "get-value"
                      "Get the value itself."
                      (command-line
                       context
                       (list "json" "get" path pointer)))))
                   (multiple-value-bind (fields truncated)
                       (%json-get-fields
                        context target pointer value keys raw limit)
                     (if truncated
                         (aitools.kernel.domain:flow-continue
                          :on-partial
                          fields)
                         (aitools.kernel.domain:flow-continue
                          :on-ok
                          fields)))))
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
                       :repairs repairs))))
    (:on-error (code message
                     &key candidates diagnostics conflicts repairs)
      (report-error code message
                    :candidates candidates
                    :diagnostics diagnostics
                    :conflicts conflicts
                    :repairs repairs)))))
