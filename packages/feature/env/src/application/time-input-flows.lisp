(in-package #:aitools.env.application)

(aitools.kernel.domain:define-flow
    (%parse-durations/k texts &key on-ok on-error)
  "Call ON-OK with the summed milliseconds, or ON-ERROR for the first invalid duration."
  (let ((total 0))
    (dolist (text texts
             (aitools.kernel.domain:flow-continue :on-ok total))
      (handler-case
          (incf total
                (aitools.kernel.domain:duration-milliseconds
                 (aitools.kernel.domain:parse-duration text)))
        (aitools.kernel.domain:invalid-duration-error ()
          (return
            (aitools.kernel.domain:flow-continue
             :on-error
             "argument.invalid"
             (format nil
                     "not a duration: ~S (use <number>ms|s|m|h|d; negative values go in --sub)"
                     text)
             :repairs
             (list (repair "fix-duration"
                           "Durations are <number>ms|s|m|h|d."
                           "aitools time convert now --add 1h")))))))))

(aitools.kernel.domain:define-flow
    (%parse-instant/k ports text zone-name example &key on-instant on-error)
  "Resolve TEXT to epoch milliseconds, reading offset-less input in ZONE-NAME."
  (if (string-equal text "now")
      (aitools.kernel.domain:flow-continue :on-instant (%now ports) nil)
      (multiple-value-bind (input syntax-condition)
          (handler-case
              (values (aitools.env.domain:parse-time-input text) nil)
            (aitools.env.domain:time-syntax-error (condition)
              (values nil condition)))
        (if syntax-condition
            (aitools.kernel.domain:flow-continue
             :on-error
             "input.syntax-error"
             (princ-to-string syntax-condition)
             :repairs
             (list (repair "fix-value"
                           (concatenate 'string
                                        "Pass ISO 8601 (2026-03-08T01:30:00, optionally with Z or +09:00) "
                                        "or Unix epoch seconds/milliseconds.")
                           example)))
            (if (eq (aitools.env.domain:time-input-kind input) :instant)
                (aitools.kernel.domain:flow-continue
                 :on-instant
                 (aitools.env.domain:time-input-milliseconds input)
                 input)
                (aitools.kernel.domain:flow-call
                    (%with-zone/k ports zone-name)
                  (:on-zone (zone)
                    (aitools.kernel.domain:flow-continue
                     :on-instant
                     (aitools.env.domain:zone-local-to-epoch-milliseconds
                      zone (aitools.env.domain:time-input-milliseconds input))
                     input))
                  (:on-error (code message &key repairs)
                    (aitools.kernel.domain:flow-continue
                     :on-error code message :repairs repairs))))))))
