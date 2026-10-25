;;;;
;;;; Pieces every process flow shares: repair entries (every error
;;;; carries at least one complete command), duration parsing into the
;;;; `argument.invalid` shape, and the mapping of a port I/O failure.
(in-package #:aitools.process.application)

(defun %schema-repair (command-name)
  (schema-repair
   (aitools.protocol.domain:command-line "aitools" "schema" command-name)))

(defun %parse-duration-ms (text)
  "TEXT (`<number>ms|s|m|h|d`) in milliseconds, or NIL when it is not a
duration."
  (handler-case (aitools.kernel.domain:duration-milliseconds (aitools.kernel.domain:parse-duration text))
    (aitools.kernel.domain:invalid-duration-error () nil)))

(aitools.kernel.domain:define-flow
    (%call-with-duration-ms text option command-name
                            &key (allow-zero t) on-value on-error)
  "Call ON-VALUE with TEXT parsed to milliseconds, or report `argument.invalid` through ON-ERROR."
  (let ((milliseconds (%parse-duration-ms text)))
    (if (and milliseconds (or allow-zero (plusp milliseconds)))
        (aitools.kernel.domain:flow-continue :on-value milliseconds)
        (aitools.kernel.domain:flow-continue
         :on-error
         "argument.invalid"
         (format nil
                 "~A expects a ~:[positive ~;~]duration such as 30s or 500ms, got ~S"
                 option
                 allow-zero
                 text)
         :repairs (list (%schema-repair command-name))))))

(aitools.kernel.domain:define-flow
    (%call-with-line-pattern source command-name &key on-pattern on-error)
  "Compile SOURCE and call ON-PATTERN, or report `input.syntax-error` through ON-ERROR."
  (if (null source)
      (aitools.kernel.domain:flow-continue :on-pattern nil)
      (handler-case (aitools.process.domain:compile-line-pattern source)
        (aitools.process.domain:invalid-line-pattern (condition)
          (aitools.kernel.domain:flow-continue
           :on-error
           "input.syntax-error"
           (aitools.process.domain:invalid-line-pattern-message condition)
           :repairs (list (%schema-repair command-name))))
        (:no-error (pattern)
          (aitools.kernel.domain:flow-continue :on-pattern pattern)))))

(defmacro %reporting-port-errors (command-name &body body)
  "Run BODY, reporting a PROCESS-PORT-ERROR or a pattern that fails while
matching through ON-ERROR instead of letting it escape the flow."
  (let ((condition (gensym "CONDITION")))
    `(handler-case (progn ,@body)
       (process-target-changed (,condition)
         (aitools.kernel.domain:flow-continue
          :on-error "refusal.target-changed" (process-port-error-message ,condition)
          :repairs (list (%schema-repair ,command-name))))
       (process-port-error (,condition)
         (aitools.kernel.domain:flow-continue
          :on-error "environment.io" (process-port-error-message ,condition)
          :repairs (list (%schema-repair ,command-name))))
       (aitools.process.domain:invalid-line-pattern (,condition)
         (aitools.kernel.domain:flow-continue
          :on-error "input.syntax-error"
          (aitools.process.domain:invalid-line-pattern-message ,condition)
          :repairs (list (%schema-repair ,command-name)))))))

(aitools.kernel.domain:define-flow
    (%unavailable-program-error message program &key on-error)
  (aitools.kernel.domain:flow-continue
   :on-error
   "environment.unavailable"
   message
   :repairs
   (list
    (repair "check-tool"
            "Check whether the program is installed and on PATH."
            (aitools.protocol.domain:command-line
             "aitools" "sys" "tools" program)))))
