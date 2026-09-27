;;;; packages/feature/process/src/application/common.lisp
;;;;
;;;; Pieces every process flow shares: repair entries (every error
;;;; carries at least one complete command), duration parsing into the
;;;; `argument.invalid` shape, and the mapping of a port I/O failure.
(in-package #:aitools.process.application)

(defun %repair (action detail command)
  (list :action action :detail detail :command command))

(defun %schema-repair (command-name)
  (%repair "inspect-schema" "Show this command's arguments and rules."
           (aitools.process.domain:command-line "aitools" "schema" command-name)))

(defun %parse-duration-ms (text)
  "TEXT (`<number>ms|s|m|h|d`) in milliseconds, or NIL when it is not a
duration."
  (handler-case (aitools.kernel.domain:duration-milliseconds (aitools.kernel.domain:parse-duration text))
    (aitools.kernel.domain:invalid-duration-error () nil)))

(defun %call-with-duration-ms (text option command-name on-error continuation &key (allow-zero t))
  "Call CONTINUATION with TEXT parsed to milliseconds, or report
`argument.invalid` for OPTION through ON-ERROR."
  (let ((milliseconds (%parse-duration-ms text)))
    (if (and milliseconds (or allow-zero (plusp milliseconds)))
        (funcall continuation milliseconds)
        (funcall on-error "argument.invalid"
                 (format nil "~A expects a ~:[positive ~;~]duration such as 30s or 500ms, got ~S"
                         option allow-zero text)
                 :repairs (list (%schema-repair command-name))))))

(defun %call-with-line-pattern (source command-name on-error continuation)
  "Call CONTINUATION with SOURCE compiled, or with NIL when SOURCE is NIL;
report `input.syntax-error` through ON-ERROR when SOURCE does not compile."
  (if (null source)
      (funcall continuation nil)
      (handler-case (aitools.process.domain:compile-line-pattern source)
        (aitools.process.domain:invalid-line-pattern (condition)
          (funcall on-error "input.syntax-error"
                   (aitools.process.domain:invalid-line-pattern-message condition)
                   :repairs (list (%schema-repair command-name))))
        (:no-error (pattern) (funcall continuation pattern)))))

(defmacro %reporting-port-errors ((on-error command-name) &body body)
  "Run BODY, reporting a PROCESS-PORT-ERROR or a pattern that fails while
matching through ON-ERROR instead of letting it escape the flow."
  (let ((condition (gensym "CONDITION")))
    `(handler-case (progn ,@body)
       (process-port-error (,condition)
         (funcall ,on-error "environment.io" (process-port-error-message ,condition)
                  :repairs (list (%schema-repair ,command-name))))
       (aitools.process.domain:invalid-line-pattern (,condition)
         (funcall ,on-error "input.syntax-error"
                  (aitools.process.domain:invalid-line-pattern-message ,condition)
                  :repairs (list (%schema-repair ,command-name)))))))

(defun %unavailable-program-error (on-error message program)
  (funcall on-error "environment.unavailable" message
           :repairs (list (%repair "check-tool" "Check whether the program is installed and on PATH."
                                   (aitools.process.domain:command-line "aitools" "sys" "tools" program)))))
