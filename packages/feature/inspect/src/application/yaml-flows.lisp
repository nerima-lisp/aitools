(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
  (parse-yaml-write-document/k text
                               &key max-input-length max-depth on-value on-error)
  "Continue with compact JSON text and comment count, or a public error code and message."
  (declare (type function on-value on-error))
  (handler-case
      (multiple-value-bind (value comment-count)
          (parse-yaml-write-document
           text :max-input-length max-input-length :max-depth max-depth)
        (aitools.kernel.domain:flow-continue
          :on-value (render-json value) comment-count))
    (yaml-unsupported-value (condition)
      (aitools.kernel.domain:flow-continue
        :on-error "input.unsupported-value"
        (format nil "YAML value at JSON Pointer ~A uses unsupported ~A."
                (yaml-unsupported-pointer condition)
                (yaml-unsupported-reason condition))))
    (yaml-syntax-error ()
      (aitools.kernel.domain:flow-continue
        :on-error "input.syntax-error" "YAML syntax error."))))
