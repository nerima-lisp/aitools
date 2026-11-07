(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
  (parse-toml-write-document/k text &key max-depth on-value on-error)
  "Continue with compact JSON text and comment count, or a public error code and message."
  (declare (type function on-value on-error))
  (handler-case
      (multiple-value-bind (value comment-count)
          (parse-toml-write-document text :max-depth max-depth)
        (aitools.kernel.domain:flow-continue
          :on-value (render-json value) comment-count))
    (toml-unsupported-value (condition)
      (aitools.kernel.domain:flow-continue
        :on-error "input.unsupported-value"
        (format nil "TOML value at JSON Pointer ~A uses unsupported ~A."
                (toml-unsupported-pointer condition)
                (toml-unsupported-reason condition))))
    (toml-syntax-error ()
      (aitools.kernel.domain:flow-continue
        :on-error "input.syntax-error" "TOML syntax error."))))
