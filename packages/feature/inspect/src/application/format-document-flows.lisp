(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (%yaml-document context target action &key on-document on-error)
 (declare (type function on-document on-error))
 (labels ((report-error (code message
                              &key candidates diagnostics conflicts repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates :diagnostics diagnostics
             :conflicts conflicts :repairs repairs))
          (repair-command (kind detail)
            (list (repair kind detail
                          (command-line context (list "schema" "yaml" action) :tx :none)))))
   (multiple-value-bind (octets problem)
       (read-target-octets context target)
     (if (null octets)
         (fail-target-read context target #'report-error problem)
         (handler-case
             (aitools.kernel.domain:flow-continue
              :on-document
              (parse-yaml-document (%join-lines (decode-text-lines octets))))
           (yaml-unsupported-value (condition)
             (report-error
              "input.unsupported-value"
              (format nil "YAML value at JSON Pointer ~A uses unsupported ~A."
                      (yaml-unsupported-pointer condition)
                      (yaml-unsupported-reason condition))
              :repairs
              (repair-command
               "convert-to-supported-value"
               "Convert the value to the supported subset, then retry.")))
           (yaml-syntax-error ()
             (report-error
              "input.syntax-error" "YAML syntax error."
              :repairs
              (repair-command
               "fix-format-syntax"
               "Fix the YAML/TOML syntax or TOML integer range, then retry."))))))))

(aitools.kernel.domain:define-flow
 (%toml-document context target action &key on-document on-error)
 (declare (type function on-document on-error))
 (labels ((report-error (code message
                              &key candidates diagnostics conflicts repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates :diagnostics diagnostics
             :conflicts conflicts :repairs repairs))
          (repair-command (kind detail)
            (list (repair kind detail
                          (command-line context (list "schema" "toml" action) :tx :none)))))
   (multiple-value-bind (octets problem)
       (read-target-octets context target)
     (if (null octets)
         (fail-target-read context target #'report-error problem)
         (handler-case
             (aitools.kernel.domain:flow-continue
              :on-document
              (parse-toml-document (%join-lines (decode-text-lines octets))))
           (toml-unsupported-value (condition)
             (report-error
              "input.unsupported-value"
              (format nil "TOML value at JSON Pointer ~A uses unsupported ~A."
                      (toml-unsupported-pointer condition)
                      (toml-unsupported-reason condition))
              :repairs
              (repair-command
               "convert-to-supported-value"
               "Convert the value to the supported subset, then retry.")))
           (toml-syntax-error ()
             (report-error
              "input.syntax-error" "TOML syntax error."
              :repairs
              (repair-command
               "fix-format-syntax"
               "Fix the YAML/TOML syntax or TOML integer range, then retry."))))))))
