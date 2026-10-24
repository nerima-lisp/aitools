;;;; TOML write-value validation and representability.
(in-package #:aitools.edit.test)

(defun toml-write-value-refusal (value)
  (handler-case
      (progn (validate-toml-write-value value) nil)
    (edit-refusal (condition) condition)))

(describe "aitools.edit.domain TOML write values"
  (it "checks root, null, and integer constraints"
    (let ((root (toml-write-value-refusal (parse-json-text "[null]")))
          (null-value
            (toml-write-value-refusal (parse-json-text "{\"a\":null}")))
          (large-int
            (toml-write-value-refusal
             (parse-json-text "{\"a\":9223372036854775808}")))
          (bounds
            (parse-json-text
             "{\"min\":-9223372036854775808,\"max\":9223372036854775807}")))
      (expect (edit-refusal-detail root) :to-equal
              "TOML value at JSON Pointer  uses unsupported non-table-root.")
      (expect (edit-refusal-detail null-value) :to-equal
              "TOML value at JSON Pointer /a uses unsupported toml-null.")
      (expect (edit-refusal-code large-int) :to-equal "argument.invalid")
      (expect (edit-refusal-detail large-int) :to-equal
              "TOML integer must fit signed 64-bit.")
      (expect (eq bounds (validate-toml-write-value bounds)) :to-be t)))
  (it "rejects nonzero underflow and overflow while accepting exponent-form zero"
    (let ((underflow
            (toml-write-value-refusal
             (parse-json-text "{\"value\":1e-9999}")))
          (zero (parse-json-text "{\"value\":0e-9999}"))
          (overflow
            (toml-write-value-refusal
             (parse-json-text "{\"value\":1e9999}"))))
      (expect (edit-refusal-code underflow) :to-equal "argument.invalid")
      (expect (eq zero (validate-toml-write-value zero)) :to-be t)
      (expect (edit-refusal-code overflow) :to-equal "argument.invalid")))
  (it "returns supported JSON tables unchanged"
    (let ((value (parse-json-text
                  "{\"first\":1,\"second\":true,\"items\":[\"x\",false]}")))
      (expect (eq (validate-toml-write-value value) value) :to-be t))))
