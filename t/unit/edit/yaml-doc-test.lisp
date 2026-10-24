;;;; YAML write-value validation and number representability.
(in-package #:aitools.edit.test)

(defun yaml-write-value-refusal (value)
  (handler-case
      (progn (validate-yaml-write-value value) nil)
    (edit-refusal (condition) condition)))

(describe "aitools.edit.domain YAML write values"
  (it "accepts JSON values without changing them"
    (let ((value (parse-json-text "{\"b\":[true,false],\"a\":null}")))
      (expect (eq (validate-yaml-write-value value) value) :to-be t)))
  (it "rejects nonzero mantissa underflow and accepts exponent-form zero"
    (let ((underflow (yaml-write-value-refusal
                      (parse-json-text "{\"value\":1e-9999}")))
          (zero (parse-json-text "{\"value\":0e-9999}")))
      (expect (edit-refusal-code underflow) :to-equal "argument.invalid")
      (expect (eq (validate-yaml-write-value zero) zero) :to-be t)))
  (it "rejects unrepresentable JSON numbers"
    (let ((overflow (yaml-write-value-refusal
                     (parse-json-text "{\"value\":1e9999}"))))
      (expect (edit-refusal-code overflow) :to-equal "argument.invalid")))
  (it "rejects malformed JSON number tokens"
    (let ((refusal
            (yaml-write-value-refusal (make-json-num "1 trailing"))))
      (expect (edit-refusal-code refusal) :to-equal "argument.invalid"))))
