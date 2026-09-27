;;;; t/unit/protocol/error-catalog-test.lisp
(in-package #:aitools.protocol.test)

(describe "aitools.protocol.domain error-catalog"
  (it "knows every documented error.code"
    (dolist (code '("argument.invalid" "input.not-found" "input.not-utf8"
                    "input.unsupported-format" "input.unsupported-language" "input.syntax-error"
                    "selection.no-match" "selection.ambiguous" "selection.count-mismatch"
                    "refusal.target-changed" "refusal.redacted-input" "refusal.outside-workspace"
                    "refusal.exists" "refusal.not-a-file" "refusal.too-large"
                    "environment.io" "environment.busy" "environment.timeout"
                    "environment.unavailable" "internal.unexpected"))
      (expect (error-code-known-p code) :to-be-truthy)))

  (it "does not know a made-up code"
    (expect (error-code-known-p "not.a.real.code") :to-be-falsy))

  (it "maps selection and refusal codes to exit code 2"
    (dolist (code '("selection.no-match" "selection.ambiguous" "selection.count-mismatch"
                    "refusal.target-changed"))
      (expect (error-code-exit-code code) :to-be 2)))

  (it "maps every other known code to exit code 1"
    (expect (error-code-exit-code "argument.invalid") :to-be 1)
    (expect (error-code-exit-code "environment.busy") :to-be 1)
    (expect (error-code-exit-code "internal.unexpected") :to-be 1))

  (it "signals for an unknown code"
    (signals error (error-code-exit-code "not.a.real.code"))))
