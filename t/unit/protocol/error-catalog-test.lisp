(in-package #:aitools.protocol.test)

(describe "aitools.protocol.domain error-catalog"
  (it-each (("argument.invalid" 1)
            ("input.not-found" 1)
            ("input.not-utf8" 1)
            ("input.unsupported-format" 1)
            ("input.unsupported-language" 1)
            ("input.syntax-error" 1)
            ("input.unsupported-value" 1)
            ("selection.no-match" 2)
            ("selection.ambiguous" 2)
            ("selection.count-mismatch" 2)
            ("refusal.target-changed" 2)
            ("refusal.redacted-input" 1)
            ("refusal.outside-workspace" 1)
            ("refusal.exists" 1)
            ("refusal.not-a-file" 1)
            ("refusal.too-large" 1)
            ("environment.io" 1)
            ("environment.busy" 1)
            ("environment.timeout" 1)
            ("environment.unavailable" 1)
            ("internal.unexpected" 1))
    "maps ~S to exit code ~D"
    (code exit-code)
    (let ((entry (find code *protocol-error-codes*
                       :key (lambda (item) (getf item :code))
                       :test #'string=)))
      (expect (getf entry :exit-code) :to-be exit-code)
      (expect (error-code-exit-code code) :to-be exit-code)))

  (it "signals for an unknown code"
    (signals error (error-code-exit-code "not.a.real.code"))))
