;;;; t/unit/protocol/error-catalog-test.lisp
(in-package #:aitools.protocol.test)

(describe "aitools.protocol.domain error-catalog"
  (it "maps selection and refusal codes to exit code 2"
    (dolist (code '("selection.no-match" "selection.ambiguous" "selection.count-mismatch"
                    "refusal.target-changed"))
      (expect (error-code-exit-code code) :to-be 2)))

  (it "maps every documented code to its documented exit code"
    (let ((entries (list (cons "argument.invalid" 1)
                         (cons "input.not-found" 1)
                         (cons "input.not-utf8" 1)
                         (cons "input.unsupported-format" 1)
                         (cons "input.unsupported-language" 1)
                         (cons "input.syntax-error" 1)
                         (cons "selection.no-match" 2)
                         (cons "selection.ambiguous" 2)
                         (cons "selection.count-mismatch" 2)
                         (cons "refusal.target-changed" 2)
                         (cons "refusal.redacted-input" 1)
                         (cons "refusal.outside-workspace" 1)
                         (cons "refusal.exists" 1)
                         (cons "refusal.not-a-file" 1)
                         (cons "refusal.too-large" 1)
                         (cons "environment.io" 1)
                         (cons "environment.busy" 1)
                         (cons "environment.timeout" 1)
                         (cons "environment.unavailable" 1)
                         (cons "internal.unexpected" 1))))
      (expect (mapcar (lambda (entry)
                        (cons (getf entry :code) (getf entry :exit-code)))
                      *protocol-error-codes*)
              :to-equal entries)
      (dolist (entry entries)
        (expect (error-code-exit-code (car entry)) :to-be (cdr entry)))))

  (it "signals for an unknown code"
    (signals error (error-code-exit-code "not.a.real.code"))))
