;;;; t/unit/kernel/token-estimate-test.lisp
(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain approx-token-count"
  (it "rounds up to the next whole token"
    (expect (approx-token-count 1) :to-be 1)
    (expect (approx-token-count 4) :to-be 1)
    (expect (approx-token-count 5) :to-be 2))

  (it "returns zero for zero characters"
    (expect (approx-token-count 0) :to-be 0))

  (it "scales roughly linearly with character count"
    (expect (approx-token-count 400) :to-be 100)))
