(in-package #:aitools.inspect.test)

(describe "ordered JSON comparison type boundaries"
  (it "orders values within a type and leaves mixed types unordered"
    (expect (aitools.inspect.domain::%ordered-compare 1 2) :to-equal -1)
    (expect (aitools.inspect.domain::%ordered-compare 2 1) :to-equal 1)
    (expect (aitools.inspect.domain::%ordered-compare 2 2.0) :to-equal 0)
    (expect (aitools.inspect.domain::%ordered-compare "a" "b") :to-equal -1)
    (expect (aitools.inspect.domain::%ordered-compare "b" "a") :to-equal 1)
    (expect (aitools.inspect.domain::%ordered-compare "a" "a") :to-equal 0)
    (expect (aitools.inspect.domain::%ordered-compare 1 "1") :to-be nil)))
