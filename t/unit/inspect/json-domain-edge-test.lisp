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

(describe "JSON model array representation"
  (it "keeps vectors when replacing nested values, inserting, and removing"
    (let* ((model aitools.inspect.domain::*json-value-model*)
           (replaced (aitools.kernel.domain:json-model-replace
                      model #(#("a")) '("0" "0") "b"))
           (inserted (aitools.kernel.domain:json-model-add
                      model #("a" "b") '("1") "middle"))
           (removed (aitools.kernel.domain:json-model-remove
                     model #("a" "middle" "b") '("1"))))
      (expect replaced :to-equalp #(#("b")))
      (expect inserted :to-equalp #("a" "middle" "b"))
      (expect removed :to-equalp #("a" "b"))))

  (it "classifies a non-JSON Lisp value as unknown"
    (multiple-value-bind (kind payload)
        (aitools.inspect.domain::%classify-json :not-json)
      (expect kind :to-be :unknown)
      (expect payload :to-be :not-json))))
