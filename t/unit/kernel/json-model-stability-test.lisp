(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain JSON model callback behavior"
  (it "reports a missing path when classification changes after validation"
    (let* ((root-checks 0)
           (root (test-json-object "child" (test-json-object "leaf" "old")))
           (model
             (test-json-value-model
              :object-p
              (lambda (value)
                (and (test-json-object-p value)
                     (or (not (eq value root))
                         (= (incf root-checks) 1)))))))
      (expect
       (model-error-message
        (lambda ()
          (json-model-replace model root (quote ("child")) "new")))
       :to-equal "no value at JSON pointer \"/child\""))))
