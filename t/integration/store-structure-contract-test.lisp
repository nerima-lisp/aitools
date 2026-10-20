(in-package #:aitools.integration.structure-test)

(setf *context-symbol-allowances*
      (append *context-symbol-allowances*
              '(("store" (:application)
                 "RESILIENCE-KIT"
                 "MAKE-RETRY-POLICY"
                 "Lock contention uses resilience-kit retry-policy settings.")
                ("store" (:infrastructure)
                 "RESILIENCE-KIT"
                 "CALL-WITH-RETRY"
                 "The infrastructure adapter runs the retry with injected boundaries.")
                ("store" (:infrastructure)
                 "RESILIENCE-KIT"
                 "RETRY-EXHAUSTED"
                 "The infrastructure adapter maps exhaustion to no acquired lock."))))

(describe "aitools structure test (store lock kit dependency)"
  (it "keeps retry policy in application and kit boundaries in infrastructure"
    (expect (symbol-allowed-p "store" :application "RESILIENCE-KIT" "MAKE-RETRY-POLICY") :to-be-truthy)
    (dolist (reference '(("RESILIENCE-KIT" "CALL-WITH-RETRY")
                         ("RESILIENCE-KIT" "RETRY-EXHAUSTED")
                         ("CL-BOUNDARY-KIT" "MAKE-CLOCK")
                         ("CL-BOUNDARY-KIT" "MAKE-SLEEPER")))
      (destructuring-bind (package symbol) reference
        (expect (symbol-allowed-p "store" :application package symbol)
                :to-equal
                nil)
        (expect (symbol-allowed-p "store" :domain package symbol)
                :to-equal
                nil)))
    (dolist (reference '(("RESILIENCE-KIT" "CALL-WITH-RETRY")
                         ("RESILIENCE-KIT" "RETRY-EXHAUSTED")))
      (destructuring-bind (package symbol) reference
        (expect (symbol-allowed-p "store" :infrastructure package symbol) :to-be-truthy)
        (expect (symbol-allowed-p "store" :domain package symbol)
                :to-equal
                nil)
        (expect (symbol-allowed-p "protocol" :application package symbol)
                :to-equal
                nil)))
    (expect
     (symbol-allowed-p "store" :application "OTHER-KIT" "CALL-WITH-RETRY")
     :to-equal
     nil)
    (expect
     (symbol-allowed-p "store" :application "RESILIENCE-KIT" "UNLISTED-RETRY")
     :to-equal
     nil)))
