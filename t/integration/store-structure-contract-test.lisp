(in-package #:aitools.integration.structure-test)

(describe "aitools structure test (store lock kit dependency)"
  (it "allows application boundary objects and keeps retry execution in infrastructure"
    (expect (symbol-allowed-p "store" :application "RESILIENCE-KIT" "MAKE-RETRY-POLICY")
            :to-be-truthy)
    (dolist (layer '(:application :infrastructure))
      (expect (member "CL-BOUNDARY-KIT"
                      (allowed-package-names "store" layer :core)
                      :test #'string=)
              :to-be-truthy))
    (expect (member "CL-BOUNDARY-KIT"
                    (allowed-package-names "store" :domain :core)
                    :test #'string=)
            :to-equal nil)
    (dolist (reference '(("RESILIENCE-KIT" "CALL-WITH-RETRY")
                         ("RESILIENCE-KIT" "RETRY-EXHAUSTED")))
      (destructuring-bind (package symbol) reference
        (expect (symbol-allowed-p "store" :application package symbol)
                :to-equal nil)
        (expect (symbol-allowed-p "store" :domain package symbol)
                :to-equal nil)
        (expect (symbol-allowed-p "store" :infrastructure package symbol)
                :to-be-truthy)
        (expect (symbol-allowed-p "protocol" :application package symbol)
                :to-equal nil)))
    (expect
     (symbol-allowed-p "store" :application "OTHER-KIT" "CALL-WITH-RETRY")
     :to-equal
     nil)
    (expect
     (symbol-allowed-p "store" :application "RESILIENCE-KIT" "UNLISTED-RETRY")
     :to-equal
     nil)))
