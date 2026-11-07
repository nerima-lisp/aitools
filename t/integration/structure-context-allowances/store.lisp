(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      '(:context "store"
        :symbol-allowances
        (
          ("store" (:application)
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
                           "The infrastructure adapter maps exhaustion to no acquired lock.")
        )
))
