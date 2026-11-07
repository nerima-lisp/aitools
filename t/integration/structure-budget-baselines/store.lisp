(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      '(:context "store"
        :complex-functions
        (
          ("AITOOLS.STORE.APPLICATION" "%RECOVER-INTENT" 5 "packages/core/store/src/application/recovery.lisp:27"
               (("AITOOLS.STORE.APPLICATION" "packages/core/store/src/application/recovery.lisp" "%RECOVER-INTENT")
                ("AITOOLS.STORE.APPLICATION" "packages/core/store/src/application/recovery.lisp" "%RECOVER-COMPLETE-INTENT")
                ("AITOOLS.STORE.APPLICATION" "packages/core/store/src/application/recovery.lisp" "%DISCARD-INCOMPLETE-STEP")
                ("AITOOLS.STORE.APPLICATION" "packages/core/store/src/application/recovery.lisp" "%DISCARD-INCOMPLETE-INTENT")))
        )
        :generated-exemptions
        nil
))
