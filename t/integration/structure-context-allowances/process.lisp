(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      (quote (:context "process"
              :symbol-allowances
              (("process" (:composition) "CL-BOUNDARY-KIT" "MAKE-BOUNDARY-CONTEXT"
                "Construct the process boundary context in the composition factory.")
               ("process" (:composition) "CL-BOUNDARY-KIT" "BOUNDARY-CONTEXT-WITH"
                "Register production process capabilities in the boundary context.")))))
