(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      (quote (:context "util"
              :symbol-allowances
              (("util" (:composition) "CL-BOUNDARY-KIT" "MAKE-BOUNDARY-CONTEXT"
                "Constructs the util boundary context in the composition factory.")
               ("util" (:composition) "CL-BOUNDARY-KIT" "BOUNDARY-CONTEXT-WITH"
                "Registers util capabilities in the composition boundary context.")))))
