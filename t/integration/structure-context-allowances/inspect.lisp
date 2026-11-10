(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      '(:context "inspect"
        :symbol-allowances
        (("inspect" (:composition) "CL-BOUNDARY-KIT" "MAKE-BOUNDARY-CONTEXT"
          "Creates the inspect boundary context in the composition factory.")
         ("inspect" (:composition) "CL-BOUNDARY-KIT" "BOUNDARY-CONTEXT-WITH"
          "Registers inspect capabilities in the composition boundary context."))))
