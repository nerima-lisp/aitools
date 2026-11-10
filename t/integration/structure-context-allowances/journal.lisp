(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      '(:context "journal"
        :symbol-allowances
        (("journal" (:composition) "CL-BOUNDARY-KIT" "MAKE-BOUNDARY-CONTEXT"
          "Construct the journal boundary context in the composition factory.")
         ("journal" (:composition) "CL-BOUNDARY-KIT" "BOUNDARY-CONTEXT-WITH"
          "Register journal capabilities in the composition boundary context."))))
