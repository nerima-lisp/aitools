(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      '(:context "edit"
        :symbol-allowances
        (
          ("edit" (:infrastructure) "AITOOLS.KERNEL.DOMAIN" "UNIVERSAL-TIME-TO-UNIX-SECONDS" "pure Unix-time conversion for the edit adapter")
          ("edit" (:infrastructure) "YAML-KIT" "MAKE-YAML-MAPPING" "convert ordered JSON objects to YAML mappings")
          ("edit" (:infrastructure) "YAML-KIT" "+YAML-NULL+" "map JSON null values to the YAML null sentinel")
          ("edit" (:infrastructure) "YAML-KIT" "+YAML-FALSE+" "map JSON false values to the YAML false sentinel")
          ("edit" (:infrastructure) "YAML-KIT" "EMIT" "serialize the YAML document")
          ("edit" (:infrastructure) "CL-TOML-KIT" "+TOML-FALSE+" "map JSON false values to the TOML false sentinel")
          ("edit" (:infrastructure) "CL-TOML-KIT" "ENCODE" "serialize the TOML document")
          ("edit" (:infrastructure) "CL-TOML-KIT" "*TOML-MAX-NESTING-DEPTH*" "enforce the application nesting limit during TOML encoding")
          ("edit" (:composition) "CL-BOUNDARY-KIT" "MAKE-BOUNDARY-CONTEXT" "create the edit boundary context in composition")
          ("edit" (:composition) "CL-BOUNDARY-KIT" "BOUNDARY-CONTEXT-WITH" "register edit-specific capabilities")
        )
))
