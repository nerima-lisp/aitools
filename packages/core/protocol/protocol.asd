(in-package #:asdf-user)

(defsystem "aitools/core/protocol"
  :description "Protocol domain, application, and JSON infrastructure."
  :depends-on ("aitools/core/kernel" "cl-json-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "error-catalog")
                 (:file "redaction-patterns") (:file "redaction-pem")
                 (:file "redaction") (:file "shell-words")
                 (:file "command-placement") (:file "envelope")
                 (:file "schema-model")))
   (:module "application"
    :components ((:file "package") (:file "command-result")
                 (:file "command-registry") (:file "redaction-flow")
                 (:file "schema-flow")))
   (:module "infrastructure"
    :components ((:file "package") (:file "json-writer")))))
(defsystem "aitools/test/protocol"
           :description "protocol context tests."
           :depends-on ("aitools/test/base" "aitools/core/protocol")
           :pathname "../../../t/"
           :serial t
           :components ((:file "unit/protocol/package")
                        (:file "unit/protocol/envelope-test")
                        (:file "unit/protocol/error-catalog-test")
                        (:file "unit/protocol/redaction-test")
                        (:file "unit/protocol/command-placement-test")
                        (:file "unit/protocol/redaction-flow-test")
                        (:file "unit/protocol/schema-flow-test")
                        (:file "unit/protocol/command-result-test")
                        (:file "unit/protocol/command-registry-test")
                        (:file "unit/protocol/json-writer-test")
                        (:file "unit/protocol/redaction-edges-test")
                        (:file "unit/protocol/redaction-helpers-test")))
