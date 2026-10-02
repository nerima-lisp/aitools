(in-package #:asdf-user)

(defsystem "aitools/core/protocol"
  :description "Protocol domain, application, and JSON infrastructure."
  :depends-on ("aitools/core/kernel" "cl-json-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "error-catalog")
                 (:file "redaction") (:file "shell-words")
                 (:file "command-placement") (:file "envelope")
                 (:file "schema-model")))
   (:module "application"
    :components ((:file "package") (:file "command-result")
                 (:file "command-registry") (:file "redaction-flow")
                 (:file "schema-flow") (:file "unknown-name")))
   (:module "infrastructure"
    :components ((:file "package") (:file "json-writer")))))
