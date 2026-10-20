(in-package #:asdf-user)

(defsystem "aitools/feature/org"
  :description "Read-only Org document commands."
  :depends-on ("aitools/core/kernel" "aitools/core/protocol"
               "aitools/core/text" "aitools/core/workspace"
               "cl-org-kit" "cl-date-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "document")))
   (:module "application"
    :components ((:file "package") (:file "ports") (:file "read-flow")
                 (:file "outline-flow") (:file "query-flow")
                 (:file "export-flow")))
   (:module "infrastructure"
    :components ((:file "package") (:file "org-kit-ports")
                 (:file "time-query-ports") (:file "ports")))))
