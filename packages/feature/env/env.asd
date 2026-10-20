(in-package #:asdf-user)

(defsystem "aitools/feature/env"
  :description "Environment and time features."
  :depends-on ("aitools/core/text" "cl-date-kit" "cl-json-kit"
               "cl-host-kit" "cl-process-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package")
                 (:file "civil-time")
                 (:file "time-input")
                 (:file "time-input-iso8601")
                 (:file "tzif")
                 (:file "host-parsers")
                 (:file "host-process-parsers")
                 (:file "host-port-parsers")
                 (:file "host-values")))
   (:module "application"
    :components ((:file "package")
                 (:file "ports")
                 (:file "time-zone-flows")
                 (:file "time-input-flows")
                 (:file "time-flows")
                 (:file "sys-flows")
                 (:file "sys-tool-flows")
                 (:file "sys-process-flows")
                 (:file "sys-port-flows")
                 (:file "sys-snapshot-flow")))
   (:module "infrastructure"
    :components ((:file "package")
                 (:file "production-ports")))))
