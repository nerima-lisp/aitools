(in-package #:asdf-user)

(defsystem "aitools/feature/env"
  :description "Environment and time features."
  :depends-on ("aitools/core/text" "cl-json-kit" "cl-host-kit"
               "cl-process-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "civil-time")
                 (:file "time-input") (:file "posix-tz") (:file "tzif")
                 (:file "host-parsers") (:file "host-values")))
   (:module "application"
    :components ((:file "package") (:file "ports")
                 (:file "time-flows") (:file "sys-flows")))
   (:module "infrastructure"
    :components ((:file "package") (:file "production-ports")))))
