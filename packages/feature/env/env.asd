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
(defsystem "aitools/feature/env/presentation"
  :description "Environment and time CLI commands."
  :depends-on ("aitools/cli/foundation" "aitools/feature/env" "cl-cli")
  :pathname "src/"
  :components
  ((:module "presentation" :serial t
    :components ((:file "package")
                 (:file "env-command-support")
                 (:file "env-sys-command-builders")
                 (:file "env-time-command-builders")
                 (:file "env-commands")
                 (:file "context-registration")))))

(defsystem "aitools/test/env"
  :description "Environment context tests."
  :depends-on ("aitools/test/base" "aitools/feature/env" "aitools/cli")
  :pathname "../../../"
  :components
  ((:module "t" :pathname "t/" :serial t
    :components
    ((:file "unit/env/package")
     (:file "unit/env/tzif-fixtures")
     (:file "unit/env/support")
     (:file "unit/env/civil-time-test")
     (:file "unit/env/time-input-test")
     (:file "unit/env/tzif-test")
     (:file "unit/env/host-parsers-test")
     (:file "unit/env/host-values-test")
     (:file "unit/env/host-system-parsers-test")
     (:file "unit/env/time-flows-test")
     (:file "unit/env/sys-flows-test")
     (:file "unit/env/sys-snapshot-flow-test")
     (:file "unit/env/sys-flow-edge-test")
     (:file "unit/env/sys-host-flows-test")
     (:file "unit/env/env-commands-test")
     (:module "integration" :pathname "integration/" :serial t
      :components ((:file "env-host-test")
                   (:file "env-host-trace-test")))
     (:module "e2e" :pathname "e2e/" :serial t
      :components ((:file "time-cases")
                   (:file "sys-info-cases")))))))
