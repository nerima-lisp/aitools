(in-package #:asdf-user)

(defsystem "aitools/feature/process"
  :description "Foreground and background process features."
  :depends-on ("aitools/data" "aitools/core/protocol" "aitools/core/text" "cl-boundary-kit" "cl-json-kit" "cl-host-kit" "cl-process-kit" "cl-concurrent-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "line-pattern")
                 (:file "terminal-text") (:file "output-report")
                 (:file "process-outcome") (:file "bg-record")
                 (:file "log-slice") (:file "wait-condition")
                 (:file "signal-target")))
   (:module "application" :serial t
    :components ((:file "package")
                 (:file "boundaries")
                 (:file "common")
                 (:file "run-flow")
                 (:file "bg-flow")
                 (:file "bg-query-flow")
                 (:file "bg-stop-flow")
                 (:file "wait-flow")
                 (:file "signal-flow")))
   (:module "infrastructure" :serial t
    :components ((:file "package") (:file "host")
                 (:file "signal-host-helpers") (:file "signal-host")
                 (:file "runner") (:file "bg-launcher") (:file "boundaries")))))
(defsystem "aitools/feature/process/presentation"
  :description "Process CLI commands."
  :depends-on ("aitools/cli/foundation" "aitools/feature/process" "cl-boundary-kit" "cl-cli")
  :pathname "src/"
  :components
  ((:module "presentation" :serial t :components ((:file "package") (:file "commands")))
   (:module "composition" :depends-on ("presentation") :serial t :components ((:file "package") (:file "context-registration")))))

(defsystem "aitools/test/process"
  :description "Process context tests."
  :depends-on ("aitools/test/base" "aitools/feature/process" "aitools/cli")
  :pathname "../../../"
  :components
  ((:module "t" :pathname "t/" :serial t
    :components
    ((:file "unit/process/package")
     (:file "unit/process/fakes")
     (:file "unit/process/domain-test")
     (:file "unit/process/domain-records-test")
     (:file "unit/process/domain-coverage-test")
     (:file "unit/process/flows-test")
     (:file "unit/process/flows-bg-control-test")
     (:file "unit/process/flows-bg-wait-control-test")
     (:file "unit/process/flows-bg-error-test")
     (:file "unit/process/signal-flow-test")
     (:file "unit/process/flow-coverage-test")
     (:file "unit/process/host-clock-test")
     (:file "unit/process/host-boundary-test")
     (:file "unit/process/host-safety-test")
     (:file "unit/process/infrastructure-coverage-test")
     (:file "unit/process/runner-coverage-test")
     (:module "integration" :pathname "integration/" :serial t :components ((:file "process-test")
                   (:file "process-run-test")
                   (:file "process-timeout-group-test")
                   (:file "process-bg-test")
                   (:file "process-bg-ended-test")
                   (:file "process-signal-test")
                   (:file "process-ports-test")
                   (:file "process-ports-state-test")
                   (:file "process-ports-launch-test")
                   (:file "process-ports-launch-trace-test") (:file "process-boundary-contract-test")))
     (:module "e2e" :pathname "e2e/" :serial t
      :components ((:file "process-env-cases")))))))
