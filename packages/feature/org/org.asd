(in-package #:asdf-user)

(defsystem "aitools/feature/org"
  :description "Org document commands."
  :depends-on ("aitools/core/kernel" "aitools/core/protocol"
               "aitools/core/text" "aitools/core/workspace"
               "aitools/feature/edit" "cl-org-kit" "cl-date-kit")
  :pathname "src/"
  :components ((:module "domain"
                :components ((:file "package")
                             (:file "document")))
               (:module "application"
                :components ((:file "package")
                             (:file "ports")
                             (:file "read-flow")
                             (:file "outline-flow")
                             (:file "query-flow")
                             (:file "export-flow")
                             (:file "write-flow")
                             (:file "edit-flow")
                             (:file "agenda-flow")
                             (:file "clock-report-flow")))
               (:module "infrastructure"
                :components ((:file "package")
                             (:file "org-kit-ports")
                             (:file "time-query-ports")
                             (:file "time-context-ports")
                             (:file "planning-timestamp-ports")
                             (:file "org-kit-planning-ports")
                             (:file "org-kit-table-ports")
                             (:file "org-kit-clock-ports")
                             (:file "org-kit-agenda-ports")
                             (:file "org-kit-write-ports")
                             (:file "ports")))))

(defsystem "aitools/feature/org/presentation"
  :description "Org document CLI commands."
  :depends-on ("aitools/cli/foundation" "aitools/feature/org" "cl-cli")
  :pathname "src/"
  :components
  ((:module "presentation" :serial t :components ((:file "package") (:file "commands") (:file "command-support") (:file "todo-property-commands") (:file "planning-clock-commands") (:file "agenda-report-commands") (:file "table-commands") (:file "command-registration"))) (:module "composition" :depends-on ("presentation") :serial t :components ((:file "package") (:file "context-registration")))))

(defsystem "aitools/test/org"
  :description "Org context tests."
  :depends-on ("aitools/test/base" "aitools/feature/org" "aitools/cli")
  :pathname "../../../"
  :components
  ((:module "t" :pathname "t/" :serial t
    :components
    ((:file "unit/edit/package")
     (:file "unit/org/package")
     (:file "unit/org/parser-property-test")
     (:module "integration" :pathname "integration/" :serial t
      :components
      ((:file "org-write-flow-test")
       (:file "org-adapter-validation-test")
       (:file "org-adapter-flow-edge-test")
       (:file "org-dispatch-test")
       (:file "org-write-dispatch-test")
       (:file "org-write-edge-test")
       (:file "org-read-errors-test" :depends-on ("org-dispatch-test"))
       (:file "org-read-contract-test" :depends-on ("org-read-errors-test"))
       (:file "org-read-port-contract-test" :depends-on ("org-read-contract-test"))))
     (:module "e2e" :pathname "e2e/" :serial t
      :components
      ((:file "org-cases")
       (:file "org-read-error-cases")
       (:file "org-query-cases")
       (:file "org-export-cases")
       (:file "org-write-cases")
       (:file "org-write-edge-cases")
       (:file "org-agenda-cases")
       (:file "org-write-workflow-cases")
       (:file "org-write-table-cases")))))))
