(in-package #:asdf-user)

(defsystem "aitools/feature/org"
  :description "Org document commands."
  :depends-on ("aitools/core/kernel" "aitools/core/protocol"
               "aitools/core/text" "aitools/core/workspace"
               "aitools/feature/edit" "cl-boundary-kit"
               "cl-org-kit" "cl-date-kit" "cl-json-kit")
  :pathname "src/"
  :components ((:module "domain"
                :components ((:file "package")
                             (:file "document")
                             (:file "archive-timestamp")))
               (:module "application"
                :components ((:file "package")
                             (:file "read-flow")
                             (:file "read-files-flow" :depends-on ("read-flow"))
                             (:file "outline-flow")
                             (:file "query-flow" :depends-on ("read-files-flow"))
                             (:file "export-flow")
                             (:file "write-flow")
                             (:file "archive-flow" :depends-on ("write-flow"))
                             (:file "edit-flow")
                             (:file "dynamic-block-flow")
                             (:file "agenda-flow" :depends-on ("read-files-flow"))
                             (:file "clock-report-flow")))
               (:module "infrastructure"
                :components ((:file "package")
                             (:file "org-kit-adapter")
                             (:file "time-query-adapter")
                             (:file "time-context-adapter")
                             (:file "planning-timestamp-adapter")
                             (:file "org-kit-planning-adapter")
                             (:file "org-kit-heading-edit-adapter")
                             (:file "org-kit-table-adapter")
                             (:file "org-kit-clock-adapter")
                             (:file "org-kit-agenda-adapter")
                             (:file "org-kit-dynamic-block-adapter")
                             (:file "org-kit-write-adapter")
                             (:file "org-kit-archive-adapter" :depends-on ("org-kit-planning-adapter"))
                             (:file "capabilities")
                             (:file "org-kit-capture-adapter")))))

(defsystem "aitools/feature/org/presentation"
  :description "Org document CLI commands."
  :depends-on ("aitools/cli/foundation" "aitools/feature/org" "cl-cli")
  :pathname "src/"
  :components
  ((:module "presentation" :serial t :components ((:file "package") (:file "commands") (:file "command-support") (:file "todo-property-commands") (:file "planning-clock-commands") (:file "agenda-report-commands") (:file "table-commands") (:file "dynamic-block-commands")
                                                   (:file "capture-command")
                                                   (:file "command-registration"))) (:module "composition" :depends-on ("presentation") :serial t :components ((:file "package") (:file "context-registration")))))

(defsystem "aitools/test/org"
  :description "Org context tests."
  :depends-on ("aitools/test/base" "aitools/feature/org" "aitools/cli")
  :pathname "../../../"
  :components
  ((:module "t" :pathname "t/" :serial t
    :components
    ((:file "unit/edit/package")
     (:file "unit/edit/boundaries")
     (:file "unit/org/package")
     (:file "unit/org/parser-property-test")
     (:module "integration" :pathname "integration/" :serial t
      :components
      ((:file "org-write-flow-test")
       (:file "org-read-flow-coverage-test")
       (:file "org-multi-file-coverage-test")
       (:file "org-multi-file-failure-coverage-test")
       (:file "org-infrastructure-coverage-test")
       (:file "org-planning-timestamp-coverage-test")
       (:file "org-planning-coverage-test")
       (:file "org-archive-timestamp-test")
       (:file "org-heading-path-coverage-test")
       (:file "org-heading-path-selection-coverage-test")
       (:file "org-planning-edge-coverage-test")
       (:file "org-time-adapter-coverage-test")
       (:file "org-time-adapter-extra-coverage-test")
       (:file "org-agenda-clock-coverage-test")
       (:file "org-agenda-extra-coverage-test")
       (:file "org-adapter-coverage-test")
       (:file "org-application-coverage-test")
       (:file "org-adapter-validation-test")
       (:file "org-adapter-flow-edge-test")
       (:file "org-dispatch-test")
       (:file "org-write-dispatch-test")
       (:file "org-write-adapter-extra-coverage-test")
       (:file "org-capture-coverage-test")
       (:file "org-dynamic-block-coverage-test")
       (:file "org-dynamic-block-agenda-coverage-test")
       (:file "org-dynamic-block-clock-coverage-test")
       (:file "org-dynamic-block-scope-test")
       (:file "org-dynamic-block-scope-message-test")
       (:file "org-dynamic-block-emacs-layout-test")
       (:file "org-write-edge-test")
       (:file "org-capture-coverage-failure-test")
       (:file "org-archive-coverage-test")
       (:file "org-archive-rejection-coverage-test")
       (:file "org-archive-adapter-coverage-test")
       (:file "org-planning-weekday-test")
       (:file "org-planning-closed-test")
       (:file "org-planning-order-test")
       (:file "org-read-errors-test" :depends-on ("org-dispatch-test"))
       (:file "org-read-contract-test" :depends-on ("org-read-errors-test"))
       (:file "org-read-boundary-contract-test" :depends-on ("org-read-contract-test"))
       (:file "org-boundary-contract-test")
       (:file "org-heading-edit-checkbox-coverage-test")
       (:file "org-heading-edit-priority-tags-coverage-test")
       (:file "org-heading-edit-rename-flow-coverage-test")))
     (:module "e2e" :pathname "e2e/" :serial t
      :components
      ((:file "org-cases")
       (:file "org-read-error-cases")
       (:file "org-query-cases")
       (:file "org-export-cases")
       (:file "org-write-cases")
       (:file "org-write-edge-cases")
       (:file "org-heading-path-cases")
       (:file "org-heading-path-write-cases")
       (:file "org-heading-edit-cases")
       (:file "org-agenda-cases")
       (:file "org-write-workflow-cases")
       (:file "org-write-table-cases")
       (:file "org-dynamic-block-cases")
       (:file "org-capture-cases")))))))
