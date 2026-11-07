(in-package #:asdf-user)

(defsystem "aitools/feature/org" :description "Org document commands." :depends-on ("aitools/core/kernel" "aitools/core/protocol" "aitools/core/text" "aitools/core/workspace" "aitools/feature/edit" "cl-org-kit" "cl-date-kit") :pathname "src/" :components ((:module "domain" :components ((:file "package") (:file "document"))) (:module "application" :components ((:file "package") (:file "ports") (:file "read-flow") (:file "outline-flow") (:file "query-flow") (:file "export-flow") (:file "write-flow") (:file "edit-flow") (:file "agenda-flow") (:file "clock-report-flow"))) (:module "infrastructure" :components ((:file "package") (:file "org-kit-ports") (:file "time-query-ports") (:file "time-context-ports") (:file "planning-timestamp-ports") (:file "org-kit-planning-ports") (:file "org-kit-table-ports") (:file "org-kit-clock-ports") (:file "org-kit-agenda-ports") (:file "org-kit-write-ports") (:file "ports")))))
(defsystem "aitools/feature/org/presentation"
  :description "Org document CLI commands."
  :depends-on ("aitools/cli/foundation" "aitools/feature/org" "cl-cli")
  :pathname "src/"
  :components
  ((:module "presentation" :serial t
    :components ((:file "package")
                 (:file "commands")
                 (:file "command-support")
                 (:file "todo-property-commands")
                 (:file "planning-clock-commands")
                 (:file "agenda-report-commands")
                 (:file "table-commands")
                 (:file "command-registration")
                 (:file "context-registration")))))
