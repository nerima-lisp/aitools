(in-package #:asdf-user)

(defsystem "aitools/feature/journal"
  :description "Journal history, replay, and undo features."
  :depends-on ("aitools/core/protocol" "aitools/core/store" "cl-json-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "command-line") (:file "render")))
   (:module "application"
    :components ((:file "package")
                 (:file "ports")
                 (:file "replayers")
                 (:file "common")
                 (:file "history-flow")
                 (:file "undo-flow")
                 (:file "tx-flows")
                 (:file "tx-stage-flows")
                 (:file "tx-commit-flows")))
   (:module "infrastructure"
    :components ((:file "package") (:file "ports")))))
(defsystem "aitools/feature/journal/presentation"
  :description "Journal and undo CLI commands."
  :depends-on ("aitools/cli/foundation" "aitools/feature/journal" "cl-cli")
  :pathname "src/"
  :components
  ((:module "presentation" :serial t
    :components ((:file "package")
                 (:file "commands")
                 (:file "context-registration")))))

(defsystem "aitools/test/journal"
  :description "Journal context tests."
  :depends-on ("aitools/test/base" "aitools/feature/journal" "aitools/cli")
  :pathname "../../../"
  :components
  ((:module "t" :pathname "t/" :serial t
    :components
    ((:file "unit/journal/package")
     (:file "unit/journal/domain-test")
     (:file "unit/journal/changes-json-test")
     (:module "integration" :pathname "integration/" :serial t
      :components ((:file "journal-flows-test")
                   (:file "journal-tx-test")
                   (:file "journal-tx-rebase-test")
                   (:file "journal-tx-failure-test")
                   (:file "journal-cli-test")))))))
