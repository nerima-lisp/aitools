(in-package #:asdf-user)

(defsystem "aitools/feature/journal"
  :description "Journal history, replay, and undo features."
  :depends-on ("aitools/core/protocol" "aitools/core/store" "cl-json-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "command-line") (:file "render")))
   (:module "application"
    :components ((:file "package") (:file "ports") (:file "replayers")
                 (:file "common") (:file "history-flow")
                 (:file "undo-flow") (:file "tx-flows")))
   (:module "infrastructure"
    :components ((:file "package") (:file "ports")))))
