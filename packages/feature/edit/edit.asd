(in-package #:asdf-user)

(defsystem "aitools/feature/edit"
  :description "Document, line, content, file, and archive editing."
  :depends-on ("aitools/core/kernel" "aitools/core/protocol"
               "aitools/core/text" "aitools/core/workspace"
               "aitools/core/store" "aitools/feature/journal"
               "aitools/feature/inspect" "cl-json-kit" "cl-regex-kit"
               "cl-codec-kit" "cl-boundary-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "refusal") (:file "document")
                 (:file "old-match") (:file "template") (:file "transform")
                 (:file "json-doc") (:file "regex") (:file "table")
                 (:file "split") (:file "archive-plan")))
   (:module "application"
    :components ((:file "package") (:file "ports") (:file "command-spec")
                 (:file "write-plan") (:file "write-guards")
                 (:file "pipeline") (:file "input") (:file "scan")
                 (:file "edit-flows") (:file "replace-flows")
                 (:file "apply-flows") (:file "line-flows")
                 (:file "content-flows") (:file "file-flows")
                 (:file "mktemp") (:file "json-flows")
                 (:file "archive-flows") (:file "commands")))
   (:module "infrastructure"
    :components ((:file "package") (:file "ports")))))
