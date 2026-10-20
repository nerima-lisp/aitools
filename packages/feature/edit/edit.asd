(in-package #:asdf-user)

(defsystem "aitools/feature/edit"
  :description "Document, line, content, file, and archive editing."
  :depends-on ("aitools/data"
               "aitools/core/kernel" "aitools/core/protocol"
               "aitools/core/text"
               "aitools/core/workspace"
               "aitools/core/store"
               "aitools/feature/journal"
               "aitools/feature/inspect"
               "cl-json-kit"
               "cl-regex-kit"
               "cl-codec-kit"
               "cl-boundary-kit")
  :pathname "src/"
  :components ((:module "domain"
                        :components
                        ((:file "package") (:file "refusal")
                         (:file "document")
                         (:file "document-decode")
                         (:file "document-finalization")
                         (:file "old-match")
                         (:file "template")
                         (:file "version-order")
                         (:file "transform-prose")
                         (:file "transform")
                         (:file "json-doc")
                         (:file "regex")
                         (:file "mtime")
                         (:file "table")
                         (:file "split")
                         (:file "archive-plan")))
               (:module "application"
                        :components
                        ((:file "package") (:file "ports")
                         (:file "command-spec")
                         (:file "write-plan")
                         (:file "write-guards")
                         (:file "input-selectors")
                         (:file "input-targets")
                         (:file "write-targets")
                         (:file "write-validation")
                         (:file "write-repairs")
                         (:file "write-operation")
                         (:file "write-runtime")
                         (:file "write-command")
                         (:file "input")
                         (:file "input-content-flows")
                         (:file "scan")
                         (:file "edit-operation-flows")
                         (:file "edit-flows")
                         (:file "replace-plan")
                         (:file "replace-flows")
                         (:file "replace-command-flows")
                         (:file "apply-plan-flows")
                         (:file "apply-flows")
                         (:file "line-flows")
                         (:file "transform-command-flows")
                         (:file "content-flows")
                         (:file "split-plan-flows")
                         (:file "split-command-flows")
                         (:file "insert-flows")
                         (:file "move-lines-flows")
                         (:file "move-lines-command-flows")
                         (:file "transcode-flows")
                         (:file "file-helper-flows")
                         (:file "file-flows")
                         (:file "file-copy-flows")
                         (:file "file-copy-command-flows")
                         (:file "file-move-flows")
                         (:file "file-metadata-flows")
                         (:file "mktemp")
                         (:file "json-flows")
                         (:file "json-edit-flows")
                         (:file "json-command-flows")
                         (:file "table-flows")
                         (:file "archive-flows")
                         (:file "archive-create-flows")
                         (:file "replay-flows")
                         (:file "commands")))
               (:module "infrastructure" :components ((:file "package") (:file "ports")))))
