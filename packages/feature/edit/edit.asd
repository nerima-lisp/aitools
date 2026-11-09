(in-package #:asdf-user)

(defsystem "aitools/feature/edit"
  :description "Document, line, content, file, and archive editing."
  :depends-on ("aitools/core/kernel" "aitools/data"
                                     "aitools/core/protocol"
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
                        :serial
                        t
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
                         (:file "yaml-doc")
                         (:file "toml-doc")
                         (:file "regex")
                         (:file "mtime")
                         (:file "table")
                         (:file "split")
                         (:file "archive-plan-support")
                         (:file "archive-plan")
                         (:file "archive-build")))
               (:module "application"
                        :depends-on
                        ("domain")
                        :serial
                        t
                        :components
                        ((:file "package") (:file "ports")
                         (:file "command-spec")
                         (:file "write-plan")
                         (:file "write-guards")
                         (:file "input-selectors")
                         (:file "input-targets")
                         (:file "pipeline")
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
                         (:file "yaml-toml-flows")
                         (:file "yaml-toml-command-flows")
                         (:file "table-flows")
                         (:file "archive-flows")
                         (:file "archive-create-flows")
                         (:file "commands")
                         (:file "replay-flows")))
               (:module "infrastructure"
                        :depends-on
                        ("application")
                        :serial
                        t
                        :components
                        ((:file "package") (:file "ports")))))
(defsystem "aitools/feature/edit/presentation"
  :description "Document and file editing CLI commands."
  :depends-on ("aitools/cli/foundation" "aitools/feature/edit" "cl-cli")
  :pathname "src/"
  :components
  ((:module "presentation" :serial t
    :components ((:file "package")
                 (:file "commands")))
   (:module "composition" :depends-on ("presentation") :serial t
    :components ((:file "package")
                 (:file "context-registration")))))

(defsystem "aitools/test/edit"
  :description "Edit context tests."
  :depends-on ("aitools/test/base" "aitools/feature/edit" "aitools/cli")
  :pathname "../../../"
  :components
  ((:module "t" :pathname "t/" :serial t
    :components
    ((:file "unit/edit/package")
     (:file "unit/edit/domain-test")
     (:file "unit/edit/command-spec-test")
     (:file "unit/edit/domain-edge-test")
     (:file "unit/edit/pipeline-edge-test")
     (:file "unit/edit/format-test")
     (:file "unit/edit/format-edge-test")
     (:file "unit/edit/table-test")
     (:file "unit/edit/archive-path-limits-test")
     (:file "unit/edit/archive-pax-limit-test")
     (:file "unit/edit/archive-symlink-test")
     (:module "yaml-toml-write-tests" :pathname "unit/edit/" :serial t
      :components ((:file "yaml-doc-test")
                   (:file "toml-doc-test")
                   (:file "yaml-flows-test")
                   (:file "toml-flows-test")))
     (:module "integration" :pathname "integration/" :serial t
      :components ((:file "edit-text-test")
                   (:file "edit-text-pipeline-test")
                   (:file "edit-text-write-runtime-test")
                   (:file "edit-text-ports-test")
                   (:file "edit-text-commands-test")
                   (:file "edit-text-commands-inputs-test")
                   (:file "edit-text-command-input-ports-test")
                   (:file "edit-text-command-arguments-test")
                   (:file "edit-text-commands-paths-test")
                   (:file "edit-files-test")
                   (:file "edit-files-safety-test")
                   (:file "edit-files-refusals-test")
                   (:file "edit-tx-and-limits-test")
                   (:file "edit-tx-lock-test")
                   (:file "edit-cli-test")
                   (:file "edit-json-repair-test")
                   (:file "edit-security-test")
                   (:file "edit-tx-replay-test")
                   (:file "edit-text-write-commands-test")
                   (:file "edit-text-paths-extra-test")
                   (:file "edit-file-formats-test")
                   (:file "edit-archive-symlink-safety-test")
                   (:file "edit-archive-tx-symlink-test")
                   (:file "edit-archive-tx-parent-swap-test")
                   (:file "edit-file-metadata-test")
                   (:file "edit-rm-safety-test")
                   (:file "edit-mktemp-race-test")
                   (:file "edit-files-refusals-more-test")
                   (:file "edit-data-refusals-test")
                   (:file "edit-cli-expect-count-test")
                   (:file "edit-cli-paths-test")))
     (:module "e2e" :pathname "e2e/" :serial t
      :components ((:file "edit-cases")
                   (:file "edit-envelope-compatibility")
                   (:file "transform-file-cases")
                   (:file "file-operation-cases")
                   (:file "yaml-toml-write-cases")
                   (:file "yaml-toml-write-errors")
                   (:file "yaml-toml-write-format-cases")
                   (:file "archive-failure-cases")
                   (:file "json-write-cases")
                   (:file "archive-write-cases")))))))
