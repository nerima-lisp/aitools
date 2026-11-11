(in-package #:asdf-user)

(defsystem "aitools/data/inspect"
  :description "Static JSON query data for inspect."
  :depends-on ("aitools/data")
  :pathname "../../../data/"
  :components
  ((:file "domain/inspect/json-query-operators-data")))

(defsystem "aitools/feature/inspect"
           :description "Inspection, archive, snapshot, and table features."
           :depends-on ("aitools/data/inspect" "aitools/core/kernel" "aitools/core/protocol"
                                              "aitools/core/workspace"
                                              "aitools/core/text"
                                              "aitools/core/store"
                                              "cl-json-kit"
                                              "cl-yaml-kit"
                                              "cl-toml-kit"
                                              "cl-date-kit"
                                              "cl-codec-kit"
                                              "cl-host-kit"
                                              "cl-boundary-kit"
                                              "cl-crypto-kit")
           :pathname "src/"
           :components ((:module "domain"
                                 :components
                                 ((:file "package")
                                  (:file "json-values")
                                  (:file "lines")
                                  (:file "pattern")
                                  (:file "similarity")
                                  (:file "lisp-scan")
                                  (:file "outline")
                                  (:file "selection")
                                  (:file "selection-symbol-flow")
                                  (:file "read-render")
                                  (:file "file-facts")
                                  (:file "diff")
                                  (:file "json-query")
                                  (:file "yaml-values")
                                  (:file "toml-values")
                                  (:file "table")
                                  (:file "table-text-build")
                                  (:file "table-build")
                                  (:file "table-query")
                                  (:file "table-agg")
                                  (:file "archive")
                                  (:file "snapshot")))
                        (:module "application" :serial t
                                 :components
                                 ((:file "package")
                                  (:file "context")
                                  (:file "files")
                                  (:file "target-flows")
                                  (:file "selector")
                                  (:file "read-text-flow")
                                  (:file "read-text-view-output")
                                  (:file "read-text-view-selection-output")
                                  (:file "read-text-view-flow")
                                  (:file "read-binary-flow")
                                  (:file "read-strings-flow")
                                  (:file "read-text-reader-flow")
                                  (:file "read-flow")
                                  (:file "read-entry-flow")
                                  (:file "info-flow")
                                  (:file "check-flow")
                                  (:file "diff-flow")
                                  (:file "diff-op-flows")
                                  (:file "json-flows")
                                  (:file "format-document-flows")
                                  (:file "format-flows")
                                  (:file "json-get-flow")
                                  (:file "json-select-helpers")
                                  (:file "json-select-flow")
                                  (:file "json-diff-flows")
                                  (:file "document-command-flow")
                                  (:file "yaml-flows")
                                  (:file "toml-flows")
                                  (:file "table-option-flows")
                                  (:file "table-flows")
                                  (:file "table-read-flows")
                                  (:file "table-agg-validation-flows")
                                  (:file "table-agg-flows")
                                  (:file "archive-flows")
                                  (:file "archive-read-flows")
                                  (:file "archive-read-item-flow")
                                  (:file "archive-read-flow")
                                  (:file "snapshot-flows")
                                  (:file "snapshot-diff-flows")))))
(defsystem "aitools/feature/inspect/presentation"
  :description "Inspection and archive CLI commands."
  :depends-on ("aitools/cli/foundation" "aitools/feature/inspect" "cl-cli")
  :pathname "src/"
  :components
  ((:module "presentation" :serial t :components ((:file "package") (:file "registry") (:file "file-commands") (:file "json-commands") (:file "table-commands") (:file "archive-commands") (:file "snapshot-commands"))) (:module "composition" :depends-on ("presentation") :serial t :components ((:file "package") (:file "context-registration")))))

(defsystem "aitools/test/inspect"
  :description "Inspection context tests."
  :depends-on ("aitools/test/base" "aitools/data/inspect" "aitools/feature/inspect" "aitools/feature/inspect/presentation" "aitools/cli")
  :pathname "../../../"
  :components
  ((:module "t" :pathname "t/" :serial t
    :components
    ((:file "unit/inspect/package")
     (:file "unit/inspect/support")
     (:file "unit/inspect/context-test")
     (:file "unit/inspect/read-test")
     (:file "unit/inspect/read-missing-path-test")
     (:file "unit/inspect/read-more-test")
     (:module "yaml-toml-read-tests" :pathname "unit/inspect/" :serial t
      :components ((:file "yaml-values-test")
                   (:file "toml-values-test")
                   (:file "yaml-flows-test")
                   (:file "toml-flows-test")))
     (:file "unit/inspect/json-query-test")
     (:file "unit/inspect/json-flow-skeleton-test")
     (:file "unit/inspect/json-query-property-test")
     (:file "unit/inspect/table-test")
     (:file "unit/inspect/table-edge-test")
     (:file "unit/inspect/archive-test")
     (:file "unit/inspect/archive-size-limit-test")
     (:file "unit/inspect/archive-list-test")
     (:file "unit/inspect/snapshot-test")
     (:file "unit/inspect/info-check-diff-test")
     (:file "unit/inspect/text-content-counts-test")
     (:file "unit/inspect/diff-edge-test")
     (:file "unit/inspect/diff-operation-coverage-test")
     (:file "unit/inspect/inspect-file-error-test")
     (:file "unit/inspect/selection-test")
     (:file "unit/inspect/selector-property-test")
     (:module "integration" :pathname "integration/" :serial t
      :components ((:file "inspect-boundary-contract-test")
                   (:file "inspect-snapshot-test")
                   (:file "inspect-cli-test")
                   (:file "inspect-cli-commands-test")
                   (:file "inspect-yaml-toml-read-test")
                   (:file "inspect-cli-store-test")))
     (:module "inspect-coverage-edge-tests" :pathname "unit/inspect/" :serial t
      :components ((:file "application-read-edge-test")
                   (:file "application-flow-coverage-test")
                   (:file "archive-flow-edge-test")
                   (:file "document-snapshot-flow-edge-test")
                   (:file "json-domain-edge-test")
                   (:file "lisp-source-edge-test")
                   (:file "diff-domain-coverage-test")
                   (:file "outline-diff-snapshot-coverage-test")
                   (:file "line-domain-edge-test")
                   (:file "read-render-edge-test")
                   (:file "read-parse-domain-coverage-test")
                   (:file "presentation-registry-coverage-test")
                   (:file "table-domain-coverage-test")))
     (:module "e2e" :pathname "e2e/" :serial t
      :components ((:file "read-search-cases")
                   (:file "format-cases")
                   (:file "yaml-toml-read-cases")
                   (:file "diff-cases")))))))
