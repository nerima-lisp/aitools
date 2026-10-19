(in-package #:asdf-user)

(defsystem "aitools/feature/inspect"
           :description "Inspection, archive, snapshot, and table features."
           :depends-on ("aitools/core/kernel" "aitools/core/protocol"
                                              "aitools/core/workspace"
                                              "aitools/core/text"
                                              "aitools/core/store"
                                              "cl-json-kit"
                                              "cl-codec-kit"
                                              "cl-host-kit"
                                              "cl-boundary-kit")
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
                                  (:file "table")
                                  (:file "table-text-build")
                                  (:file "table-build")
                                  (:file "table-query")
                                  (:file "table-agg")
                                  (:file "archive")
                                  (:file "snapshot")))
                        (:module "application"
                                 :components
                                 ((:file "package")
                                  (:file "ports")
                                  (:file "context")
                                  (:file "files")
                                  (:file "target-flows")
                                  (:file "selector")
                                  (:file "read-text-flow")
                                  (:file "read-text-view-output")
                                  (:file "read-text-view-selection-output")
                                  (:file "read-text-view-flow")
                                  (:file "read-target-error-flow")
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
                                  (:file "json-get-flow")
                                  (:file "json-select-helpers")
                                  (:file "json-select-flow")
                                  (:file "json-diff-flows")
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
                                  (:file "snapshot-diff-flows")))
                        (:module "infrastructure"
                                 :components
                                 ((:file "package") (:file "ports")))))
