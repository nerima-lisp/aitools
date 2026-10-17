(in-package #:asdf-user)

(defsystem "aitools/feature/inspect"
  :description "Inspection, archive, snapshot, and table features."
  :depends-on ("aitools/core/kernel" "aitools/core/protocol"
               "aitools/core/workspace" "aitools/core/text"
               "aitools/core/store" "cl-json-kit" "cl-codec-kit"
               "cl-host-kit" "cl-boundary-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "json-values") (:file "lines")
                 (:file "pattern") (:file "similarity")
                 (:file "lisp-scan") (:file "outline") (:file "selection")
                 (:file "read-render") (:file "file-facts") (:file "diff")
                 (:file "json-query") (:file "table")
                 (:file "table-build") (:file "table-query")
                 (:file "table-agg") (:file "archive") (:file "snapshot")))
   (:module "application"
    :components ((:file "package") (:file "ports") (:file "context")
                 (:file "files") (:file "selector") (:file "read-flow")
                 (:file "info-flow") (:file "check-flow")
                 (:file "diff-flow") (:file "json-flows")
                 (:file "table-flows") (:file "table-agg-flows")
                 (:file "archive-flows") (:file "snapshot-flows")))
   (:module "infrastructure"
    :components ((:file "package") (:file "ports")))))
