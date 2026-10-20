(in-package #:asdf-user)

(defsystem "aitools/feature/search"
  :description "Search, find, code, and overview features."
  :depends-on ("aitools/core/kernel" "aitools/core/protocol"
               "aitools/core/text" "aitools/core/workspace"
               "cl-json-kit" "cl-regex-kit")
  :pathname "src/"
  :serial t
  :components
  ((:module "domain" :serial t
    :components ((:file "package") (:file "bytes")
                 (:file "matcher") (:file "matcher-scan")
                 (:file "results") (:file "find")
                 (:file "code-extents") (:file "code")
                 (:file "overview")))
   (:module "application" :serial t
    :components ((:file "package") (:file "ports") (:file "session")
                 (:file "session-file-read")
                 (:file "search-command-line") (:file "search-worker")
                 (:file "search-run") (:file "search-flow")
                 (:file "find-flow") (:file "code-flow")
                 (:file "overview-flow")))
   (:module "infrastructure" :serial t
    :components ((:file "package") (:file "ports")))))
