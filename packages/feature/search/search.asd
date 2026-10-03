(in-package #:asdf-user)

(defsystem "aitools/feature/search"
  :description "Search, find, code, and overview features."
  :depends-on ("aitools/core/kernel" "aitools/core/protocol"
               "aitools/core/text" "aitools/core/workspace"
               "cl-json-kit" "cl-regex-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "bytes") (:file "json")
                 (:file "matcher") (:file "results") (:file "find")
                 (:file "code") (:file "overview")))
   (:module "application"
    :components ((:file "package") (:file "ports") (:file "session")
                 (:file "search-flow") (:file "find-flow")
                 (:file "code-flow") (:file "overview-flow")))
   (:module "infrastructure"
    :components ((:file "package") (:file "ports")))))
