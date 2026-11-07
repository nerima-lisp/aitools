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
                 (:file "code-extent-common") (:file "code-extents") (:file "code")
                 (:file "overview")))
   (:module "application" :serial t
    :components ((:file "package") (:file "ports") (:file "session") (:file "session-open")
                 (:file "session-file-read")
                 (:file "search-command-line") (:file "search-worker")
                 (:file "search-run") (:file "search-flow")
                 (:file "find-flow") (:file "code-flow")
                 (:file "overview-flow")))
   (:module "infrastructure" :serial t
    :components ((:file "package") (:file "ports")))))
(defsystem "aitools/feature/search/presentation"
  :description "Search and find CLI commands."
  :depends-on ("aitools/cli/foundation" "aitools/feature/search" "cl-cli")
  :pathname "src/"
  :components
  ((:module "presentation" :serial t :components ((:file "package") (:file "commands") (:file "command-registration"))) (:module "composition" :depends-on ("presentation") :serial t :components ((:file "package") (:file "context-registration")))))

(defsystem "aitools/test/search"
  :description "Search context tests."
  :depends-on ("aitools/test/base" "aitools/feature/search" "aitools/cli")
  :pathname "../../../"
  :components
  ((:module "t" :pathname "t/" :serial t
    :components
    ((:file "unit/search/package")
     (:file "unit/search/fakes")
     (:file "unit/search/matcher-test")
     (:file "unit/search/search-flow-test")
     (:file "unit/search/search-flow-options-test")
     (:file "unit/search/find-flow-test")
     (:file "unit/search/code-flow-test")
     (:file "unit/search/code-extents-test")
     (:file "unit/search/overview-flow-test")
     (:file "unit/search/domain-contract-test")
     (:file "unit/search/commands-test")
     (:file "unit/search/allocation-child-protocol")
     (:file "unit/search/allocation-child-protocol-test")
     (:module "perf" :pathname "perf/" :serial t
      :components ((:file "search-allocation-cache")
                   (:file "search-allocation-test")))
     (:module "integration" :pathname "integration/" :serial t
      :components ((:file "search-host-test")))
     (:module "e2e" :pathname "e2e/" :serial t
      :components ((:file "search-find-cases")))))))
