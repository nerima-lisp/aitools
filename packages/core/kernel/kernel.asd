(in-package #:asdf-user)
(defsystem "aitools/data/kernel"
  :description "Static data tables for the kernel domain."
  :depends-on ("aitools/data")
  :pathname "../../../data/"
  :components ((:file "domain/kernel/duration-units-data")
               (:file "domain/kernel/size-units-data")
               (:file "domain/kernel/selector-tables-data")))

(defsystem "aitools/core/kernel"
  :description "Shared kernel value types and pure algorithms."
  :depends-on ("aitools/data/kernel" "cl-crypto-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "path") (:file "selector")
                 (:file "guard") (:file "duration") (:file "time") (:file "size")
                 (:file "digest-kit") (:file "token-estimate")
                 (:file "flow")
                 (:file "unified-diff-lcs")
                 (:file "unified-diff" :depends-on ("unified-diff-lcs"))
                 (:file "unified-diff-patch" :depends-on ("flow" "unified-diff"))
                 (:file "json")
                 (:file "json-model" :depends-on ("flow"))
                 (:file "json-model-patch" :depends-on ("json-model" "json-model-resolve"))
                 (:file "json-model-diff" :depends-on ("json-model"))
                 (:file "json-model-resolve" :depends-on ("json-model" "flow"))))))
(defsystem "aitools/test/kernel"
  :description "kernel context tests."
  :depends-on ("aitools/test/base" "aitools/core/kernel")
  :pathname "../../../t/"
  :serial t
  :components ((:file "unit/kernel/package")
               (:file "unit/kernel/flow-test")
               (:file "unit/kernel/path-test")
               (:file "unit/kernel/selector-test")
               (:file "unit/kernel/selector-expansion-coverage-test")
               (:file "unit/kernel/guard-test")
               (:file "unit/kernel/duration-test")
               (:file "unit/kernel/size-test")
               (:file "unit/kernel/digest-test")
               (:file "unit/kernel/token-estimate-test")
               (:file "unit/kernel/unified-diff-test")
               (:file "unit/kernel/unified-diff-defaults-test")
               (:file "unit/kernel/json-test")
               (:file "unit/kernel/json-value-model-test")
               (:file "unit/kernel/json-model-resolve-test")
               (:file "unit/kernel/json-model-stability-test")))
