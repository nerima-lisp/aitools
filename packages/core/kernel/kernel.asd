(in-package #:asdf-user)

(defsystem "aitools/core/kernel"
  :description "Shared kernel value types and pure algorithms."
  :depends-on ("cl-crypto-kit")
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
                 (:file "json-model-patch" :depends-on ("json-model"))
                 (:file "json-model-diff" :depends-on ("json-model"))
                 (:file "json-model-resolve" :depends-on ("json-model" "flow"))))))
