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
                 (:file "unified-diff") (:file "unified-diff-patch" :depends-on ("flow"))
                 (:file "json") (:file "json-model" :depends-on ("flow")) (:file "flow") (:file "json-model-resolve" :depends-on ("json-model" "flow"))))))
