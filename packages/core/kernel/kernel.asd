(in-package #:asdf-user)

(defsystem "aitools/core/kernel"
  :description "Shared kernel value types and pure algorithms."
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "path") (:file "selector")
                 (:file "guard") (:file "duration") (:file "size")
                 (:file "digest") (:file "token-estimate")
                 (:file "unified-diff") (:file "unified-diff-patch")
                 (:file "json")))))
