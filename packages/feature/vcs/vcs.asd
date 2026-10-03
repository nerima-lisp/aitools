(in-package #:asdf-user)

(defsystem "aitools/feature/vcs"
  :description "Version-control inspection features."
  :depends-on ("aitools/core/protocol" "cl-json-kit" "cl-vcs-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "render") (:file "paths")
                 (:file "status") (:file "log") (:file "blame")
                 (:file "diff") (:file "blob")))
   (:module "application"
    :components ((:file "package") (:file "port") (:file "flows")
                 (:file "blame-show-flows")))
   (:module "infrastructure"
    :components ((:file "package") (:file "git-port")))))
