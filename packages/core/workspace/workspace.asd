(in-package #:asdf-user)

(defsystem "aitools/core/workspace"
  :description "Workspace discovery and ignore handling."
  :depends-on ("aitools/core/kernel" "cl-codec-kit" "cl-host-kit"
               "cl-boundary-kit" "cl-concurrent-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "path-syntax") (:file "entry")
                 (:file "wildmatch") (:file "gitignore")
                 (:file "builtin-excludes") (:file "glob")
                 (:file "git-config") (:file "git-index")
                 (:file "repository") (:file "boundary")))
   (:module "application"
    :components ((:file "package") (:file "host") (:file "real-path")
                 (:file "root") (:file "boundary")
                 (:file "ignore-context") (:file "scan")))
   (:module "infrastructure"
    :components ((:file "package") (:file "host")
                 (:file "ordered-mapper") (:file "boundaries")))))
