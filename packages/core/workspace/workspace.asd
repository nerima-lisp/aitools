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
                 (:file "ignore-context") (:file "scan-model")
                 (:file "scan-helpers") (:file "scan-walk")
                 (:file "scan") (:file "scan-options")))
   (:module "infrastructure"
    :components ((:file "package") (:file "host")
                 (:file "ordered-mapper")))))
(defsystem "aitools/test/workspace"
           :description "workspace context tests."
           :depends-on ("aitools/test/base" "aitools/core/workspace")
           :pathname "../../../t/"
           :serial t
           :components ((:file "unit/workspace/package")
 (:file "unit/workspace/fake-workspace-boundaries")
 (:file "unit/workspace/wildmatch-test")
 (:file "unit/workspace/gitignore-test")
 (:file "unit/workspace/root-boundary-test")
 (:file "unit/workspace/workspace-values-test")
 (:file "unit/workspace/real-path-test")
 (:file "unit/workspace/git-data-test")
 (:file "unit/workspace/scan-test")
 (:file "unit/workspace/scan-edge-test")
 (:file "unit/workspace/host-boundary-edge-test")
 (:file "unit/workspace/scan-options-test")
 (:file "unit/workspace/scan-worker-pool-test")
 (:file "unit/workspace/scan-git-config-test")
 (:file "integration/workspace-gitignore-parity-test")
 (:file "integration/workspace-host-test")))
