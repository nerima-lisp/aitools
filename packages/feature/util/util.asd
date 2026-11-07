(in-package #:asdf-user)

(defsystem "aitools/feature/util"
  :description "Utility codecs, calculations, UUIDs, and random strings."
  :depends-on ("cl-codec-kit" "cl-host-kit")
  :pathname "src/"
  :components
  ((:module "domain" :serial t
    :components
    ((:file "package")
     (:file "codec")
     (:file "text-stats")
     (:file "calc")
     (:file "calc-parser")
     (:file "uuid")
     (:file "random-string")))
   (:module "application" :depends-on ("domain") :serial t
    :components
    ((:file "package")
     (:file "ports")
     (:file "input")
     (:file "flows")
     (:file "codec-flows")
     (:file "calc-flow")
     (:file "random-flows")))
   (:module "infrastructure" :depends-on ("application") :serial t
    :components
    ((:file "package")
     (:file "os-random")
     (:file "ports")))))
(defsystem "aitools/feature/util/presentation"
  :description "Utility CLI commands."
  :depends-on ("aitools/cli/foundation" "aitools/feature/util" "cl-cli")
  :pathname "src/"
  :components
  ((:module "presentation" :serial t :components ((:file "package") (:file "util-commands"))) (:module "composition" :depends-on ("presentation") :serial t :components ((:file "package") (:file "context-registration")))))

(defsystem "aitools/test/util"
  :description "Utility context tests."
  :depends-on ("aitools/test/base" "aitools/feature/util" "aitools/cli")
  :pathname "../../../"
  :components
  ((:module "t" :pathname "t/" :serial t
    :components
    ((:file "unit/util/package")
     (:file "unit/util/codec-test")
     (:file "unit/util/data-tables-test")
     (:file "unit/util/calc-test")
     (:file "unit/util/uuid-random-test")
     (:file "unit/util/flows-test")
     (:file "unit/util/flow-errors-test")
     (:module "integration" :pathname "integration/" :serial t
      :components ((:file "util-cli-test")))
     (:module "e2e" :pathname "e2e/" :serial t
      :components ((:file "process-env-util-cases")))))))
