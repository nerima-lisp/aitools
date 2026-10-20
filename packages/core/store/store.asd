(in-package #:asdf-user)

(defsystem "aitools/core/store"
  :description "Crash-safe storage and transaction primitives."
  :depends-on ("aitools/data" "aitools/core/kernel" "aitools/core/protocol"
               "aitools/core/text" "aitools/core/workspace" "cl-json-kit" "cl-host-kit"
               "cl-boundary-kit" "cl-concurrent-kit" "cl-resilience-kit")
  :pathname "src/"
  :serial t
  :components
  ((:module "domain"
    :serial t
    :components ((:file "package") (:file "entry-state") (:file "layout")
                 (:file "change") (:file "plan") (:file "journal")
                 (:file "intent") (:file "tx-model")
                 (:file "tx-index-ops") (:file "changes-json")))
   (:module "application"
    :serial t
    :components ((:file "package") (:file "port") (:file "lock")
                 (:file "files") (:file "journal") (:file "blobs")
                 (:file "view") (:file "write-completion")
                 (:file "write-preparation") (:file "write-planning")
                 (:file "write-protocol") (:file "recovery")
                 (:file "undo") (:file "tx") (:file "tx-stage")
                 (:file "tx-operations") (:file "tx-commit")))
   (:module "infrastructure"
    :serial t
    :components ((:file "package") (:file "posix-syscall")
                 (:file "posix-file-io") (:file "posix-io")))))
