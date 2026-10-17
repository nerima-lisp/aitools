(in-package #:asdf-user)

(defsystem "aitools/core/store"
  :description "Crash-safe storage and transaction primitives."
  :depends-on ("aitools/core/kernel" "aitools/core/protocol"
               "aitools/core/text" "cl-json-kit" "cl-host-kit"
               "cl-boundary-kit" "cl-concurrent-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "entry-state") (:file "layout")
                 (:file "change") (:file "plan") (:file "journal")
                 (:file "intent") (:file "tx-model")
                 (:file "changes-json")))
   (:module "application"
    :components ((:file "package") (:file "port") (:file "lock")
                 (:file "files") (:file "journal") (:file "blobs")
                 (:file "view") (:file "write-completion")
                 (:file "write-preparation") (:file "write-protocol")
                 (:file "recovery") (:file "undo") (:file "tx")
                 (:file "tx-stage") (:file "tx-commit")))
   (:module "infrastructure"
    :components ((:file "package") (:file "posix-syscall")
                 (:file "posix-file-io") (:file "posix-io")))))
