(in-package #:asdf-user)

(defsystem "aitools/feature/process"
  :description "Foreground and background process features."
  :depends-on ("aitools/core/protocol" "aitools/core/text"
               "cl-json-kit" "cl-host-kit" "cl-process-kit"
               "cl-concurrent-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "json-values")
                 (:file "shell-words") (:file "line-pattern")
                 (:file "terminal-text") (:file "output-report")
                 (:file "process-outcome") (:file "bg-record")
                 (:file "log-slice") (:file "wait-condition")))
   (:module "application"
    :components ((:file "package") (:file "ports") (:file "common")
                 (:file "run-flow") (:file "bg-flow") (:file "wait-flow")))
   (:module "infrastructure"
    :components ((:file "package") (:file "host") (:file "runner")
                 (:file "bg-launcher") (:file "ports")))))
