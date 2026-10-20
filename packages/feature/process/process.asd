(in-package #:asdf-user)

(defsystem "aitools/feature/process"
  :description "Foreground and background process features."
  :depends-on ("aitools/data"
               "aitools/core/protocol" "aitools/core/text"
               "cl-json-kit" "cl-host-kit" "cl-process-kit"
               "cl-concurrent-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "line-pattern")
                 (:file "terminal-text") (:file "output-report")
                 (:file "process-outcome") (:file "bg-record")
                 (:file "log-slice") (:file "wait-condition")
                 (:file "signal-target")))
   (:module "application" :serial t
    :components ((:file "package")
                 (:file "ports")
                 (:file "common")
                 (:file "run-flow")
                 (:file "bg-flow")
                 (:file "bg-query-flow")
                 (:file "bg-stop-flow")
                 (:file "wait-flow")
                 (:file "signal-flow")))
   (:module "infrastructure" :serial t
    :components ((:file "package") (:file "host")
                 (:file "signal-host-helpers") (:file "signal-host")
                 (:file "runner") (:file "bg-launcher") (:file "ports")))))
