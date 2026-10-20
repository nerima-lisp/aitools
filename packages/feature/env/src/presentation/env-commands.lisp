;;;; packages/feature/env/src/presentation/env-commands.lisp
;;;;
;;;; cl-cli definitions and schemas for the `sys` and `time` groups
;;;; (docs/src/reference/commands.md). Handlers turn the parsed invocation into a flow call;
;;;; the flow's continuation decides the envelope. The `schema` text
;;;; (summaries, descriptions, args, output fields, error codes) is data:
;;;; AITOOLS.DATA:*ENV-COMMAND-SCHEMAS*.
(in-package #:aitools.env.presentation)

(defun register-env-commands (registry ports)
  "Register the `sys` and `time` commands on REGISTRY, running their flows
against PORTS (an AITOOLS.ENV.APPLICATION:ENV-PORTS)."
  (aitools.protocol.application:register-command-declarations
   registry
   (list
    (aitools.protocol.application:define-command
        "sys.info"
      (:group "sys"
       :command-builder #'%sys-info-command
       :schema (%schema "sys.info")))
    (aitools.protocol.application:define-command
        "sys.env"
      (:group "sys"
       :command-builder #'%sys-env-command
       :schema (%schema "sys.env")))
    (aitools.protocol.application:define-command
        "sys.tools"
      (:group "sys"
       :command-builder #'%sys-tools-command
       :schema (%schema "sys.tools")))
    (aitools.protocol.application:define-command
        "sys.procs"
      (:group "sys"
       :command-builder #'%sys-procs-command
       :schema (%schema "sys.procs")))
    (aitools.protocol.application:define-command
        "sys.metrics"
      (:group "sys"
       :command-builder #'%sys-metrics-command
       :schema (%schema "sys.metrics")))
    (aitools.protocol.application:define-command
        "sys.network"
      (:group "sys"
       :command-builder #'%sys-network-command
       :schema (%schema "sys.network")))
    (aitools.protocol.application:define-command
        "sys.disk"
      (:group "sys"
       :command-builder #'%sys-disk-command
       :schema (%schema "sys.disk")))
    (aitools.protocol.application:define-command
        "sys.ports"
      (:group "sys"
       :command-builder #'%sys-ports-command
       :schema (%schema "sys.ports")))
    (aitools.protocol.application:define-command
        "sys.snapshot"
      (:group "sys"
       :command-builder #'%sys-snapshot-command
       :schema (%schema "sys.snapshot")))
    (aitools.protocol.application:define-command
        "time.now"
      (:group "time"
       :command-builder #'%time-now-command
       :schema (%schema "time.now")))
    (aitools.protocol.application:define-command
        "time.convert"
      (:group "time"
       :command-builder #'%time-convert-command
       :schema (%schema "time.convert")))
    (aitools.protocol.application:define-command
        "time.diff"
      (:group "time"
       :command-builder #'%time-diff-command
       :schema (%schema "time.diff"))))
   ports)
  registry)
