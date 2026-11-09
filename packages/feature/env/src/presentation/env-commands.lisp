;;;;
;;;; cl-cli definitions and schemas for the `sys` and `time` groups
;;;; (docs/src/reference/commands/env.md). Handlers turn the parsed invocation into a flow call;
;;;; the flow's continuation decides the envelope. The `schema` text
;;;; (summaries, descriptions, args, output fields, error codes) is data:
;;;; AITOOLS.DATA:*ENV-COMMAND-SCHEMAS*.
(in-package #:aitools.env.presentation)

(defmacro define-env-command (name group command-builder)
  `(aitools.protocol.application:define-command
     ,name
     (:group ,group
      :command-builder ,command-builder
      :schema (%schema ,name))))

(defun register-env-commands (registry boundaries)
  "Register the `sys` and `time` commands on REGISTRY using BOUNDARIES,
a cl-boundary-kit boundary context."
  (aitools.protocol.application:register-command-declarations
   registry
   (list
    (define-env-command "sys.info" "sys" #'%sys-info-command)
    (define-env-command "sys.env" "sys" #'%sys-env-command)
    (define-env-command "sys.tools" "sys" #'%sys-tools-command)
    (define-env-command "sys.procs" "sys" #'%sys-procs-command)
    (define-env-command "sys.metrics" "sys" #'%sys-metrics-command)
    (define-env-command "sys.network" "sys" #'%sys-network-command)
    (define-env-command "sys.disk" "sys" #'%sys-disk-command)
    (define-env-command "sys.ports" "sys" #'%sys-ports-command)
    (define-env-command "sys.snapshot" "sys" #'%sys-snapshot-command)
    (define-env-command "time.now" "time" #'%time-now-command)
    (define-env-command "time.convert" "time" #'%time-convert-command)
    (define-env-command "time.diff" "time" #'%time-diff-command))
   boundaries)
  registry)
