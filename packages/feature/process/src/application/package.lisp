;;;;
;;;; Use cases for `run`, `wait`, and `bg`. Every side effect uses a process
;;;; boundary context supplied by the caller (production adapters from the
;;;; infrastructure layer, fakes in unit tests); every flow reports through
;;;; the command-result contract's on-ok / on-partial / on-error.
(in-package #:cl-user)

(defpackage #:aitools.process.application
  (:use #:cl)
  (:import-from #:aitools.protocol.domain #:repair #:schema-repair)
  (:export
   process-port-error
   process-port-error-message
   #:run-request
   #:make-run-request
   #:run-command/k
   #:wait-request
   #:make-wait-request
   #:wait-until/k
   #:wait-command/k
   #:bg-start/k
   #:bg-logs/k
   #:bg-status/k
   #:bg-stop/k
   #:signal-command/k))
