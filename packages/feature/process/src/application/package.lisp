;;;; packages/feature/process/src/application/package.lisp
;;;;
;;;; Use cases for `run`, `wait`, and `bg`. Every side effect goes through a
;;;; PROCESS-PORTS value the caller supplies (production adapters from the
;;;; infrastructure layer, fakes in unit tests); every flow reports through
;;;; the command-result contract's on-ok / on-partial / on-error.
(in-package #:cl-user)

(defpackage #:aitools.process.application
  (:use #:cl)
  (:import-from #:aitools.protocol.domain #:repair #:schema-repair)
  (:export
   ;; ports.lisp
   #:process-ports
   #:make-process-ports
   #:process-ports-p
   #:process-port-error
   #:process-port-error-message
   ;; run-flow.lisp
   #:run-request
   #:make-run-request
   #:run-command/k
   ;; wait-flow.lisp
   #:wait-request
   #:make-wait-request
   #:wait-until/k
   #:wait-command/k
   ;; bg-flow.lisp
   #:bg-start/k
   #:bg-logs/k
   #:bg-status/k
   #:bg-stop/k))
