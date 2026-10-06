;;;; packages/feature/env/src/application/package.lisp
;;;;
;;;; Use cases of the env context: the `sys` and `time` flows, each a /k
;;;; function over an ENV-PORTS record, calling one of the command-result
;;;; continuations.
(in-package #:cl-user)

(defpackage #:aitools.env.application
  (:use #:cl)
  (:import-from #:aitools.protocol.domain #:repair)
  (:export
   ;; ports.lisp
   #:env-ports
   #:env-ports-p
   #:make-env-ports
   ;; time-flows.lisp
   #:local-zone-name
   #:resolve-zone/k
   #:time-now/k
   #:time-convert/k
   #:time-diff/k
   ;; sys-flows.lisp
   #:find-executable
   #:sys-info/k
   #:sys-env/k
   #:sys-tools/k
   #:sys-procs/k
   #:sys-ports/k))
