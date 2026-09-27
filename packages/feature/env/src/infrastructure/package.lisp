;;;; packages/feature/env/src/infrastructure/package.lisp
;;;;
;;;; Adapters behind AITOOLS.ENV.APPLICATION:ENV-PORTS.
(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defpackage #:aitools.env.infrastructure
  (:use #:cl)
  (:export
   #:make-env-ports-from-boundaries
   #:make-production-env-ports))
