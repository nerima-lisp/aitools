;;;; packages/feature/util/src/infrastructure/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.util.infrastructure
  (:use #:cl)
  (:export
   ;; os-random.lisp
   #:os-random-source
   #:make-os-random-source
   ;; ports.lisp
   #:make-util-ports-from-boundaries
   #:make-production-util-ports
   #:read-file-octets
   #:read-stdin-octets))
