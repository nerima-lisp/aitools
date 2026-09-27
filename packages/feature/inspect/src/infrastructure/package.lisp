;;;; packages/feature/inspect/src/infrastructure/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.inspect.infrastructure
  (:use #:cl)
  (:import-from #:aitools.inspect.application #:make-inspect-ports)
  (:export
   #:make-production-inspect-ports))
