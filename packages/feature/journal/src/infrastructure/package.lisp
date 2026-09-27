;;;; packages/feature/journal/src/infrastructure/package.lisp
;;;;
;;;; The journal context has no adapter of its own: its only side effects go
;;;; through the store, which the composition root supplies as a function.
(in-package #:cl-user)

(defpackage #:aitools.journal.infrastructure
  (:use #:cl)
  (:export
   #:make-production-journal-ports))
