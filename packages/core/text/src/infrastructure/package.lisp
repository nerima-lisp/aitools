;;;; packages/core/text/src/infrastructure/package.lisp
;;;;
;;;; The production TEXT-SOURCE adapter and its cl-boundary-kit registration.
(in-package #:cl-user)

(defpackage #:aitools.text.infrastructure
  (:use #:cl)
  (:import-from #:aitools.text.application
                #:make-text-source)
  (:export
   ;; host-source.lisp
   #:make-host-text-source
   ;; boundaries.lisp
   #:+text-source-boundary+
   #:with-text-boundaries
   #:text-source-from-context))
