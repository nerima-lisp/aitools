;;;; t/e2e/package.lisp
;;;;
;;;; The end-to-end suite: every row of the correspondence table
;;;; (docs/src/guide/agents.md) runs the real aitools executable in a fresh workspace and compares
;;;; its JSON result with the shell command the row replaces.
(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix)
  (require :sb-bsd-sockets))

(defpackage #:aitools.e2e.test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect #:expect-not #:fail #:skip))
