;;;; data/package.lisp
;;;;
;;;; Data is kept separate from logic: every `data/**/*-data.lisp` file
;;;; interns its constants into this one shared package rather than each
;;;; context's own domain package, because a :DATA component is loaded
;;;; before its context's :LIBRARY (docs/src/project/development.md, "The
;;;; component lists") -- before that context's own
;;;; DOMAIN/PACKAGE.LISP has run, so there is no context-specific package yet
;;;; to intern into.
;;;;
;;;; Each data file EXPORTs its own symbols at its own end (see e.g.
;;;; error-catalog-data.lisp) instead of listing them in a shared
;;;; DEFPACKAGE :EXPORT clause here, so that adding a new data file never
;;;; requires editing this file -- consistent with "each writer owns only
;;;; its context's components.sexp (plus its dirs)".
(in-package #:cl-user)

(defpackage #:aitools.data
  (:use #:cl))
