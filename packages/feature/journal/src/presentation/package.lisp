;;;; packages/feature/journal/src/presentation/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.journal.presentation
  (:use #:cl)
  (:import-from #:cl-cli #:make-command #:make-option #:make-positional #:option-value #:positional-value)
  (:export
   #:register-journal-commands))
