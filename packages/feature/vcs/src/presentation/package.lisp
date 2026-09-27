;;;; packages/feature/vcs/src/presentation/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.vcs.presentation
  (:use #:cl)
  (:import-from #:cl-cli #:make-command #:make-option #:make-positional #:option-value #:positional-value)
  (:export
   #:register-vcs-commands))
