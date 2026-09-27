;;;; packages/feature/util/src/presentation/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.util.presentation
  (:use #:cl)
  (:import-from #:cl-cli
                #:make-command #:make-option #:make-positional #:option-value #:positional-value)
  (:export #:register-util-commands))
