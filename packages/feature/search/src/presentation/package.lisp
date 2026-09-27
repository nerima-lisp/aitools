;;;; packages/feature/search/src/presentation/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.search.presentation
  (:use #:cl)
  (:import-from #:cl-cli #:make-command #:make-option #:make-positional #:option-value #:positional-value)
  (:export
   #:register-search-commands))
