(in-package #:cl-user)

(defpackage #:aitools.process.presentation
  (:use #:cl)
  (:import-from #:cl-cli #:make-command #:make-option #:make-positional #:option-value #:positional-value)
  (:export
   #:register-process-commands))
