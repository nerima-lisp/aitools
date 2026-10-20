(in-package #:cl-user)

(defpackage #:aitools.org.presentation
  (:use #:cl)
  (:import-from #:cl-cli
                #:make-command #:make-option #:make-positional
                #:option-value #:positional-value)
  (:export #:register-org-commands))
