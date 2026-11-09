(in-package #:cl-user)
(defpackage #:aitools.net.presentation
  (:use #:cl)
  (:import-from #:cl-cli
                #:make-command
                #:make-option
                #:make-positional
                #:option-value
                #:positional-value)
  (:export #:register-net-commands))
