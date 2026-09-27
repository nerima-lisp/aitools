;;;; t/support/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.test.support
  (:use #:cl)
  (:export
   #:json-alist
   #:json-alist-value
   #:string-bytes))
