;;;; t/support/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.test.support
  (:use #:cl)
  (:export #:call-with-temporary-directory
   #:call-with-workspace
   #:envelope-value
   #:json-alist
   #:json-alist-value
   #:read-bytes
   #:read-text
   #:run-aitools
   #:skip-unless-tools
   #:string-bytes
   #:to-match-envelope
   #:tool-path
   #:with-temporary-directory
   #:with-workspace
   #:write-bytes
   #:write-text
   #:with-stdin-from))
