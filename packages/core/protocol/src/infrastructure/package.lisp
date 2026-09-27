;;;; packages/core/protocol/src/infrastructure/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.protocol.infrastructure
  (:use #:cl)
  (:export
   #:write-envelope
   #:envelope-too-large
   #:envelope-too-large-limit
   #:+envelope-max-length+))
