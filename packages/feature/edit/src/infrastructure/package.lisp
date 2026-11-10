;;;;
;;;; Production adapters for the edit context's boundary capabilities: standard input as
;;;; bytes, a file's modification time, the clock. The workspace host, the
;;;; store constructor and the text source come from the composition root.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(in-package #:cl-user)

(defpackage #:aitools.edit.infrastructure
  (:use #:cl)
  (:export
   #:read-stdin-octets
   #:unix-now
   #:serialize-document))
