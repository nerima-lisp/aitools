;;;;
;;;; Production adapters installed in AITOOLS.PROCESS.APPLICATION boundary contexts:
;;;; process-kit for `run` and `bg start`, sb-posix for process-group
;;;; signals, sb-bsd-sockets for `wait --port`, and plain CL file I/O for the
;;;; `bg/` directory.
(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix)
  (require :sb-bsd-sockets))

(defpackage #:aitools.process.infrastructure
  (:use #:cl)
  (:export
   ;; bg-launcher.lisp
   #:find-spawn-trampoline
   ;; boundaries.lisp
   #:make-production-process-capabilities
   #:make-production-process-boundaries))
