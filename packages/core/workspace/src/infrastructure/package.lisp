;;;; packages/core/workspace/src/infrastructure/package.lisp
;;;;
;;;; Production adapters for the workspace context's WORKSPACE-HOST port,
;;;; and their registration as a cl-boundary-kit boundary for the
;;;; composition root.
(in-package #:cl-user)

(defpackage #:aitools.workspace.infrastructure
  (:use #:cl)
  (:import-from #:aitools.workspace.domain
                #:make-workspace-entry)
  (:import-from #:aitools.workspace.application
                #:make-workspace-host)
  (:export
   ;; host.lisp
   #:read-regular-file-octets
   #:make-host-workspace-host
   ;; ordered-mapper.lisp
   #:processor-count
   #:call-with-ordered-mapper
   ;; boundaries.lisp
   #:+workspace-host-boundary+
   #:with-workspace-boundaries
   #:workspace-host-from-context))
