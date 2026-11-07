;;;;
;;;; Production filesystem capabilities for workspace boundary contexts and
;;;; their ordered mapper.
(in-package #:cl-user)

(defpackage #:aitools.workspace.infrastructure
  (:use #:cl)
  (:import-from #:aitools.workspace.domain
                #:make-workspace-entry)
  (:import-from #:aitools.workspace.application
                #:make-workspace-boundaries)
  (:export
   ;; host.lisp
   #:read-regular-file-octets
   #:make-host-workspace-boundaries
   ;; ordered-mapper.lisp
   #:processor-count
   #:call-with-ordered-mapper))
