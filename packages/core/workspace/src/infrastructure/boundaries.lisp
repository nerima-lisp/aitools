;;;; packages/core/workspace/src/infrastructure/boundaries.lisp
;;;;
;;;; The composition root keeps every production port in one
;;;; cl-boundary-kit boundary context; this file adds and looks up the
;;;; workspace context's entry. Flows still take the host explicitly.
(in-package #:aitools.workspace.infrastructure)

(defconstant +workspace-host-boundary+ :workspace-host
  "The boundary-context key holding the WORKSPACE-HOST.")

(defun with-workspace-boundaries (context &key (host (make-host-workspace-host)))
  "A boundary context derived from CONTEXT with HOST under
+WORKSPACE-HOST-BOUNDARY+."
  (cl-boundary-kit:boundary-context-with context +workspace-host-boundary+ host))

(defun workspace-host-from-context (context)
  "The WORKSPACE-HOST in CONTEXT; signals when it was never installed."
  (cl-boundary-kit:boundary-context-require context +workspace-host-boundary+))
