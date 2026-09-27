;;;; packages/core/text/src/infrastructure/boundaries.lisp
;;;;
;;;; Registration of the TEXT-SOURCE in the composition root's
;;;; cl-boundary-kit boundary context. Flows still take the source
;;;; explicitly.
(in-package #:aitools.text.infrastructure)

(defconstant +text-source-boundary+ :text-source
  "The boundary-context key holding the TEXT-SOURCE.")

(defun with-text-boundaries (context &key (source (make-host-text-source)))
  "A boundary context derived from CONTEXT with SOURCE under
+TEXT-SOURCE-BOUNDARY+."
  (cl-boundary-kit:boundary-context-with context +text-source-boundary+ source))

(defun text-source-from-context (context)
  "The TEXT-SOURCE in CONTEXT; signals when it was never installed."
  (cl-boundary-kit:boundary-context-require context +text-source-boundary+))
