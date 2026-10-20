;;;; packages/feature/edit/src/domain/transform.lisp
;;;;
;;;; `transform --op` line operations. Each op maps a list of line
;;;; strings to a new list; the whole-file ops (eol-*, *final-newline,
;;;; strip-bom) act on the document and live with the flow. Pure value
;;;; computations, so no continuations.
(in-package #:aitools.edit.domain)
