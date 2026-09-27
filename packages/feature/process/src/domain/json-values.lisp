;;;; packages/feature/process/src/domain/json-values.lisp
;;;;
;;;; json-kit writes NIL as `[]`, so a JSON `false` or `null` has to be one
;;;; of its sentinels. The application and presentation layers may not name
;;;; json-kit (docs/src/reference/architecture.md), so every result value that can be false or
;;;; null is built through these helpers.
(in-package #:aitools.process.domain)

(defun json-or-null (value)
  (if (null value) json-kit:+json-null+ value))
