;;;; packages/core/kernel/src/domain/json-model-resolve.lisp
;;;; CPS JSON Pointer resolution over a value model.
(in-package #:aitools.kernel.domain)

(define-flow
 (json-model-resolve/k model document tokens &key on-found on-missing)
 (declare (type function on-found on-missing))
 (let ((value document)
       (missing nil))
   (loop for token in tokens
         for depth from 0
         do (multiple-value-bind (child present)
                (json-model-child model value token)
              (unless present
                (setf missing (list (subseq tokens 0 depth) value token))
                (return))
              (setf value child)))
   (if missing
       (flow-continue :on-missing
                      (first missing)
                      (second missing)
                      (third missing))
       (flow-continue :on-found value))))
