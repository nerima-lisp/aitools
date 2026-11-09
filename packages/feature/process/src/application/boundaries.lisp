;;;;
;;;; The process context's capability keys and I/O error conditions. Production
;;;; and test adapters install their functions in a cl-boundary-kit context.
;;;; WORKSPACE-HOST carries optional workspace boundary data. Adapters signal
;;;; PROCESS-PORT-ERROR for I/O failures, which flows report as `environment.io`.
(in-package #:aitools.process.application)

(define-condition process-port-error
  (error)
  ((message :initarg :message :reader process-port-error-message))
  (:report
   (lambda (condition stream)
     (write-string (process-port-error-message condition) stream))))

(define-condition process-target-changed
  (process-port-error)
  ())

(defmacro process-boundary-call (boundaries key &rest arguments)
  `(funcall (cl-boundary-kit:boundary-context-require ,boundaries ,key)
            ,@arguments))
