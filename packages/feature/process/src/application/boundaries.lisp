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

(progn
  (defparameter +process-boundary-keys+
    '(:process-run-program :process-bg-directory
                           :process-launch-detached
                           :process-list-directory
                           :process-read-file-text
                           :process-read-file-octets
                           :process-file-size
                           :process-create-file-exclusive
                           :process-replace-file
                           :process-remove-file
                           :process-group-alive-p
                           :process-signal-group
                           :process-list-pids
                           :process-process-info
                           :process-safe-target-p
                           :process-signal-process
                           :process-tcp-connectable-p
                           :process-universal-time
                           :process-monotonic-ms
                           :process-sleep-ms
                           :process-workspace-host
                           :process-temporary-directory))
  (defmacro process-boundary-call (boundaries key &rest arguments)
    `(funcall (cl-boundary-kit:boundary-context-require ,boundaries ,key)
              ,@arguments)))
