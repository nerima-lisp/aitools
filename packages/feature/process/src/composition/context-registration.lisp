(in-package #:aitools.process.composition)

(defun make-process-dependencies (&rest arguments)
  (let ((context (cl-boundary-kit:make-boundary-context)))
    (dolist (capability
             (apply (function aitools.process.infrastructure:make-production-process-capabilities)
                    arguments))
      (setf context
            (cl-boundary-kit:boundary-context-with
             context (car capability) (cdr capability))))
    context))

(aitools/cli:define-context-command-registration :process #'aitools.process.presentation:register-process-commands #'make-process-dependencies)
