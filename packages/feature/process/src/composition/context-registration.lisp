(in-package #:aitools.process.composition)

(defun make-process-dependencies (&rest arguments)
  (let ((context (cl-boundary-kit:make-boundary-context)))
    (dolist (capability
             (apply (function aitools.process.infrastructure:make-production-process-capabilities)
                    arguments))
      #+linux
      (unless (eq (car capability) :process-signal-process)
        (setf context
              (cl-boundary-kit:boundary-context-with
               context (car capability) (cdr capability))))
      #+darwin
      (setf context
            (cl-boundary-kit:boundary-context-with
             context (car capability) (cdr capability))))
    #+linux
    (setf context
          (cl-boundary-kit:boundary-context-with
           context :process-signal-process
           (aitools.process.infrastructure::%make-signal-process-boundary context)))
    context))

(aitools/cli:define-context-command-registration :process #'aitools.process.presentation:register-process-commands #'make-process-dependencies)
