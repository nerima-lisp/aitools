(in-package #:aitools.org.composition)

(defun make-org-dependencies (&rest arguments)
  (let ((boundaries (cl-boundary-kit:make-boundary-context)))
    (dolist (capability
             (apply (function
                     aitools.org.infrastructure:make-production-org-capabilities)
                    arguments))
      (setf boundaries
            (cl-boundary-kit:boundary-context-with
             boundaries (car capability) (cdr capability))))
    boundaries))

(aitools/cli:define-context-command-registration :org #'aitools.org.presentation:register-org-commands #'make-org-dependencies)
