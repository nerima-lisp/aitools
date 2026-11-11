(in-package #:aitools.util.composition)

(defun make-util-dependencies (&key workspace-host open-store &allow-other-keys)
  (let ((boundaries (cl-boundary-kit:make-boundary-context)))
    (dolist (capability
             (append
              (aitools.util.infrastructure:make-production-util-capabilities)
              (list (cons :util-workspace-host workspace-host)
                    (cons :util-open-store open-store))))
      (setf boundaries
            (cl-boundary-kit:boundary-context-with
             boundaries (car capability) (cdr capability))))
    boundaries))

(aitools/cli:define-context-command-registration :util #'aitools.util.presentation:register-util-commands #'make-util-dependencies)
