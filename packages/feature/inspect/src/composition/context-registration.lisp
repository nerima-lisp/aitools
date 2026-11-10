(in-package #:aitools.inspect.composition)

(defun make-inspect-dependencies
    (&key state-directory-function workspace-host open-store
          ((:text-source text-context))
          &allow-other-keys)
  (let ((boundaries (cl-boundary-kit:make-boundary-context)))
    (dolist (capability
             (list (cons :inspect-workspace-host workspace-host)
                   (cons :inspect-text-context text-context)
                   (cons :inspect-open-store open-store)
                   (cons :inspect-state-directory state-directory-function)))
      (setf boundaries
            (cl-boundary-kit:boundary-context-with
             boundaries (car capability) (cdr capability))))
    boundaries))

(aitools/cli:define-context-command-registration :inspect
                                                 #'aitools.inspect.presentation:register-inspect-commands
                                                 #'make-inspect-dependencies)
