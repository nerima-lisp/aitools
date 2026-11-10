(in-package #:aitools.journal.composition)

(defun make-journal-dependencies (&key workspace-host open-store &allow-other-keys)
  (let ((boundaries (cl-boundary-kit:make-boundary-context)))
    (dolist (capability
             (list (cons :journal-workspace-host workspace-host)
                   (cons :journal-open-store open-store)))
      (setf boundaries
            (cl-boundary-kit:boundary-context-with
             boundaries (car capability) (cdr capability))))
    boundaries))

(aitools/cli:define-context-command-registration :journal #'aitools.journal.presentation:register-journal-commands #'make-journal-dependencies)
