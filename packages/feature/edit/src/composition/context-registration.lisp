(in-package #:aitools.edit.composition)

(defun make-edit-dependencies (&key workspace-host open-store text-source &allow-other-keys)
  (let ((boundaries (cl-boundary-kit:make-boundary-context)))
    (dolist (capability
             (list (cons :edit-workspace-host workspace-host)
                   (cons :edit-open-store open-store)
                   (cons :edit-text-source text-source)
                   (cons :edit-read-stdin-octets
                         (function aitools.edit.infrastructure:read-stdin-octets))
                   (cons :edit-unix-now
                         (function aitools.edit.infrastructure:unix-now))
                   (cons :edit-serialize-document
                         (function aitools.edit.infrastructure:serialize-document))))
      (setf boundaries
            (cl-boundary-kit:boundary-context-with
             boundaries (car capability) (cdr capability))))
    boundaries))

(aitools/cli:define-context-command-registration :edit #'aitools.edit.presentation:register-edit-commands #'make-edit-dependencies)
