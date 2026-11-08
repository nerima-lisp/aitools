(in-package #:aitools.edit.composition)

(defun make-edit-dependencies (&rest arguments)
  (apply #'aitools.edit.infrastructure:make-production-edit-ports arguments))

(aitools/cli:define-context-command-registration :edit #'aitools.edit.presentation:register-edit-commands #'make-edit-dependencies)
