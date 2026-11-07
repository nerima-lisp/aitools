(in-package #:aitools.edit.presentation)

(defun make-edit-dependencies (&rest arguments)
  (apply #'aitools.edit.infrastructure:make-production-edit-ports arguments))

(aitools/cli::define-context-command-registration :edit
                                                  #'register-edit-commands
                                                  #'make-edit-dependencies)
