(in-package #:aitools.env.presentation)

(defun make-env-dependencies (&rest arguments)
  (apply #'aitools.env.infrastructure:make-production-env-ports arguments))

(aitools/cli::define-context-command-registration :env
                                                  #'register-env-commands
                                                  #'make-env-dependencies)
