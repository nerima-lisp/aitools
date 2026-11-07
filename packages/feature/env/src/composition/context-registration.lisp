(in-package #:aitools.env.composition)

(defun make-env-dependencies (&rest arguments)
  (apply #'aitools.env.infrastructure:make-production-env-ports arguments))

(aitools/cli::define-context-command-registration :env #'aitools.env.presentation:register-env-commands #'make-env-dependencies)
