(in-package #:aitools.env.composition)

(defun make-env-dependencies (&rest arguments)
  (apply (function aitools.env.infrastructure:make-production-env-boundaries) arguments))

(aitools/cli:define-context-command-registration :env #'aitools.env.presentation:register-env-commands #'make-env-dependencies)
