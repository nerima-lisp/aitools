(in-package #:aitools.util.composition)

(defun make-util-dependencies (&rest arguments)
  (apply #'aitools.util.infrastructure:make-production-util-ports arguments))

(aitools/cli:define-context-command-registration :util #'aitools.util.presentation:register-util-commands #'make-util-dependencies)
