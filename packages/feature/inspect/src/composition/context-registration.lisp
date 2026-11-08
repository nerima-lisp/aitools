(in-package #:aitools.inspect.composition)

(defun make-inspect-dependencies (&rest arguments)
  (apply #'aitools.inspect.infrastructure:make-production-inspect-ports
         arguments))

(aitools/cli:define-context-command-registration :inspect #'aitools.inspect.presentation:register-inspect-commands #'make-inspect-dependencies)
