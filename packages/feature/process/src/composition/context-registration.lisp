(in-package #:aitools.process.composition)

(defun make-process-dependencies (&rest arguments)
  (apply #'aitools.process.infrastructure:make-production-process-boundaries
         arguments))

(aitools/cli:define-context-command-registration :process #'aitools.process.presentation:register-process-commands #'make-process-dependencies)
