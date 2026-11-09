(in-package #:aitools.md.composition)

(defun make-md-dependencies (&rest arguments)
  (apply #'aitools.md.infrastructure:make-production-md-ports arguments))

(aitools/cli:define-context-command-registration :md
                                                 #'aitools.md.presentation:register-md-commands
                                                 #'make-md-dependencies)
