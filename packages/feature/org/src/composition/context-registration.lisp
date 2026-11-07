(in-package #:aitools.org.composition)

(defun make-org-dependencies (&rest arguments)
  (apply #'aitools.org.infrastructure:make-production-org-ports arguments))

(aitools/cli::define-context-command-registration :org #'aitools.org.presentation:register-org-commands #'make-org-dependencies)
