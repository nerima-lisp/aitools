(in-package #:aitools.org.presentation)

(defun make-org-dependencies (&rest arguments)
  (apply #'aitools.org.infrastructure:make-production-org-ports arguments))

(aitools/cli::define-context-command-registration :org
                                                  #'register-org-commands
                                                  #'make-org-dependencies)
