(in-package #:aitools.search.presentation)

(defun make-search-dependencies (&rest arguments)
  (apply #'aitools.search.infrastructure:make-production-search-ports arguments))

(aitools/cli::define-context-command-registration :search
                                                  #'register-search-commands
                                                  #'make-search-dependencies)
