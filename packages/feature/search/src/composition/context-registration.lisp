(in-package #:aitools.search.composition)

(defun make-search-dependencies (&rest arguments)
  (apply #'aitools.search.infrastructure:make-production-search-ports arguments))

(aitools/cli::define-context-command-registration :search #'aitools.search.presentation:register-search-commands #'make-search-dependencies)
