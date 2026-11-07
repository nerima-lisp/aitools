(in-package #:aitools.journal.composition)

(defun make-journal-dependencies (&rest arguments)
  (apply #'aitools.journal.infrastructure:make-production-journal-ports
         arguments))

(aitools/cli::define-context-command-registration :journal #'aitools.journal.presentation:register-journal-commands #'make-journal-dependencies)
