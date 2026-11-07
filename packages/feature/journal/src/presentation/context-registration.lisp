(in-package #:aitools.journal.presentation)

(defun make-journal-dependencies (&rest arguments)
  (apply #'aitools.journal.infrastructure:make-production-journal-ports
         arguments))

(aitools/cli::define-context-command-registration :journal
                                                  #'register-journal-commands
                                                  #'make-journal-dependencies)
