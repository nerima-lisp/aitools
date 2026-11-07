(in-package #:aitools.process.presentation)

(defun make-process-dependencies (&rest arguments)
  (apply #'aitools.process.infrastructure:make-production-process-ports
         arguments))

(aitools/cli::define-context-command-registration :process
                                                  #'register-process-commands
                                                  #'make-process-dependencies)
