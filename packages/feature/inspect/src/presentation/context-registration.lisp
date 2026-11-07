(in-package #:aitools.inspect.presentation)

(defun make-inspect-dependencies (&rest arguments)
  (apply #'aitools.inspect.infrastructure:make-production-inspect-ports
         arguments))

(aitools/cli::define-context-command-registration :inspect
                                                  #'register-inspect-commands
                                                  #'make-inspect-dependencies)
