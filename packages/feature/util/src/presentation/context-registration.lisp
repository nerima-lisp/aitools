(in-package #:aitools.util.presentation)

(defun make-util-dependencies (&rest arguments)
  (apply #'aitools.util.infrastructure:make-production-util-ports arguments))

(aitools/cli::define-context-command-registration :util
                                                  #'register-util-commands
                                                  #'make-util-dependencies)
