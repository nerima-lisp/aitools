(defun aitools.util.composition::make-util-dependencies (&rest arguments)
  (apply #'aitools.util.infrastructure:make-production-util-ports arguments))
(defun aitools.util.composition::register-util-context-commands ()
  (aitools/cli::define-context-command-registration
   :util
   #'aitools.util.presentation:register-util-commands
   #'aitools.util.composition::make-util-dependencies))
(aitools.util.composition::register-util-context-commands)

