(in-package #:aitools.search.composition)

(defun make-search-dependencies (&key workspace-host text-source open-store &allow-other-keys)
  (let ((boundaries (cl-boundary-kit:make-boundary-context)))
    (dolist (capability
             (append (list (cons :search-workspace-host workspace-host)
                           (cons :search-text-context text-source)
                           (cons :search-open-store open-store))
                     (aitools.search.infrastructure:make-production-search-capabilities)))
      (setf boundaries
            (cl-boundary-kit:boundary-context-with
             boundaries (car capability) (cdr capability))))
    boundaries))

(aitools/cli:define-context-command-registration :search #'aitools.search.presentation:register-search-commands #'make-search-dependencies)
