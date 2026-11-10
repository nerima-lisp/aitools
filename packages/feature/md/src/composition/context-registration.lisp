(in-package #:aitools.md.composition)

(defun make-md-dependencies (&key workspace-host text-source open-store &allow-other-keys)
  (let ((boundaries (cl-boundary-kit:make-boundary-context)))
    (dolist (capability
             (list (cons :md-workspace-host workspace-host)
                   (cons :md-text-source text-source)
                   (cons :md-open-store open-store)
                   (cons :md-read-stdin-octets
                         (function aitools.md.infrastructure:read-stdin-octets))
                   (cons :md-parse-markdown
                         (function aitools.md.infrastructure:parse-md-octets))
                   (cons :md-splice-section
                         (function aitools.md.infrastructure::splice-section-bytes))))
      (setf boundaries
            (cl-boundary-kit:boundary-context-with
             boundaries (car capability) (cdr capability))))
    boundaries))

(aitools/cli:define-context-command-registration :md
                                                 #'aitools.md.presentation:register-md-commands
                                                 #'make-md-dependencies)
