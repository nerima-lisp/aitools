;;;;
;;;; Builds every feature context's production ports and registers its
;;;; commands. Only feature contexts have a presentation layer
;;;; (docs/src/reference/architecture.md, "Contexts"). Every
;;;; constructor and registration function is named by its qualified symbol,
;;;; so a renamed or missing one fails the build instead of silently dropping
;;;; that context's commands.
(in-package #:aitools/cli)

(defun %context-dependency-arguments ()
  "Shared keyword arguments used to build each feature context's runtime dependencies."
  (list :state-directory-function #'current-state-directory
        :workspace-host (aitools.workspace.infrastructure:make-host-workspace-host)
        :text-source (aitools.text.infrastructure:make-host-text-source)
        :open-store #'%open-invocation-store
        :with-span #'%call-with-port-span))

(defun %call-with-port-span (kind name operation arguments &key outcome-function)
  (if *trace-session*
      (%call-with-trace-span kind name
                             (lambda () (apply operation arguments))
                             :outcome-function outcome-function)
      (apply operation arguments)))

(defun register-all-context-commands (registry)
  "Register every feature context's commands on REGISTRY using its runtime
  dependencies, in context order (docs/src/reference/architecture.md). Returns REGISTRY."
  (let ((arguments (%context-dependency-arguments)))
    (flet ((register (register-function make-dependencies)
             (funcall register-function
                      registry
                      (apply make-dependencies arguments))))
      (dolist (registration (context-command-registrations))
        (destructuring-bind (context register-function dependency-function)
            registration
          (register register-function dependency-function)))))
  registry)
