;;;;
;;;; Registers commands for each composition registration loaded by its feature
;;;; presentation system. Context order follows the CLI presentation dependencies.
(in-package #:aitools/cli)

(defun %context-dependency-arguments ()
  "Shared keyword arguments used to build each feature context's runtime dependencies."
  (list :state-directory-function #'current-state-directory
        :workspace-host (aitools.workspace.infrastructure:make-host-workspace-boundaries)
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
  "Register commands for every feature context using runtime dependencies, in
  context order (docs/src/reference/architecture.md). Returns REGISTRY."
  (let ((arguments (%context-dependency-arguments)))
    (flet ((register (register-function make-dependencies)
             (funcall register-function registry (apply make-dependencies arguments))))
      (dolist (registration (context-command-registrations))
        (register (second registration) (third registration)))))
  registry)
