;;;; src/context-registration.lisp
;;;;
;;;; Builds every feature context's production ports and registers its
;;;; commands. Only feature contexts have a presentation layer
;;;; (docs/src/reference/architecture.md, "Contexts"). Every
;;;; constructor and registration function is named by its qualified symbol,
;;;; so a renamed or missing one fails the build instead of silently dropping
;;;; that context's commands.
(in-package #:aitools/cli)

(defun %port-arguments ()
  "The shared keyword arguments every `make-production-<context>-ports'
accepts. The composition root is the only place allowed to see both
presentation and infrastructure, so ports are injected here rather than
published by infrastructure through a global."
  (list :state-directory-function #'current-state-directory
        :workspace-host (aitools.workspace.infrastructure:make-host-workspace-host)
        :text-source (aitools.text.infrastructure:make-host-text-source)
        :open-store #'aitools.store.infrastructure:make-posix-store))

(defun register-all-context-commands (registry)
  "Register every feature context's commands on REGISTRY with its production
ports, in context order (docs/src/reference/architecture.md). Returns REGISTRY."
  (let ((arguments (%port-arguments)))
    (flet ((register (register-function make-ports)
             (funcall register-function registry (apply make-ports arguments))))
      (register #'aitools.search.presentation:register-search-commands
                #'aitools.search.infrastructure:make-production-search-ports)
      (register #'aitools.inspect.presentation:register-inspect-commands
                #'aitools.inspect.infrastructure:make-production-inspect-ports)
      (register #'aitools.edit.presentation:register-edit-commands
                #'aitools.edit.infrastructure:make-production-edit-ports)
      (register #'aitools.journal.presentation:register-journal-commands
                #'aitools.journal.infrastructure:make-production-journal-ports)
      (register #'aitools.process.presentation:register-process-commands
                #'aitools.process.infrastructure:make-production-process-ports)
      (register #'aitools.vcs.presentation:register-vcs-commands
                #'aitools.vcs.infrastructure:make-production-vcs-ports)
      (register #'aitools.env.presentation:register-env-commands
                #'aitools.env.infrastructure:make-production-env-ports)
      (register #'aitools.util.presentation:register-util-commands
                #'aitools.util.infrastructure:make-production-util-ports)))
  registry)
