;;;; src/app.lisp
;;;;
;;;; Assembles the cl-cli app: every feature context's registered commands,
;;;; plus `schema` (src/schema.lisp) and `batch` (src/batch.lisp).
(in-package #:aitools/cli)

;; Read at compile time: looking the system up at run time makes ASDF reload
;; aitools.asd from the installed sources, which prints redefinition warnings
;; on stderr and adds startup work.
(defparameter +aitools-version+
  #.(asdf:component-version (asdf:find-system "aitools")))

(defun %aitools-version ()
  +aitools-version+)

(defun build-app ()
  "Return (VALUES APP REGISTRY). REGISTRY is kept alongside APP because
DISPATCH needs it too, for `schema` lookups and for `--help` on a
specific command."
  (let ((registry (make-command-registry)))
    (register-all-context-commands registry)
    (register-schema-command registry)
    (register-batch-command registry)
    (values (make-app :name "aitools"
                      :version (%aitools-version)
                      :summary "An AI-agent-oriented file and text CLI."
                      :require-command t
                      :global-options (list (make-option :name "root" :kind :value
                                                         :description "Workspace root; defaults to the git root, then the working directory.")
                                            (make-option :name "lock-timeout" :kind :value
                                                         :description "How long a write waits for the workspace lock (<n>ms|s|m|h|d, default 10s)."))
                      :commands (finalize-app-commands registry))
            registry)))

(defparameter *application*
  (multiple-value-list (build-app))
  "(LIST APP REGISTRY), built once as this file loads so `program-op' bakes the
command registry into the delivered heap instead of MAIN rebuilding it on every
process start (BUILD-APP was ~20% of `--version'). Only the static
command tree is frozen. Every per-invocation fact stays dynamic: the ports hold
function references (#'current-state-directory, #'make-posix-store) and closures
whose adapters read cwd/HOME/XDG_STATE_HOME and the filesystem only when a flow
calls them, so the workspace root (--root/cwd), state directory, and
lock-timeout all resolve at dispatch time and the same image behaves identically
from any cwd, root, or environment.")
