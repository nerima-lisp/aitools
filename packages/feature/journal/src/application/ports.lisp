;;;; packages/feature/journal/src/application/ports.lisp
;;;;
;;;; The journal context's side-effect boundary is the store itself: the
;;;; flows resolve the workspace root through the workspace context's host
;;;; and open the STORE for it. JOURNAL-CONTEXT carries the global options
;;;; of one invocation (`--root`, `--lock-timeout`), which also reappear in
;;;; repair commands.
(in-package #:aitools.journal.application)

(defstruct (journal-ports (:constructor make-journal-ports (&key workspace-host open-store))
                          (:copier nil))
  ;; An AITOOLS.WORKSPACE.APPLICATION:WORKSPACE-HOST for workspace root
  ;; resolution, or NIL when none was wired in.
  (workspace-host nil :read-only t)
  ;; (REAL-ROOT) -> the AITOOLS.STORE.APPLICATION:STORE of that workspace,
  ;; or NIL when none was wired in.
  (open-store nil :type (or null function) :read-only t))

(defstruct (journal-context (:constructor make-journal-context (&key root lock-timeout))
                            (:copier nil))
  ;; The global `--root` and `--lock-timeout` texts as given, or NIL.
  (root nil :type (or null string) :read-only t)
  (lock-timeout nil :type (or null string) :read-only t))
