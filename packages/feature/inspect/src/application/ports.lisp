;;;; packages/feature/inspect/src/application/ports.lisp
;;;;
;;;; The effects inspect flows use, passed explicitly to every flow
;;;; (docs/src/reference/architecture.md, "Ports and the composition root").
;;;; All are other contexts' ports or
;;;; plain functions: the workspace host (root, real paths, stat, scans), the
;;;; text source (file bytes, binary sniffed first), a store opener (tx
;;;; views, read-set records, the journal, and the store's file primitives
;;;; for snapshot records), and the aitools state directory of the current
;;;; invocation.
(in-package #:aitools.inspect.application)

(defstruct (inspect-ports (:constructor make-inspect-ports
                              (&key workspace-host text-source open-store state-directory-function))
                          (:copier nil))
  "WORKSPACE-HOST is an AITOOLS.WORKSPACE.APPLICATION:WORKSPACE-HOST.
TEXT-SOURCE is an AITOOLS.TEXT.APPLICATION:TEXT-SOURCE. OPEN-STORE (real-root)
returns an AITOOLS.STORE.APPLICATION:STORE. STATE-DIRECTORY-FUNCTION () returns
`<state>/<workspace-id>` for the current invocation, or NIL."
  (workspace-host nil :read-only t)
  (text-source nil :read-only t)
  (open-store nil :type (or null function) :read-only t)
  (state-directory-function nil :type (or null function) :read-only t))
