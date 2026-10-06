;;;; packages/feature/edit/src/application/ports.lisp
;;;;
;;;; The effects the edit flows need beyond the store, as plain closures
;;;; (the infrastructure layer builds them; unit tests pass their own).
(in-package #:aitools.edit.application)

(defconstant +max-input-bytes+ (* 256 1024 1024)
  "Largest --stdin or --content-file input read.")

(defun unix-seconds-from-universal-time (universal-time)
  (aitools.kernel.domain:universal-time-to-unix-seconds universal-time))

(defstruct (edit-ports (:constructor make-edit-ports
                           (&key workspace-host open-store text-source read-stdin-octets unix-now))
                       (:copier nil))
  "WORKSPACE-HOST: the workspace context's host (root resolution, the workspace boundary,
scans). OPEN-STORE (real-root) -> a store for that workspace. TEXT-SOURCE:
the text context's reader, for --content-file and --stdin-free inputs.
READ-STDIN-OCTETS (limit &key on-octets on-too-large on-failure) reads
standard input, called only for --stdin. UNIX-NOW () -> Unix seconds
(`touch`'s default time, mktemp's age cutoff)."
  (workspace-host nil :read-only t)
  (open-store nil :type (or null function) :read-only t)
  (text-source nil :read-only t)
  (read-stdin-octets nil :type (or null function) :read-only t)
  (unix-now nil :type (or null function) :read-only t))

(defun make-write-edit-ports (&key workspace-host open-store)
  "EDIT-PORTS for a caller that only writes files whose bytes it already
holds (RUN-WRITE-COMMAND/K with a WRITE-PLAN that carries its own inputs, no
--stdin or --content-file, no `touch`): the write pipeline reaches only
WORKSPACE-HOST and OPEN-STORE, so TEXT-SOURCE, READ-STDIN-OCTETS, and UNIX-NOW
are left unset. Contexts reusing edit's write pipeline (e.g. `util decode
--to`) call this instead of assembling a partial EDIT-PORTS themselves."
  (make-edit-ports :workspace-host workspace-host :open-store open-store))
