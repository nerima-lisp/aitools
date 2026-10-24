;;;; packages/feature/edit/src/application/ports.lisp
;;;;
;;;; The effects the edit flows need beyond the store, as plain closures
;;;; (the infrastructure layer builds them; unit tests pass their own).
(in-package #:aitools.edit.application)

(defconstant +max-input-bytes+
  (* 256 1024 1024)
  "Largest --stdin or --content-file input read.")

(defstruct
    (edit-ports
     (:constructor make-edit-ports
                   (&key workspace-host
                         open-store
                         text-source
                         read-stdin-octets
                         unix-now
                         serialize-document))
     (:copier nil))
  "WORKSPACE-HOST supplies workspace filesystem and environment functions.
Directory listing returns entries and a readable flag. STAT, READ-LINK,
READ-OCTETS, and GETENV return a value or NIL. HOME-DIRECTORY and
CURRENT-DIRECTORY return absolute paths; CALL-WITH-ORDERED-MAPPER preserves
result order. OPEN-STORE (real-root) returns a store for that workspace.
TEXT-SOURCE supplies file-size and octet-read operations for --content-file.
READ-STDIN-OCTETS (limit) reads at most limit plus one octet to detect
overflow. It returns :OCTETS with the bytes, :TOO-LARGE with NIL when input
exceeds limit, or :FAILURE with an error message. Called only for --stdin.
UNIX-NOW () returns Unix seconds used for touch default time and mktemp age
cutoff. SERIALIZE-DOCUMENT (FORMAT VALUE) returns text for a validated YAML or
TOML value."
  (workspace-host nil :read-only t)
  (open-store nil :type (or null function) :read-only t)
  (text-source nil :read-only t)
  (read-stdin-octets nil :type (or null function) :read-only t)
  (unix-now nil :type (or null function) :read-only t)
  (serialize-document nil :type (or null function) :read-only t))

(defun make-write-edit-ports (&key workspace-host open-store)
  "EDIT-PORTS for write plans that already carry their input bytes. This path
does not accept --stdin, --content-file, or touch. The write pipeline uses
only WORKSPACE-HOST and OPEN-STORE, so TEXT-SOURCE, READ-STDIN-OCTETS, and
UNIX-NOW remain unset."
  (make-edit-ports :workspace-host workspace-host :open-store open-store))
