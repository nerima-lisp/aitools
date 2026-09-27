;;;; packages/feature/util/src/application/ports.lisp
;;;;
;;;; The effects `util` needs, as one explicit argument to every flow.
;;;; Each port is a closure: the structure test keeps the
;;;; application layer off cl-boundary-kit, so the infrastructure layer
;;;; builds these closures over cl-boundary-kit's random-source, uuid-source,
;;;; and clock ports (production or fake), and unit tests inject the fakes
;;;; through that same constructor.
(in-package #:aitools.util.application)

(defconstant +util-max-input-bytes+ (* 64 1024 1024)
  "Largest `--content-file` or `--stdin` input a util command reads.")

(defstruct (util-ports (:constructor make-util-ports
                           (&key random-octets uuid-v4 unix-ms read-file-octets read-stdin-octets
                              workspace-host open-store))
                       (:copier nil))
  "RANDOM-OCTETS (COUNT) returns COUNT fresh octets from a cryptographic
source. UUID-V4 () returns a version-4 UUID string. UNIX-MS () returns the
wall-clock Unix time in milliseconds. READ-FILE-OCTETS (PATH LIMIT &key
ON-OCTETS ON-MISSING ON-TOO-LARGE ON-FAILURE) reads at most LIMIT bytes of
PATH and calls exactly one continuation: ON-OCTETS (octets), ON-MISSING (),
ON-TOO-LARGE (), or ON-FAILURE (message). READ-STDIN-OCTETS (LIMIT &key
ON-OCTETS ON-TOO-LARGE ON-FAILURE) does the same for standard input.
WORKSPACE-HOST and OPEN-STORE are the composition root's workspace host and
store constructor, which `util decode --to` hands to the edit context's
write pipeline."
  (random-octets nil :type (or null function) :read-only t)
  (uuid-v4 nil :type (or null function) :read-only t)
  (unix-ms nil :type (or null function) :read-only t)
  (read-file-octets nil :type (or null function) :read-only t)
  (read-stdin-octets nil :type (or null function) :read-only t)
  (workspace-host nil :read-only t)
  (open-store nil :type (or null function) :read-only t))
