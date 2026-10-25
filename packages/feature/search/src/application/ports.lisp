;;;;
;;;; The effects the search flows need, as one explicit argument. Each slot
;;;; is supplied by the composition root through
;;;; AITOOLS.SEARCH.INFRASTRUCTURE:MAKE-PRODUCTION-SEARCH-PORTS, or by a unit
;;;; test with fakes.
(in-package #:aitools.search.application)

(defstruct (search-ports (:constructor make-search-ports
                             (&key workspace-host text-source open-store unix-now read-stdin-octets))
                         (:copier nil))
  "WORKSPACE-HOST supplies listing, stat, and ordered mapping. TEXT-SOURCE
is used through CALL-WITH-CHUNKS(path, chunk-size, function): it calls FUNCTION
with successive octet chunks until :STOP, returning T when PATH is readable or
NIL when it cannot be opened. OPEN-STORE(real-root) returns the workspace
store used by --tx. UNIX-NOW() returns Unix seconds for duration-based
--newer. READ-STDIN-OCTETS(limit) returns (:OCTETS octets), (:TOO-LARGE), or
(:FAILURE message)."
  (workspace-host nil :read-only t)
  (text-source nil :read-only t)
  (open-store nil :type (or null function) :read-only t)
  (unix-now nil :type (or null function) :read-only t)
  (read-stdin-octets nil :type (or null function) :read-only t))
