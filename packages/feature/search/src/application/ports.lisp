;;;; packages/feature/search/src/application/ports.lisp
;;;;
;;;; The effects the search flows need, as one explicit argument. Each slot
;;;; is supplied by the composition root through
;;;; AITOOLS.SEARCH.INFRASTRUCTURE:MAKE-PRODUCTION-SEARCH-PORTS, or by a unit
;;;; test with fakes. Ports are called per directory or per file, never per
;;;; line.
(in-package #:aitools.search.application)

(defstruct (search-ports (:constructor make-search-ports
                             (&key workspace-host text-source open-store unix-now read-stdin-octets))
                         (:copier nil))
  "WORKSPACE-HOST: the workspace context's WORKSPACE-HOST (listing, stat,
the ordered worker pool). TEXT-SOURCE: the text context's TEXT-SOURCE, used
for every file whose bytes a command reads from disk. OPEN-STORE (real-root)
returns the store context's STORE for a workspace, used only by `--tx`.
UNIX-NOW () returns the current Unix time in seconds (for a `--newer`
duration). READ-STDIN-OCTETS (limit &key on-octets on-too-large on-failure)
reads standard input, calling exactly one continuation."
  (workspace-host nil :read-only t)
  (text-source nil :read-only t)
  (open-store nil :type (or null function) :read-only t)
  (unix-now nil :type (or null function) :read-only t)
  (read-stdin-octets nil :type (or null function) :read-only t))

(defun unix-seconds-from-universal-time (universal-time)
  (aitools.kernel.domain:universal-time-to-unix-seconds universal-time))
