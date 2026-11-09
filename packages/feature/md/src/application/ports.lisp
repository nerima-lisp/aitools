(in-package #:aitools.md.application)

(defconstant +md-max-input-bytes+
  (* 16 1024 1024))

(defstruct
    (md-ports
     (:constructor make-md-ports
                   (&key workspace-host
                         text-source
                         open-store
                         read-stdin-octets
                         parse-markdown
                         splice-section))
     (:copier nil))
  (workspace-host nil :read-only t)
  (text-source nil :read-only t)
  (open-store nil :type (or null function) :read-only t)
  (read-stdin-octets nil :type (or null function) :read-only t)
  (parse-markdown nil :type (or null function) :read-only t)
  (splice-section nil :type (or null function) :read-only t))
