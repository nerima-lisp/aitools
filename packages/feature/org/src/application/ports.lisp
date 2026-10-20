(in-package #:aitools.org.application)

(defstruct (org-ports (:constructor make-org-ports
                           (&key workspace-host text-source max-input-bytes
                                 parse-org outline-org query-org export-org))
                      (:copier nil))
  (workspace-host nil :read-only t)
  (text-source nil :read-only t)
  (max-input-bytes 0 :type integer :read-only t)
  (parse-org nil :type (or null function) :read-only t)
  (outline-org nil :type (or null function) :read-only t)
  (query-org nil :type (or null function) :read-only t)
  (export-org nil :type (or null function) :read-only t))
