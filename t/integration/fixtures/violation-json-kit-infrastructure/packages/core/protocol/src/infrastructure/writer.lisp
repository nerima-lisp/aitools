;;;; Fixture: the one infrastructure package allowed to use JSON-KIT.
(in-package #:aitools.protocol.infrastructure)

(defun write-value (value stream)
  (json-kit:write-json value stream))
