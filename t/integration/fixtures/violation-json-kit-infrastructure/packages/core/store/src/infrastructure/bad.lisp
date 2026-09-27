;;;; Fixture: the JSON-KIT exemption belongs to protocol's envelope writer.
(in-package #:aitools.store.infrastructure)

(defun bad-parse (text)
  (json-kit:parse text))
