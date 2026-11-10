;;;;
;;;; Size limits shared by the edit application flows.
(in-package #:aitools.edit.application)

(defconstant +max-input-bytes+
  (* 256 1024 1024)
  "Largest --stdin or --content-file input read.")
(defconstant +max-format-document-bytes+ (* 16 1024 1024))
