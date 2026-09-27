;;;; packages/core/text/src/domain/binary.lisp
;;;;
;;;; A file is binary when its first 8 KiB contain a NUL
;;;; byte. Callers pass only that prefix when they can, so the decision is
;;;; made before the whole file is read.
(in-package #:aitools.text.domain)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defconstant +binary-sniff-length+ 8192)

(defun binary-octets-p (octets &key (start 0) end)
  "True when a NUL byte occurs within the first +BINARY-SNIFF-LENGTH+ bytes
of OCTETS[START,END)."
  (declare (type octets octets) (type fixnum start))
  (let ((limit (min (or end (length octets)) (+ start +binary-sniff-length+))))
    (and (position 0 octets :start start :end limit) t)))
