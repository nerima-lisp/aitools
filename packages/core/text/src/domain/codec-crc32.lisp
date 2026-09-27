;;;; packages/core/text/src/domain/codec-crc32.lisp
;;;;
;;;; CRC-32 (ISO 3309 / ITU-T V.42, reflected polynomial #xEDB88320) as used
;;;; by gzip (RFC 1952) and zip.
(in-package #:aitools.text.domain)

(defparameter *crc32-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8)
          (setf c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) c)))))

(defun crc32 (octets &key (start 0) end (crc 0))
  "The CRC-32 of OCTETS[START,END), continuing from CRC (the value returned
for the preceding bytes, 0 to begin)."
  (declare (type octets octets) (type fixnum start) (type (unsigned-byte 32) crc)
           (optimize (speed 3) (safety 1)))
  (let ((table *crc32-table*)
        (c (logxor crc #xFFFFFFFF)))
    (declare (type (simple-array (unsigned-byte 32) (256)) table)
             (type (unsigned-byte 32) c))
    (loop for i of-type fixnum from start below (or end (length octets))
          do (setf c (logxor (aref table (logand (logxor c (aref octets i)) #xFF)) (ash c -8))))
    (logxor c #xFFFFFFFF)))
