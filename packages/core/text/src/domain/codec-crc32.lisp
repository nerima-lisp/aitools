;;;; packages/core/text/src/domain/codec-crc32.lisp
;;;;
;;;; CRC-32 (ISO 3309 / ITU-T V.42, reflected polynomial #xEDB88320) as used
;;;; by gzip (RFC 1952) and zip.
(in-package #:aitools.text.domain)

(defun crc32 (octets &key (start 0) end (crc 0))
  "The CRC-32 of OCTETS[START,END), continuing from CRC (0 to begin)."
  (declare (type octets octets) (type fixnum start) (type (unsigned-byte 32) crc))
  (deflate-kit:crc32 octets :initial (logxor crc #xFFFFFFFF) :start start :end end))
