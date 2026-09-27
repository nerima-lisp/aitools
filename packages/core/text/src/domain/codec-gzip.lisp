;;;; packages/core/text/src/domain/codec-gzip.lisp
;;;;
;;;; gzip (RFC 1952) over INFLATE/DEFLATE. Reading accepts concatenated
;;;; members (as `gzip -c a b` writes) and trailing zero padding, and checks
;;;; each member's CRC-32 and ISIZE. The FNAME field is written as UTF-8 and
;;;; read as UTF-8 when valid, else ISO-8859-1 as RFC 1952 specifies.
(in-package #:aitools.text.domain)

(defun %u16le (octets offset)
  (when (> (+ offset 2) (length octets)) (%archive-fail "field is truncated"))
  (logior (aref octets offset) (ash (aref octets (1+ offset)) 8)))

(defun %u32le (octets offset)
  (when (> (+ offset 4) (length octets)) (%archive-fail "field is truncated"))
  (logior (aref octets offset) (ash (aref octets (+ offset 1)) 8)
          (ash (aref octets (+ offset 2)) 16) (ash (aref octets (+ offset 3)) 24)))

(defun %name-from-octets (octets start end)
  "Archive member names: UTF-8 when valid, else one character per byte."
  (flet ((bytewise (position)
           (declare (ignore position))
           (map 'string #'code-char (subseq octets start end))))
    (declare (dynamic-extent #'bytewise))
    (decode-utf8-strict/k octets :start start :end end :on-decoded #'identity :on-invalid #'bytewise)))

(defun gzip-member-header (octets &key (start 0))
  "Parse the gzip member header at START. Returns (VALUES NAME MTIME
DATA-START): NAME from FNAME (or NIL), MTIME in Unix seconds, DATA-START the
index of the DEFLATE stream."
  (declare (type octets octets))
  (when (> (+ start 10) (length octets)) (%archive-fail "gzip header is truncated"))
  (unless (and (= (aref octets start) #x1F) (= (aref octets (+ start 1)) #x8B))
    (%archive-fail "not a gzip stream"))
  (unless (= (aref octets (+ start 2)) 8) (%archive-unsupported "gzip compression method"))
  (let ((flags (aref octets (+ start 3)))
        (mtime (%u32le octets (+ start 4)))
        (position (+ start 10))
        (name nil))
    (unless (zerop (logand flags #xE0)) (%archive-fail "reserved gzip flags are set"))
    (when (logbitp 2 flags)
      (incf position (+ 2 (%u16le octets position))))
    (flet ((zero-terminated ()
             (let ((zero (position 0 octets :start (min position (length octets)))))
               (unless zero (%archive-fail "unterminated gzip header string"))
               (prog1 (cons position zero) (setf position (1+ zero))))))
      (when (logbitp 3 flags)
        (destructuring-bind (from . to) (zero-terminated)
          (setf name (%name-from-octets octets from to))))
      (when (logbitp 4 flags) (zero-terminated)))
    (when (logbitp 1 flags) (incf position 2))
    (when (> position (length octets)) (%archive-fail "gzip header is truncated"))
    (values name mtime position)))

(defun gzip-decompress (octets &key max-output)
  "The decompressed bytes of every member of the gzip stream OCTETS, each
checked against its CRC-32 and length. MAX-OUTPUT bounds the total and is
signalled as ARCHIVE-LIMIT-EXCEEDED. Peak memory stays near one copy of the
output: the last four bytes (a one-member stream's ISIZE) size the first
member's buffer, and a lone member is returned without concatenating."
  (declare (type octets octets))
  (let ((position 0) (parts '()) (total 0))
    (loop
      (multiple-value-bind (name mtime data-start) (gzip-member-header octets :start position)
        (declare (ignore name mtime))
        (multiple-value-bind (data next)
            (inflate octets :start data-start :max-output (and max-output (- max-output total))
                            :size-hint (and (zerop position) (>= (length octets) 4)
                                            (%u32le octets (- (length octets) 4))))
          (let ((crc (%u32le octets next))
                (size (%u32le octets (+ next 4))))
            (unless (= crc (crc32 data)) (%archive-fail "gzip CRC-32 mismatch"))
            (unless (= size (logand (length data) #xFFFFFFFF)) (%archive-fail "gzip length mismatch"))
            (push data parts)
            (incf total (length data))
            (setf position (+ next 8)))))
      (when (or (>= position (length octets))
                (not (find-if-not #'zerop octets :start position)))
        (return))
      (unless (and (< (1+ position) (length octets))
                   (= (aref octets position) #x1F) (= (aref octets (1+ position)) #x8B))
        (%archive-fail "trailing data after the gzip stream")))
    (if (null (rest parts))
        (first parts)
        (let ((result (make-array total :element-type '(unsigned-byte 8))) (offset 0))
          (dolist (part (nreverse parts) result)
            (replace result part :start1 offset)
            (incf offset (length part)))))))

(defun gzip-compress (octets &key name (mtime 0))
  "OCTETS as a single-member gzip stream. NAME, when given, is stored in
FNAME; MTIME is Unix seconds (0 means unknown, keeping output reproducible)."
  (declare (type octets octets))
  (let* ((name-octets (and name (encode-utf8 name)))
         (header (concatenate 'octets
                              (vector #x1F #x8B 8 (if name 8 0)
                                      (ldb (byte 8 0) mtime) (ldb (byte 8 8) mtime)
                                      (ldb (byte 8 16) mtime) (ldb (byte 8 24) mtime)
                                      0 255)
                              (or name-octets #())
                              (if name #(0) #())))
         (crc (crc32 octets))
         (size (logand (length octets) #xFFFFFFFF)))
    (concatenate 'octets header (deflate octets)
                 (vector (ldb (byte 8 0) crc) (ldb (byte 8 8) crc) (ldb (byte 8 16) crc) (ldb (byte 8 24) crc)
                         (ldb (byte 8 0) size) (ldb (byte 8 8) size) (ldb (byte 8 16) size) (ldb (byte 8 24) size)))))
