;;;; packages/feature/search/src/domain/bytes.lisp
;;;;
;;;; Allocation-free byte scanning: line boundaries, character
;;;; columns, and literal search run over the undecoded file bytes, so a line
;;;; that never matches is never turned into a string.
(in-package #:aitools.search.domain)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(declaim (inline line-content-end line-start-at next-line-start))

(defun line-content-end (octets position)
  "The end of the content of the line holding POSITION: the index of its LF,
less one for a CR right before it, or the buffer end."
  (declare (type octets octets) (type fixnum position) (optimize (speed 3) (safety 1)))
  (let ((lf (position 10 octets :start position)))
    (cond ((null lf) (length octets))
          ((and (> lf position) (= (aref octets (1- lf)) 13)) (1- lf))
          (t lf))))

(defun line-start-at (octets position)
  "The offset of the first byte of the line holding POSITION."
  (declare (type octets octets) (type fixnum position) (optimize (speed 3) (safety 1)))
  (let ((lf (position 10 octets :end position :from-end t)))
    (if lf (1+ lf) 0)))

(defun next-line-start (octets position)
  "The offset just after the LF ending the line holding POSITION, or the
buffer length when that line is the last."
  (declare (type octets octets) (type fixnum position) (optimize (speed 3) (safety 1)))
  (let ((lf (position 10 octets :start position)))
    (if lf (1+ lf) (length octets))))

(defun count-newlines (octets start end)
  (declare (type octets octets) (type fixnum start end) (optimize (speed 3) (safety 1)))
  (let ((count 0))
    (declare (type fixnum count))
    (loop for i of-type fixnum from start below end
          do (when (= (aref octets i) 10) (incf count)))
    count))

(defun utf8-column (octets line-start position)
  "1-based character column of byte POSITION within the line starting at
LINE-START: the UTF-8 lead bytes before it, plus one."
  (declare (type octets octets) (type fixnum line-start position) (optimize (speed 3) (safety 1)))
  (let ((count 1))
    (declare (type fixnum count))
    (loop for i of-type fixnum from line-start below position
          do (unless (= (logand (aref octets i) #xC0) #x80) (incf count)))
    count))

(defun next-char-boundary (octets position)
  "The first offset after POSITION that does not hold a UTF-8 continuation
byte, so an empty match never splits a character."
  (declare (type octets octets) (type fixnum position) (optimize (speed 3) (safety 1)))
  (let ((end (length octets)))
    (loop for i of-type fixnum from (1+ position) below end
          unless (= (logand (aref octets i) #xC0) #x80) do (return i)
          finally (return end))))

(defun strip-bom (octets)
  "OCTETS without a leading UTF-8 BOM. The BOM is not part of the text, so it stays out of `^`, and
cl-regex-kit's anchors look at the whole buffer rather than at :START, so
the only way to hide it is a buffer that does not contain it."
  (declare (type octets octets))
  (if (plusp (utf8-bom-length octets)) (subseq octets 3) octets))

(declaim (inline %ascii-fold))
(defun %ascii-fold (byte)
  (declare (type (unsigned-byte 8) byte))
  (if (<= 65 byte 90) (+ byte 32) byte))

(defun octets-find (needle haystack start &key fold)
  "The offset of the first occurrence of NEEDLE in HAYSTACK at or after
START, or NIL. FOLD compares ASCII letters case-insensitively; NEEDLE must
then be lowercase."
  (declare (type octets needle haystack) (type fixnum start) (optimize (speed 3) (safety 1)))
  (let* ((n (length needle))
         (limit (- (length haystack) n)))
    (declare (type fixnum n limit))
    (cond
      ((zerop n) (and (<= start (length haystack)) start))
      ((> start limit) nil)
      (fold
       (let* ((first (aref needle 0))
              (upper (if (<= 97 first 122) (- first 32) first)))
         (loop for i of-type fixnum from start to limit
               do (let ((byte (aref haystack i)))
                    (when (and (or (= byte first) (= byte upper))
                               (loop for j of-type fixnum from 1 below n
                                     always (= (%ascii-fold (aref haystack (+ i j))) (aref needle j))))
                      (return i))))))
      (t
       (let ((first (aref needle 0)))
         (loop for i = (position first haystack :start start :end (1+ limit))
                 then (position first haystack :start (1+ (the fixnum i)) :end (1+ limit))
               while i
               do (when (loop for j of-type fixnum from 1 below n
                              always (= (aref haystack (+ (the fixnum i) j)) (aref needle j)))
                    (return i))))))))
