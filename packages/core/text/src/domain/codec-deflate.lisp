;;;; packages/core/text/src/domain/codec-deflate.lisp
;;;;
;;;; DEFLATE (RFC 1951). INFLATE follows zlib's reference decoder puff.c:
;;;; canonical Huffman decoding one bit at a time, with every structural
;;;; check puff makes (over-subscribed or incomplete codes, distances past
;;;; the output start, missing end-of-block code). MAX-OUTPUT is enforced
;;;; while writing, so a decompression bomb stops at the limit instead of
;;;; after allocating its full expansion.
;;;;
;;;; DEFLATE favours correctness over ratio: greedy LZ77 with hash chains and
;;;; one fixed-Huffman block. Any conforming inflater (zlib, gzip, unzip)
;;;; reads its output.
(in-package #:aitools.text.domain)

(defconstant +max-bits+ 15)

(defparameter *length-base*
  #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
(defparameter *length-extra*
  #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
(defparameter *distance-base*
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073 4097
    6145 8193 12289 16385 24577))
(defparameter *distance-extra*
  #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))
(defparameter *code-length-order*
  #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

;;; ------------------------------------------------------------ Huffman tables

(defstruct (%huffman (:constructor %make-huffman (counts symbols)) (:copier nil))
  (counts nil :type (simple-array fixnum (*)) :read-only t)
  (symbols nil :type (simple-array fixnum (*)) :read-only t))

(defun %construct-huffman (lengths start count)
  "(VALUES HUFFMAN LEFT) for the code lengths LENGTHS[START,START+COUNT).
LEFT is 0 for a complete code, positive for an incomplete one, negative for
an over-subscribed one (puff.c construct)."
  (let ((counts (make-array (1+ +max-bits+) :element-type 'fixnum :initial-element 0))
        (symbols (make-array count :element-type 'fixnum :initial-element 0))
        (offsets (make-array (1+ +max-bits+) :element-type 'fixnum :initial-element 0)))
    (loop for i from start below (+ start count)
          do (incf (aref counts (aref lengths i))))
    (when (= (aref counts 0) count)
      (return-from %construct-huffman (values (%make-huffman counts symbols) 0)))
    (let ((left 1))
      (loop for length from 1 to +max-bits+
            do (setf left (- (* left 2) (aref counts length)))
               (when (minusp left)
                 (return-from %construct-huffman (values (%make-huffman counts symbols) left))))
      (loop for length from 1 below +max-bits+
            do (setf (aref offsets (1+ length)) (+ (aref offsets length) (aref counts length))))
      (loop for symbol from 0 below count
            for length = (aref lengths (+ start symbol))
            unless (zerop length)
              do (setf (aref symbols (aref offsets length)) symbol)
                 (incf (aref offsets length)))
      (values (%make-huffman counts symbols) left))))

(defparameter *fixed-literal-huffman*
  (let ((lengths (make-array 288 :element-type 'fixnum)))
    (loop for symbol from 0 below 288
          do (setf (aref lengths symbol)
                   (cond ((< symbol 144) 8) ((< symbol 256) 9) ((< symbol 280) 7) (t 8))))
    (values (%construct-huffman lengths 0 288))))

(defparameter *fixed-distance-huffman*
  (values (%construct-huffman (make-array 30 :element-type 'fixnum :initial-element 5) 0 30)))

;;; ------------------------------------------------------------ inflate

(defun inflate (octets &key (start 0) end max-output truncate-at size-hint)
  "Decompress the raw DEFLATE stream at OCTETS[START,END). Returns (VALUES
OUTPUT NEXT), NEXT being the index of the first byte after the final block.
Signals ARCHIVE-ERROR on malformed input and ARCHIVE-LIMIT-EXCEEDED when the
output would exceed MAX-OUTPUT bytes. With TRUNCATE-AT, decoding stops once
that many bytes are out and returns them with NEXT NIL: enough to sniff the
content of a stream without inflating a bomb. SIZE-HINT, the expected output
length (gzip's ISIZE), sizes the output buffer up front so a correct hint
needs neither growth nor a final copy; it is capped by MAX-OUTPUT."
  (declare (type octets octets) (type fixnum start))
  (let ((end (or end (length octets)))
        (position start)
        (bit-buffer 0)
        (bit-count 0)
        (output (make-array (max 64 (min (or size-hint (min (* 4 (- (or end (length octets)) start)) 1048576))
                                         (or max-output most-positive-fixnum)))
                            :element-type '(unsigned-byte 8)))
        (produced 0))
    (declare (type fixnum end position bit-count produced)
             (type (unsigned-byte 62) bit-buffer)
             (type octets output))
    (labels
        ((bits (count)
           (declare (type (integer 0 16) count))
           (loop while (< bit-count count)
                 do (when (>= position end) (%archive-fail "deflate stream is truncated"))
                    (setf bit-buffer (logior bit-buffer (ash (aref octets position) bit-count)))
                    (incf position)
                    (incf bit-count 8))
           (prog1 (ldb (byte count 0) bit-buffer)
             (setf bit-buffer (ash bit-buffer (- count)))
             (decf bit-count count)))
         (emit (byte)
           (when (and max-output (>= produced max-output))
             (error 'archive-limit-exceeded :limit max-output :reason "output limit"))
           (when (= produced (length output))
             (let ((grown (make-array (min (* 2 (length output)) (or max-output most-positive-fixnum))
                                      :element-type '(unsigned-byte 8))))
               (replace grown output)
               (setf output grown)))
           (setf (aref output produced) byte)
           (incf produced)
           (when (and truncate-at (>= produced truncate-at))
             (return-from inflate (values (subseq output 0 produced) nil))))
         (decode (huffman)
           (let ((counts (%huffman-counts huffman))
                 (symbols (%huffman-symbols huffman))
                 (code 0) (first 0) (index 0))
             (declare (type fixnum code first index))
             (loop for length from 1 to +max-bits+
                   do (setf code (logior code (bits 1)))
                      (let ((count (aref counts length)))
                        (when (< (- code count) first)
                          (return-from decode (aref symbols (+ index (- code first)))))
                        (incf index count)
                        (incf first count)
                        (setf first (ash first 1) code (ash code 1))))
             (%archive-fail "deflate stream uses an undefined Huffman code")))
         (stored ()
           (setf bit-buffer 0 bit-count 0)
           (when (> (+ position 4) end) (%archive-fail "stored block header is truncated"))
           (let ((length (logior (aref octets position) (ash (aref octets (+ position 1)) 8)))
                 (complement (logior (aref octets (+ position 2)) (ash (aref octets (+ position 3)) 8))))
             (incf position 4)
             (unless (= length (logxor complement #xFFFF))
               (%archive-fail "stored block length check failed"))
             (when (> (+ position length) end) (%archive-fail "stored block is truncated"))
             (loop repeat length do (emit (aref octets position)) (incf position))))
         (codes (literals distances)
           (loop
             (let ((symbol (decode literals)))
               (cond
                 ((< symbol 256) (emit symbol))
                 ((= symbol 256) (return))
                 (t
                  (let ((index (- symbol 257)))
                    (when (>= index 29) (%archive-fail "invalid length code"))
                    (let* ((length (+ (svref *length-base* index) (bits (svref *length-extra* index))))
                           ;; No distance code has more than 30 symbols, so fixed
                           ;; codes 30 and 31 fail in DECODE as undefined.
                           (distance-symbol (decode distances)))
                      (let ((distance (+ (svref *distance-base* distance-symbol)
                                         (bits (svref *distance-extra* distance-symbol)))))
                        (when (> distance produced) (%archive-fail "distance reaches before the output start"))
                        (loop repeat length do (emit (aref output (- produced distance))))))))))))
         (dynamic ()
           (let ((literal-count (+ (bits 5) 257))
                 (distance-count (+ (bits 5) 1))
                 (code-count (+ (bits 4) 4))
                 (lengths (make-array 320 :element-type 'fixnum :initial-element 0)))
             (when (or (> literal-count 286) (> distance-count 30))
               (%archive-fail "too many length or distance codes"))
             (loop for i from 0 below code-count
                   do (setf (aref lengths (svref *code-length-order* i)) (bits 3)))
             (multiple-value-bind (code-lengths left) (%construct-huffman lengths 0 19)
               (unless (zerop left) (%archive-fail "incomplete code-length code"))
               (let ((total (+ literal-count distance-count)) (index 0))
                 (fill lengths 0)
                 (loop while (< index total)
                       do (let ((symbol (decode code-lengths)))
                            (if (< symbol 16)
                                (progn (setf (aref lengths index) symbol) (incf index))
                                (multiple-value-bind (value repeat)
                                    (case symbol
                                      (16 (when (zerop index) (%archive-fail "repeat with no previous length"))
                                       (values (aref lengths (1- index)) (+ 3 (bits 2))))
                                      (17 (values 0 (+ 3 (bits 3))))
                                      (t (values 0 (+ 11 (bits 7)))))
                                  (when (> (+ index repeat) total) (%archive-fail "too many code lengths"))
                                  (loop repeat repeat do (setf (aref lengths index) value) (incf index))))))
                 (when (zerop (aref lengths 256)) (%archive-fail "no end-of-block code"))
                 (multiple-value-bind (literals left) (%construct-huffman lengths 0 literal-count)
                   (when (and (/= left 0)
                              (or (minusp left)
                                  (/= literal-count (+ (aref (%huffman-counts literals) 0)
                                                       (aref (%huffman-counts literals) 1)))))
                     (%archive-fail "invalid literal/length code"))
                   (multiple-value-bind (distances left) (%construct-huffman lengths literal-count distance-count)
                     (when (and (/= left 0)
                                (or (minusp left)
                                    (/= distance-count (+ (aref (%huffman-counts distances) 0)
                                                          (aref (%huffman-counts distances) 1)))))
                       (%archive-fail "invalid distance code"))
                     (codes literals distances))))))))
      (loop
        (let ((last (bits 1)))
          (case (bits 2)
            (0 (stored))
            (1 (codes *fixed-literal-huffman* *fixed-distance-huffman*))
            (2 (dynamic))
            (t (%archive-fail "invalid block type")))
          (when (= last 1) (return))))
      ;; No copy when the buffer is exactly full: with a large MAX-OUTPUT the copy
      ;; would hold a second output-sized array live at the peak.
      (values (if (= produced (length output)) output (subseq output 0 produced)) position))))

;;; ------------------------------------------------------------ deflate

(defun %reverse-bits (code length)
  (let ((result 0))
    (dotimes (i length result)
      (setf result (logior (ash result 1) (ldb (byte 1 i) code))))))

(defun %fixed-literal-code (symbol)
  "(VALUES CODE LENGTH) of SYMBOL in the fixed literal/length code."
  (cond ((< symbol 144) (values (+ #x30 symbol) 8))
        ((< symbol 256) (values (+ #x190 (- symbol 144)) 9))
        ((< symbol 280) (values (- symbol 256) 7))
        (t (values (+ #xC0 (- symbol 280)) 8))))

(defun %base-index (bases value)
  (loop for i from (1- (length bases)) downto 0
        when (<= (svref bases i) value) return i))

(defun deflate (octets &key (start 0) end)
  "OCTETS[START,END) compressed as one fixed-Huffman DEFLATE block."
  (declare (type octets octets) (type fixnum start))
  (let* ((end (or end (length octets)))
         (out (make-array (+ 16 (floor (* 9 (- end start)) 8)) :element-type '(unsigned-byte 8)
                                                                :adjustable t :fill-pointer 0))
         (bit-buffer 0)
         (bit-count 0)
         (head (make-array 65536 :element-type 'fixnum :initial-element -1))
         (previous (make-array 32768 :element-type 'fixnum :initial-element -1)))
    (declare (type fixnum end bit-count) (type (unsigned-byte 62) bit-buffer))
    (labels ((put (value count)
               (setf bit-buffer (logior bit-buffer (ash value bit-count)))
               (incf bit-count count)
               (loop while (>= bit-count 8)
                     do (vector-push-extend (ldb (byte 8 0) bit-buffer) out)
                        (setf bit-buffer (ash bit-buffer -8))
                        (decf bit-count 8)))
             (put-code (code length)
               (put (%reverse-bits code length) length))
             (put-symbol (symbol)
               (multiple-value-bind (code length) (%fixed-literal-code symbol)
                 (put-code code length)))
             (hash-at (i)
               (logand (logxor (ash (aref octets i) 10) (ash (aref octets (+ i 1)) 5) (aref octets (+ i 2)))
                       #xFFFF))
             (insert (i)
               (when (<= (+ i 3) end)
                 (let ((hash (hash-at i)))
                   (setf (aref previous (logand i #x7FFF)) (aref head hash)
                         (aref head hash) i))))
             (longest-match (i)
               (let ((best-length 0) (best-distance 0))
                 (when (<= (+ i 3) end)
                   (let ((candidate (aref head (hash-at i)))
                         (limit (min 258 (- end i))))
                     (loop repeat 128
                           while (and (>= candidate 0) (< candidate i) (<= (- i candidate) 32768))
                           do (let ((length (loop for k from 0 below limit
                                                  while (= (aref octets (+ candidate k)) (aref octets (+ i k)))
                                                  finally (return k))))
                                (when (> length best-length)
                                  (setf best-length length best-distance (- i candidate))
                                  (when (= length limit) (return))))
                              (let ((next (aref previous (logand candidate #x7FFF))))
                                (when (>= next candidate) (return))
                                (setf candidate next)))))
                 (values best-length best-distance))))
      (put 1 1)
      (put 1 2)
      (let ((i start))
        (loop while (< i end)
              do (multiple-value-bind (length distance) (longest-match i)
                   (if (>= length 3)
                       (let ((length-index (%base-index *length-base* length))
                             (distance-index (%base-index *distance-base* distance)))
                         (put-symbol (+ 257 length-index))
                         (put (- length (svref *length-base* length-index)) (svref *length-extra* length-index))
                         (put-code distance-index 5)
                         (put (- distance (svref *distance-base* distance-index))
                              (svref *distance-extra* distance-index))
                         (loop repeat length do (insert i) (incf i)))
                       (progn
                         (put-symbol (aref octets i))
                         (insert i)
                         (incf i))))))
      (put-symbol 256)
      (when (plusp bit-count) (put 0 (- 8 bit-count)))
      (coerce out 'octets))))
