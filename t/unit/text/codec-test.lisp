;;;;
;;;; CRC-32 and raw DEFLATE. Known vectors: the CRC-32 check value from
;;;; the CRC catalogue ("123456789" -> CBF43926), RFC 1951 stored block, and
;;;; streams produced by zlib 1.2.12 (Python's zlib.compressobj(9, DEFLATED,
;;;; -15). The dynamic-block vector's input is generated
;;;; below by the same formula used to produce it.
(in-package #:aitools.text.test)

(describe "aitools.text.domain CRC-32"
  (it "matches the catalogue check value and continues incrementally"
    (expect (crc32 (string-bytes "123456789")) :to-be #xCBF43926)
    (expect (crc32 (octets)) :to-be 0)
    (expect (crc32 (string-bytes "6789") :crc (crc32 (string-bytes "12345"))) :to-be #xCBF43926)))

(describe "aitools.text.domain inflate"
  (it "decodes a stored block, a fixed block, and a dynamic block"
    (expect (inflate (hex-octets "010500faff68656c6c6f")) :to-equalp (string-bytes "hello"))
    (expect (inflate (hex-octets "cb48cdc9c90700")) :to-equalp (string-bytes "hello"))
    (expect (inflate (hex-octets *dynamic-vector*)) :to-equalp (dynamic-vector-input)))

  (it "returns the index after the final block"
    (expect (nth-value 1 (inflate (join-octets (hex-octets "cb48cdc9c90700") (octets 1 2 3)))) :to-be 7))

  (it "rejects malformed streams with ARCHIVE-ERROR"
    (signals archive-error (inflate (octets #x07)))
    (signals archive-error (inflate (hex-octets "010500faf068656c6c6f")))
    (signals archive-error (inflate (hex-octets "cb48cdc9")))
    ;; literal 'a' then a match at distance 5; zlib 1.2.12 reports "invalid distance too far back"
    (signals archive-error (inflate (hex-octets "4b041200"))))

  (it "stops at MAX-OUTPUT instead of expanding a bomb"
    (let ((bomb (deflate (make-array 100000 :element-type '(unsigned-byte 8) :initial-element 0))))
      (expect (< (length bomb) 1000) :to-be-truthy)
      (signals archive-limit-exceeded (inflate bomb :max-output 4096))
      (expect (length (inflate bomb :max-output 100000)) :to-be 100000)))

  (it "returns a tar-block prefix and no consumed index when TRUNCATE-AT is set"
    (let* ((source (coerce (loop for index below 100000 collect (mod index 251))
                           '(vector (unsigned-byte 8))))
           (bomb (deflate source)))
      (multiple-value-bind (prefix consumed-index)
          (inflate bomb :truncate-at aitools.text.domain::+tar-block+)
        (expect (equalp prefix (subseq source 0 aitools.text.domain::+tar-block+)) :to-be-truthy)
        (expect consumed-index :to-be-falsy))))

  (it "rejects every truncation and checks bit-flipped streams"
    (let ((valid (hex-octets *dynamic-vector*)))
      (expect (loop for end from 0 below (length valid)
                    always
                      (eq (handler-case
                              (progn (inflate (subseq valid 0 end)) :returned)
                            (archive-error () :archive-error)
                            (error () :other))
                          :archive-error))
              :to-be-truthy)
      (expect (loop for i from 0 below (length valid)
                    always (let ((copy (copy-seq valid)))
                             (setf (aref copy i) (logxor (aref copy i) (ash 1 (mod i 8))))
                             (only-archive-errors-p (lambda (bytes) (inflate bytes :max-output 10000)) copy)))
              :to-be-truthy))))

(describe "aitools.text.domain deflate"
  (it "round-trips through inflate for varied inputs"
    (dolist (input (list (octets)
                         (octets 7)
                         (string-bytes "abcabcabcabcabcabc")
                         (dynamic-vector-input)
                         (pseudo-random-octets 70000 42)
                         (let ((all (make-array 1024 :element-type '(unsigned-byte 8))))
                           (dotimes (i 1024 all) (setf (aref all i) (mod i 256))))))
      (expect (inflate (deflate input)) :to-equalp input)))
  (it "compresses and decodes nonzero input and stream slices"
    (let* ((input (octets #xAA 1 2 3 #xBB))
           (compressed (deflate input :start 1 :end 4))
           (prefix (octets #xCC #xDD))
           (stream (join-octets prefix compressed (octets #xEE)))
           (end (+ (length prefix) (length compressed))))
      (expect (inflate stream :start (length prefix) :end end)
              :to-equalp (octets 1 2 3))
      (expect (nth-value 1 (inflate stream :start (length prefix) :end end))
              :to-be end)
      (signals archive-error
        (inflate stream :start (length prefix) :end (1- end)))))
  (it-property "round-trips generated octet vectors through raw DEFLATE"
    ((input (gen-map (lambda (bytes)
                       (map '(simple-array (unsigned-byte 8) (*)) #'identity bytes))
                     (gen-vector (gen-integer :min 0 :max 255)
                                 :min-length 0
                                 :max-length 4096))))
    (expect (equalp (inflate (deflate input)) input) :to-be t))

  (it "compresses repetitive input"
    (expect (< (length (deflate (dynamic-vector-input))) 600) :to-be-truthy))
  (it-each (("empty input" 0)
            ("one byte" 1)
            ("32 KiB minus one" 32767)
            ("32 KiB" 32768)
            ("32 KiB plus one" 32769)
            ("65,535 bytes" 65535)
            ("65,536 bytes" 65536)
            ("1 MiB" 1048576))
    "round-trips ~A bytes through raw DEFLATE"
    (description size)
    (declare (ignore description))
    (let ((input (pseudo-random-octets size 42)))
      (expect (equalp (inflate (deflate input)) input) :to-be t))))

(defun pack-bits (fields)
  "FIELDS, a list of (VALUE . COUNT), packed least significant bit first as
DEFLATE reads them."
  (let ((out '()) (acc 0) (n 0))
    (dolist (field fields)
      (dotimes (i (cdr field))
        (setf acc (logior acc (ash (ldb (byte 1 i) (car field)) n)))
        (when (= (incf n) 8) (push acc out) (setf acc 0 n 0))))
    (when (plusp n) (push acc out))
    (coerce (nreverse out) '(simple-array (unsigned-byte 8) (*)))))

(defun huffman-field (code length)
  "A Huffman CODE of LENGTH bits as a PACK-BITS field: DEFLATE sends codes
most significant bit first."
  (let ((reversed 0))
    (dotimes (i length) (setf reversed (logior (ash reversed 1) (ldb (byte 1 i) code))))
    (cons reversed length)))

(defun canonical-codes (lengths)
  "RFC 1951 3.2.2's codes for the code LENGTHS (a vector, 0 = unused)."
  (let ((counts (make-array 17 :initial-element 0))
        (next (make-array 17 :initial-element 0))
        (codes (make-array (length lengths) :initial-element nil))
        (code 0))
    (loop for length across lengths when (plusp length) do (incf (aref counts length)))
    (loop for bits from 1 to 16
          do (setf code (ash (+ code (aref counts (1- bits))) 1) (aref next bits) code))
    (loop for symbol from 0 below (length lengths)
          for length = (aref lengths symbol)
          when (plusp length) do (setf (aref codes symbol) (aref next length)) (incf (aref next length)))
    codes))

(defun length-vector (count alist)
  (let ((vector (make-array count :initial-element 0)))
    (loop for (symbol . length) in alist do (setf (aref vector symbol) length))
    vector))

(defun dynamic-block (&key (literal-count 257) (distance-count 1) literals distances code-length-symbols body)
  "One final dynamic-Huffman DEFLATE block. LITERALS and DISTANCES are alists
of (SYMBOL . CODE-LENGTH); CODE-LENGTH-SYMBOLS, when given, replaces the
code-length sequence with (SYMBOL . EXTRA-BITS-VALUE) items; BODY lists
literal/length symbols, or (:RAW VALUE . COUNT) bit fields."
  (let* ((literal-lengths (length-vector literal-count literals))
         (distance-lengths (length-vector distance-count distances))
         ;; 13 code-length symbols of 4 bits and 6 of 5 bits: a complete code.
         (cl-lengths (coerce (loop for symbol from 0 below 19 collect (if (< symbol 13) 4 5)) 'vector))
         (cl-codes (canonical-codes cl-lengths))
         (literal-codes (canonical-codes literal-lengths))
         (fields (list (cons 1 1) (cons 2 2) (cons (- literal-count 257) 5) (cons (- distance-count 1) 5) (cons 15 4))))
    (dolist (symbol '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
      (setf fields (append fields (list (cons (aref cl-lengths symbol) 3)))))
    (dolist (item (or code-length-symbols
                      (map 'list (lambda (length) (cons length 0)) (concatenate 'vector literal-lengths distance-lengths))))
      (destructuring-bind (symbol . extra) item
        (setf fields (append fields (list (huffman-field (aref cl-codes symbol) (aref cl-lengths symbol)))
                             (case symbol (16 (list (cons extra 2))) (17 (list (cons extra 3))) (18 (list (cons extra 7))))))))
    (dolist (item body)
      (setf fields (append fields (list (if (consp item)
                                            (cdr item)
                                            (huffman-field (aref literal-codes item) (aref literal-lengths item)))))))
    (pack-bits (append fields (list (cons 0 32))))))

(defun fixed-block (&rest fields)
  "A final fixed-Huffman block: the header, then FIELDS, then zero padding."
  (pack-bits (append (list (cons 1 1) (cons 1 2)) fields (list (cons 0 32)))))

(defun inflate-reason (octets)
  (handler-case (progn (inflate octets) :no-error)
    (archive-error (condition) (archive-error-reason condition))))

(describe "aitools.text.domain inflate on crafted streams"
  (it "decodes the crafted blocks the malformed cases below are variations of"
    (expect (multiple-value-list
             (inflate (dynamic-block :literals '((97 . 1) (256 . 1))
                                    :distances '((0 . 1))
                                    :body '(97 97 256))))
            :to-equalp (list (string-bytes "aa") 139))
    (expect (inflate (dynamic-block :literals '((256 . 1)) :distances '((0 . 1)) :body '(256))) :to-equalp (octets))
    (expect (inflate (dynamic-block :literals '((97 . 1) (256 . 1)) :distances '((0 . 1)) :body '(97 256)))
            :to-equalp (string-bytes "a"))
    (expect (inflate (fixed-block (huffman-field (+ #x30 97) 8) (huffman-field 1 7) (huffman-field 0 5) (huffman-field 0 7)))
            :to-equalp (string-bytes "aaaa")))

  (it-each (("an over-subscribed literal/length code"
           :dynamic (:literals ((0 . 1) (1 . 1) (256 . 1)) :distances ((0 . 1)))
           "A DEFLATE Huffman tree is oversubscribed.")
          ("an incomplete literal/length code of more than one symbol"
           :dynamic (:literals ((97 . 2) (256 . 2)) :distances ((0 . 1)))
           "The DEFLATE stream ended prematurely.")
          ("an over-subscribed distance code"
           :dynamic (:literals ((97 . 1) (256 . 1))
                     :distance-count 3
                     :distances ((0 . 1) (1 . 1) (2 . 1)))
           "A DEFLATE Huffman tree is oversubscribed.")
          ("an incomplete distance code of more than one symbol"
           :dynamic (:literals ((97 . 1) (256 . 1))
                     :distance-count 2
                     :distances ((0 . 2) (1 . 2)))
           "The DEFLATE stream ended prematurely.")
          ("a literal/length code without end-of-block"
           :dynamic (:literals ((97 . 1) (98 . 1)))
           "The literal/length table has no end-of-block symbol.")
          ("a repeat code before any length"
           :dynamic (:code-length-symbols ((16 . 0)))
           "A repeat code has no previous length.")
          ("more code lengths than codes"
           :dynamic (:code-length-symbols ((18 . 127) (18 . 127)))
           "A zero repeat exceeds the Huffman table.")
          ("more than 286 literal/length codes"
           :dynamic (:literal-count 287)
           "The DEFLATE literal/length count is reserved.")
          ("more than 30 distance codes"
           :dynamic (:distance-count 31)
           "The DEFLATE distance count is reserved.")
          ("the unused code of a one-symbol literal/length code"
           :dynamic (:literals ((256 . 1)) :distances ((0 . 1)) :body ((:raw 1 . 1)))
           "The DEFLATE stream contains an invalid Huffman code.")
          ("fixed length symbol 286"
           :fixed ((#xC6 . 8))
           "The DEFLATE literal/length symbol is invalid.")
          ("fixed distance symbol 30"
           :fixed ((1 . 7) (30 . 5))
           "The DEFLATE distance symbol is reserved.")
          ("a stored block shorter than its length"
           :hex "010500faff6865"
           "The DEFLATE stream ended prematurely."))
    "rejects ~A"
    (description kind arguments reason)
  (declare (ignore description))
  (expect (inflate-reason (ecase kind
                            (:dynamic (apply #'dynamic-block arguments))
                            (:fixed
                             (apply #'fixed-block
                                    (mapcar
                                     (lambda (code)
                                       (huffman-field (car code) (cdr code)))
                                     arguments)))
                            (:hex (hex-octets arguments))))
          :to-equal reason)))

