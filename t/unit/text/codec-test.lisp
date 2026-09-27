;;;; t/unit/text/codec-test.lisp
;;;;
;;;; CRC-32, DEFLATE, and gzip. Known vectors: the CRC-32 check value from
;;;; the CRC catalogue ("123456789" -> CBF43926), RFC 1951 stored block, and
;;;; streams produced by zlib 1.2.12 (Python's zlib.compressobj(9, DEFLATED,
;;;; -15)) and GNU gzip -n. The dynamic-block vector's input is generated
;;;; below by the same formula used to produce it.
(in-package #:aitools.text.test)

(defun dynamic-vector-input ()
  "bytes(97 + (i**3 + i//13) % 11 for i in range(3000))"
  (let ((result (make-array 3000 :element-type '(unsigned-byte 8))))
    (dotimes (i 3000 result)
      (setf (aref result i) (+ 97 (mod (+ (* i i i) (floor i 13)) 11))))))

(defparameter *dynamic-vector*
  "edcea901c0400c03b059edc479f7e73dde15428504768ccad2179cdc682f8136891c75d07c0bcf27cd85666d6cb98263fdbc1469ebc3422bcaa1b5e6449633e0639bd56250eba89eb0a420f66c7a5940368b527b326c709deb5ce73ad7b9ce75ae739deb5ce7dff900")

(defun only-archive-errors-p (function input)
  "True when FUNCTION on INPUT returns or signals ARCHIVE-ERROR, never any
other condition (an index error would mean an unchecked read)."
  (handler-case (progn (funcall function input) t)
    (archive-error () t)
    (error () nil)))

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

  (it "survives every truncation and many bit flips of a valid stream"
    (let ((valid (hex-octets *dynamic-vector*)))
      (expect (loop for end from 0 below (length valid)
                    always (only-archive-errors-p (lambda (bytes) (inflate bytes)) (subseq valid 0 end)))
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

  (it "compresses repetitive input"
    (expect (< (length (deflate (dynamic-vector-input))) 600) :to-be-truthy)))

(describe "aitools.text.domain gzip"
  (it "reads GNU gzip -n output and checks its CRC"
    (expect (gzip-decompress (hex-octets "1f8b0800000000000003cb48cdc9c9e7020020303a3606000000"))
            :to-equalp (string-bytes (format nil "hello~%"))))

  (it "round-trips with a name and mtime in the header"
    (let ((gz (gzip-compress (string-bytes "payload") :name "a.txt" :mtime 1700000000)))
      (expect (gzip-decompress gz) :to-equalp (string-bytes "payload"))
      (multiple-value-bind (name mtime) (gzip-member-header gz)
        (expect name :to-equal "a.txt")
        (expect mtime :to-be 1700000000))))

  (it "concatenates members and tolerates trailing zeros"
    (let ((two (join-octets (gzip-compress (string-bytes "ab")) (gzip-compress (string-bytes "cd")) (octets 0 0))))
      (expect (gzip-decompress two) :to-equalp (string-bytes "abcd"))))

  (it "rejects a CRC mismatch, a bad magic, and trailing garbage"
    (let ((gz (gzip-compress (string-bytes "payload"))))
      (let ((corrupt (copy-seq gz)))
        (setf (aref corrupt (- (length corrupt) 6)) (logxor 1 (aref corrupt (- (length corrupt) 6))))
        (signals archive-error (gzip-decompress corrupt)))
      (signals archive-error (gzip-decompress (octets 1 2 3 4 5 6 7 8 9 10 11)))
      (signals archive-error (gzip-decompress (join-octets gz (octets 9 9))))))

  (it "bounds output across members"
    (let ((gz (gzip-compress (make-array 50000 :element-type '(unsigned-byte 8) :initial-element 1))))
      (signals archive-limit-exceeded (gzip-decompress gz :max-output 1000)))))

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
    (expect (multiple-value-list (inflate (dynamic-block :literals '((97 . 1) (256 . 1)) :body '(97 97 256))))
            :to-equalp (list (string-bytes "aa") 139))
    (expect (inflate (dynamic-block :literals '((256 . 1)) :body '(256))) :to-equalp (octets))
    (expect (inflate (dynamic-block :literals '((97 . 1) (256 . 1)) :distances '((0 . 1)) :body '(97 256)))
            :to-equalp (string-bytes "a"))
    (expect (inflate (fixed-block (huffman-field (+ #x30 97) 8) (huffman-field 1 7) (huffman-field 0 5) (huffman-field 0 7)))
            :to-equalp (string-bytes "aaaa")))

  (it-each (("an over-subscribed literal/length code"
             :dynamic (:literals ((0 . 1) (1 . 1) (256 . 1))) "invalid literal/length code")
            ("an incomplete literal/length code of more than one symbol"
             :dynamic (:literals ((97 . 2) (256 . 2))) "invalid literal/length code")
            ("an over-subscribed distance code"
             :dynamic (:literals ((97 . 1) (256 . 1)) :distance-count 3 :distances ((0 . 1) (1 . 1) (2 . 1)))
             "invalid distance code")
            ("an incomplete distance code of more than one symbol"
             :dynamic (:literals ((97 . 1) (256 . 1)) :distance-count 2 :distances ((0 . 2) (1 . 2)))
             "invalid distance code")
            ("a literal/length code without end-of-block"
             :dynamic (:literals ((97 . 1) (98 . 1))) "no end-of-block code")
            ("a repeat code before any length" :dynamic (:code-length-symbols ((16 . 0)))
             "repeat with no previous length")
            ("more code lengths than codes" :dynamic (:code-length-symbols ((18 . 127) (18 . 127)))
             "too many code lengths")
            ("more than 286 literal/length codes" :dynamic (:literal-count 287) "too many length or distance codes")
            ("more than 30 distance codes" :dynamic (:distance-count 31) "too many length or distance codes")
            ("the unused code of a one-symbol literal/length code"
             :dynamic (:literals ((256 . 1)) :body ((:raw 1 . 1))) "deflate stream uses an undefined Huffman code")
            ("fixed length symbol 286" :fixed ((#xC6 . 8)) "invalid length code")
            ("fixed distance symbol 30" :fixed ((1 . 7) (30 . 5)) "deflate stream uses an undefined Huffman code")
            ("a stored block shorter than its length" :hex "010500faff6865" "stored block is truncated"))
      "rejects ~A"
      (description kind arguments reason)
    (declare (ignore description))
    (expect (inflate-reason (ecase kind
                              (:dynamic (apply #'dynamic-block arguments))
                              (:fixed (apply #'fixed-block (mapcar (lambda (code) (huffman-field (car code) (cdr code)))
                                                                   arguments)))
                              (:hex (hex-octets arguments))))
            :to-equal reason)))

(defun gzip-reason (function)
  (handler-case (progn (funcall function) :no-error)
    (archive-error (condition) (list (type-of condition) (archive-error-reason condition)))))

(describe "aitools.text.domain gzip member headers"
  (it "skips FEXTRA, FCOMMENT, and FHCRC and reads a Latin-1 FNAME"
    (expect (multiple-value-list
             (gzip-member-header (octets #x1f #x8b 8 (logior 2 4 8 16) 1 0 0 0 0 3 2 0 9 9 #xE9 0 67 0 1 2)))
            :to-equal (list (string (code-char #xE9)) 1 20)))

  (it-each (("shorter than a header" (#x1f #x8b 8 0 0 0 0) (archive-error "gzip header is truncated"))
            ("without the magic" (1 2 8 0 0 0 0 0 0 3) (archive-error "not a gzip stream"))
            ("with another method" (#x1f #x8b 7 0 0 0 0 0 0 3) (archive-unsupported "gzip compression method"))
            ("with a reserved flag" (#x1f #x8b 8 #x20 0 0 0 0 0 3) (archive-error "reserved gzip flags are set"))
            ("with FEXTRA but no length" (#x1f #x8b 8 4 0 0 0 0 0 3) (archive-error "field is truncated"))
            ("with an unterminated FNAME" (#x1f #x8b 8 8 0 0 0 0 0 3 65 66)
             (archive-error "unterminated gzip header string"))
            ("with FHCRC cut short" (#x1f #x8b 8 2 0 0 0 0 0 3 0) (archive-error "gzip header is truncated")))
      "rejects a header ~A"
      (description header expected)
    (declare (ignore description))
    (expect (gzip-reason (lambda () (gzip-member-header (apply #'octets header)))) :to-equal expected))

  (it "rejects a cut trailer and an ISIZE that disagrees with the data"
    (let ((gz (gzip-compress (string-bytes "payload"))))
      (expect (gzip-reason (lambda () (gzip-decompress (subseq gz 0 (- (length gz) 2)))))
              :to-equal '(archive-error "field is truncated"))
      (let ((wrong-size (copy-seq gz)))
        (setf (aref wrong-size (- (length wrong-size) 4)) 99)
        (expect (gzip-reason (lambda () (gzip-decompress wrong-size)))
                :to-equal '(archive-error "gzip length mismatch"))))))
