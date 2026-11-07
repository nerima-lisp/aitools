(in-package #:aitools.text.test)

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
