;;;; t/unit/util/uuid-random-test.lisp
(in-package #:aitools.util.test)

(defun octet-run (count value)
  (make-array count :element-type '(unsigned-byte 8) :initial-element value))

(defun uuid-shape-p (uuid)
  (and (= (length uuid) 36)
       (every (lambda (index) (char= (char uuid index) #\-)) '(8 13 18 23))
       (every (lambda (char) (or (char= char #\-) (digit-char-p char 16))) uuid)
       (string= uuid (string-downcase uuid))))

(describe "aitools.util.domain uuid-v4-from-octets"
  (it "sets version 4 and the RFC 9562 variant whatever the random bits are"
    (dolist (fill '(0 255 #x5a))
      (let ((uuid (uuid-v4-from-octets (octet-run 16 fill))))
        (expect (uuid-shape-p uuid) :to-be-truthy)
        (expect (uuid-version uuid) :to-be 4)
        (expect (uuid-variant-bits uuid) :to-be 2))))

  (it "keeps the other 122 bits from the input"
    (expect (uuid-v4-from-octets (octet-run 16 255)) :to-equal "ffffffff-ffff-4fff-bfff-ffffffffffff")
    (expect (uuid-v4-from-octets (octet-run 16 0)) :to-equal "00000000-0000-4000-8000-000000000000")))

(describe "aitools.util.domain next-uuid-v7"
  (it "encodes the millisecond timestamp, version 7, and the variant"
    (let ((uuid (next-uuid-v7 (make-uuid-v7-state) 1700000000123 (octet-run 10 255))))
      (expect (uuid-shape-p uuid) :to-be-truthy)
      (expect (uuid-v7-ms uuid) :to-be 1700000000123)
      (expect (uuid-version uuid) :to-be 7)
      (expect (uuid-variant-bits uuid) :to-be 2)))

  (it "increases strictly within one millisecond, even from the largest seed"
    (let* ((state (make-uuid-v7-state))
           (values (loop repeat 50 collect (next-uuid-v7 state 1700000000000 (octet-run 10 255)))))
      (expect (loop for (a b) on values while b always (string< a b)) :to-be-truthy)
      (expect (uuid-v7-ms (first values)) :to-be 1700000000000)))

  (it "borrows the next millisecond when the 12-bit counter overflows"
    (let* ((state (make-uuid-v7-state))
           (values (loop repeat 5000 collect (next-uuid-v7 state 1700000000000 (octet-run 10 0)))))
      (expect (loop for (a b) on values while b always (string< a b)) :to-be-truthy)
      (expect (uuid-v7-ms (car (last values))) :to-be 1700000000001)))

  (it "never decreases when the wall clock steps backwards"
    (let* ((state (make-uuid-v7-state))
           (later (next-uuid-v7 state 1700000005000 (octet-run 10 7)))
           (earlier-clock (next-uuid-v7 state 1700000000000 (octet-run 10 7))))
      (expect (string< later earlier-clock) :to-be-truthy)
      (expect (uuid-v7-ms earlier-clock) :to-be 1700000005000)))

  (it "orders values from successive milliseconds by time"
    (let* ((state (make-uuid-v7-state))
           (first (next-uuid-v7 state 1700000000000 (octet-run 10 255)))
           (second (next-uuid-v7 state 1700000000001 (octet-run 10 0))))
      (expect (string< first second) :to-be-truthy))))

(describe "aitools.util.domain random-alphabet-string"
  (it-each (("hex" "0123456789abcdef") ("alnum" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
            ("base64url" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"))
      "draws exactly LENGTH characters from the ~A alphabet"
      (name characters)
    (let* ((state (make-random-state nil))
           (value (random-alphabet-string name 500 (lambda () (random 256 state)))))
      (expect (length value) :to-be 500)
      (expect (every (lambda (char) (find char characters)) value) :to-be-truthy)))

  (it "discards octets above the largest multiple of the alphabet size"
    (let ((octets (list 248 255 250 61)))
      (expect (random-alphabet-string "alnum" 1 (lambda () (pop octets))) :to-equal "9")
      (expect octets :to-be nil)))

  (it "maps octets through the alphabet by remainder"
    (let ((octets (list 0 17 255)))
      (expect (random-alphabet-string "hex" 3 (lambda () (pop octets))) :to-equal "01f"))))

(describe "aitools.util.domain text-statistics"
  (it "counts chars, bytes, lines, words, and the longest line"
    (let ((stats (text-statistics (format nil "héllo world~%second line here~%") 30)))
      (expect (field stats "chars") :to-be 29)
      (expect (field stats "bytes") :to-be 30)
      (expect (field stats "lines") :to-be 2)
      (expect (field stats "words") :to-be 5)
      (expect (field stats "max_line_chars") :to-be 16)
      (expect (field stats "approx_tokens") :to-be 8)))

  (it "counts an unterminated last line and excludes CR from line length"
    (let ((stats (text-statistics (format nil "ab~C~%abc" #\Return) 7)))
      (expect (field stats "lines") :to-be 2)
      (expect (field stats "max_line_chars") :to-be 3)))

  (it "reports zeros for empty input"
    (let ((stats (text-statistics "" 0)))
      (expect (mapcar #'cdr stats) :to-equal '(0 0 0 0 0 0)))))
