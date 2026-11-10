(in-package #:aitools.inspect.test)

(defun %line-domain-test-octets (&rest chunks)
  (coerce (apply #'concatenate '(vector (unsigned-byte 8)) chunks)
          '(simple-array (unsigned-byte 8) (*))))

(describe "decode-line-range edge cases"
  (it "returns an empty result for an inverted range"
    (multiple-value-bind (lines errors cuts)
        (decode-line-range (make-array 0 :element-type '(unsigned-byte 8)) nil 2 1)
      (expect lines :to-equalp #())
      (expect errors :to-be 0)
      (expect cuts :to-be nil)))

  (it "decodes only the requested lines and preserves CR on request"
    (let* ((octets (%line-domain-test-octets
                    (string-bytes (format nil "first~C~Csecond~C" #\Return #\Newline #\Newline))))
           (index (aitools.text.domain:build-line-index octets)))
      (multiple-value-bind (lines errors cuts)
          (decode-line-range octets index 1 2)
        (expect lines :to-equalp #("first" "second"))
        (expect errors :to-be 0)
        (expect cuts :to-be nil))
      (expect (decode-line-range octets index 1 2 :keep-cr t)
              :to-equalp (vector (format nil "first~C" #\Return) "second"))))

  (it "counts invalid UTF-8 while decoding a full line range"
    (let* ((octets (%line-domain-test-octets
                    (string-bytes "bad") #(255) (string-bytes (format nil "~Cok" #\Newline))))
           (index (aitools.text.domain:build-line-index octets)))
      (multiple-value-bind (lines errors cuts)
          (decode-line-range octets index 1 2)
        (expect lines :to-equalp (vector (concatenate 'string "bad" (string (code-char #xFFFD))) "ok"))
        (expect errors :to-be 1)
        (expect cuts :to-be nil))))

  (it "backs a byte cap off a split UTF-8 character and includes CR when asked"
    (let* ((octets (%line-domain-test-octets
                    (string-bytes (format nil "abc~C~C" #\Return #\Newline))
                    (string-bytes (format nil "a~Cz" (code-char #x3042)))))
           (index (aitools.text.domain:build-line-index octets)))
      (multiple-value-bind (lines errors cuts)
          (decode-line-range octets index 1 2 :keep-cr t :max-line-bytes 3)
        (expect lines :to-equalp #("abc" "a"))
        (expect errors :to-be 0)
        (expect cuts :to-equal '((1 . 3) (2 . 6))))
      (multiple-value-bind (lines errors cuts)
          (decode-line-range octets index 1 1 :max-line-bytes 8)
        (expect lines :to-equalp #("abc"))
        (expect errors :to-be 0)
        (expect cuts :to-be nil)))))

(describe "decode-charset-lines/k BOM handling"
  (it "drops a leading BOM, preserves requested CR, and counts replacements"
    (let ((octets (%line-domain-test-octets
                   #(239 187 191)
                   (string-bytes (format nil "one~C~C" #\Return #\Newline))
                   #(255))))
      (expect (decode-charset-lines/k
               octets :utf-8 :keep-cr t
               :on-decoded (lambda (lines replacements) (list lines replacements)))
              :to-equalp (list (vector (format nil "one~C" #\Return)
                                      (string (code-char #xFFFD)))
                              1))))

  (it "passes through input without a BOM and accepts empty input"
    (expect (decode-charset-lines/k
             (string-bytes "plain") :utf-8
             :on-decoded (lambda (lines replacements) (list lines replacements)))
            :to-equalp (list #("plain") 0))
    (expect (decode-charset-lines/k
             (make-array 0 :element-type '(unsigned-byte 8)) :utf-8
             :on-decoded (lambda (lines replacements) (list lines replacements)))
            :to-equalp (list #() 0))))
