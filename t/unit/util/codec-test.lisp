;;;; t/unit/util/codec-test.lisp
(in-package #:aitools.util.test)

(defparameter *all-octets*
  (let ((vector (make-array 256 :element-type '(unsigned-byte 8))))
    (dotimes (index 256 vector) (setf (aref vector index) index))))

(describe "aitools.util.domain encode-octets"
  (it-each (("" "") ("f" "Zg==") ("fo" "Zm8=") ("foo" "Zm9v") ("foob" "Zm9vYg==") ("fooba" "Zm9vYmE=")
       ("foobar" "Zm9vYmFy"))
      "matches the RFC 4648 section 10 base64 vector for \"~A\""
      (plain encoded)
    (expect (encode-octets "base64" (string-bytes plain)) :to-equal encoded))

  (it "percent-encodes every byte except RFC 3986 unreserved characters"
    (expect (encode-octets "url" (string-bytes "a-Z_0.~ /?=é")) :to-equal "a-Z_0.~%20%2F%3F%3D%C3%A9"))

  (it "writes lowercase hex pairs"
    (expect (encode-octets "hex" (octets 0 15 16 255)) :to-equal "000f10ff")))

(describe "aitools.util.domain decode-text/k"
  (it-each (("base64") ("url") ("hex"))
      "round-trips every byte value through ~A"
      (scheme)
    (multiple-value-bind (kind decoded) (decode scheme (encode-octets scheme *all-octets*))
      (expect kind :to-be :decoded)
      (expect decoded :to-equalp *all-octets*)))

  (it "accepts unpadded base64 and ignores ASCII whitespace between digits"
    (expect (nth-value 1 (decode "base64" "Zm9v
YmE")) :to-equalp (string-bytes "fooba")))

  (it "accepts upper-case hex and whitespace"
    (expect (nth-value 1 (decode "hex" "FF 0a")) :to-equalp (octets 255 10)))

  (it "keeps + literal in URL decoding (not form encoding)"
    (expect (nth-value 1 (decode "url" "a+b%2B")) :to-equalp (string-bytes "a+b+")))

  (it-each (("base64" "Zm9v!" 4) ("base64" "Z" 1) ("base64" "Zm9=v" 4) ("base64" "Zg===" 4) ("base64" "=Zg=" 0)
       ("hex" "abc" 3) ("hex" "0g" 1) ("url" "%4" 0) ("url" "a%zz" 1))
      "rejects malformed ~A input ~S at its offending byte offset"
      (scheme text offset)
    (multiple-value-bind (kind position) (decode scheme text)
      (expect kind :to-be :invalid)
      (expect position :to-be offset)))

  (it "rejects the URL-safe base64 alphabet under the standard scheme"
    (expect (decode "base64" "-_8=") :to-be :invalid)))

(describe "aitools.util.domain UTF-8 helpers"
  (it "reports the offset of invalid UTF-8"
    (expect (octets->utf-8/k (octets 104 105 255) :on-text (lambda (text) text) :on-invalid (lambda (offset) offset))
            :to-be 2))

  (it "replaces invalid sequences leniently"
    (expect (octets->lenient-text (octets 104 255)) :to-equal (format nil "h~C" (code-char #xfffd)))))

(describe "aitools.util.domain decode-text/k error reports"
  (it-each (("base64" "Zg=" 3 "truncated base64 padding")
            ("base64" "Zm9==" 5 "excess base64 padding")
            ("base64" "Z" 1 "truncated base64 group")
            ("base64" "Zm9=v" 4 "data after '=' padding")
            ("base64" "=Zg=" 0 "misplaced '=' padding")
            ("base64" "Zm9v!" 4 "byte is not in the base64 alphabet")
            ("hex" "0g" 1 "byte is not a hex digit")
            ("hex" "abc" 3 "odd number of hex digits")
            ("url" "a%zz" 1 "'%' is not followed by two hex digits"))
      "rejects ~A ~S at offset ~D: ~A"
      (scheme text offset reason)
    (expect (decode-text/k scheme (string-bytes text) :on-decoded (constantly :decoded) :on-invalid #'list)
            :to-equal (list offset reason)))

  (it-each (("Zg==" (102)) ("Zm8=" (102 111)) ("Zm9v" (102 111 111)) ("Zm9vYg" (102 111 111 98)))
      "decodes the padded or unpadded final group of ~S"
      (text expected)
    (expect (coerce (nth-value 1 (decode "base64" text)) 'list) :to-equal expected)))
