(in-package #:aitools.inspect.test)

(defun octets (&rest parts)
  "PARTS (strings and byte lists) concatenated as an octet vector."
  (coerce (loop for part in parts
                append (if (stringp part) (coerce (string-bytes part) 'list) part))
          '(simple-array (unsigned-byte 8) (*))))

(describe "text-content-counts on bytes"
  (it-each (("two-byte characters" ("caf" (#xC3 #xA9) " ok"))
            ("three-byte characters and U+3000 between words" ((#xE6 #x97 #xA5) (#xE3 #x80 #x80) "x"))
            ("E0 and ED lead bytes at their range edges" ((#xE0 #xA0 #x80) (#xED #x9F #xBF)))
            ("four-byte characters, F0 and F4 at their range edges" ((#xF0 #x90 #x80 #x80) " " (#xF4 #x8F #xBF #xBF)))
            ("CR runs before LF and at the end" ("a" (13 13 10) "b" (13)))
            ("a last line without a newline" ("a" (10) "bc")))
      "counts valid UTF-8 with ~A exactly as the decoded reference"
      (label parts)
    (declare (ignore label))
    (let ((bytes (apply #'octets parts)))
      (expect (nth-value 4 (aitools.inspect.domain::%valid-utf8-text-counts bytes 0)) :to-be t)
      (expect (multiple-value-list (text-content-counts bytes))
              :to-equal (multiple-value-list (aitools.inspect.domain::%decoded-text-counts bytes 0)))))

  (it-each (("a lone continuation byte" ((#x80)))
            ("a truncated two-byte sequence" ((#xC3)))
            ("an overlong E0 sequence" ((#xE0 #x80 #x80)))
            ("a UTF-16 surrogate" ((#xED #xA0 #x80)))
            ("an overlong F0 sequence" ((#xF0 #x80 #x80 #x80)))
            ("a code point past U+10FFFF" ((#xF4 #x90 #x80 #x80)))
            ("a byte that never starts a sequence" ((#xFF))))
      "falls back to the decoded count for ~A"
      (label parts)
    (declare (ignore label))
    (let ((bytes (apply #'octets "a b" (append parts (list (list 13 10) "c")))))
      (expect (nth-value 4 (aitools.inspect.domain::%valid-utf8-text-counts bytes 0)) :to-be nil)
      ;; Two lines; the invalid bytes become U+FFFD inside the first line's second word.
      (expect (subseq (multiple-value-list (text-content-counts bytes)) 0 2) :to-equal '(2 3))))

  (it-each ((:lf "lf") (:crlf "crlf") (:mixed "mixed") (:none "none"))
      "names line ending ~S ~S"
      (style name)
    (expect (line-ending-name style) :to-equal name)))
