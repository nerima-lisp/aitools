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
            ("a generic four-byte lead" ((#xF1 #x80 #x80 #x80)))
            ("CR runs before LF and at the end" ("a" (13 13 10) "b" (13)))
            ("a last line without a newline" ("a" (10) "bc")))
      "counts valid UTF-8 with ~A exactly as the decoded reference"
      (label parts)
    (declare (ignore label))
    (let ((bytes (apply #'octets parts)))
      (expect (nth-value 4 (aitools.inspect.domain::%valid-utf8-text-counts bytes 0)) :to-be t)
      (expect (multiple-value-list (text-content-counts bytes))
              :to-equal (multiple-value-list (aitools.inspect.domain::%decoded-text-counts bytes 0)))))

  (it-each (("a lone continuation byte" ((#x80)) ((13 10) "c"))
            ("a truncated two-byte sequence" ((#xC3)) ((13 10) "c"))
            ("an overlong E0 sequence" ((#xE0 #x80 #x80)) ((13 10) "c"))
            ("a UTF-16 surrogate" ((#xED #xA0 #x80)) ((13 10) "c"))
            ("a truncated three-byte sequence" ((#xE1 #x80)) ((13 10) "c"))
            ("an overlong F0 sequence" ((#xF0 #x80 #x80 #x80)) ((13 10) "c"))
            ("a code point past U+10FFFF" ((#xF4 #x90 #x80 #x80)) ((13 10) "c"))
            ("a truncated four-byte sequence" ((#xF1 #x80 #x80)) ((13 10) "c"))
            ("a byte that never starts a sequence" ((#xFF)) ((13 10) "c"))
            ("a plain LF and a trailing CR" ((#xFF)) ((10) "c" (#xFF) (13))))
      "falls back to the decoded count for ~A"
      (label parts suffix)
    (declare (ignore label))
    (let ((bytes (apply #'octets "a b" (append parts suffix))))
      (expect (nth-value 4 (aitools.inspect.domain::%valid-utf8-text-counts bytes 0)) :to-be nil)
      (expect (subseq (multiple-value-list (text-content-counts bytes)) 0 2) :to-equal '(2 3))))

  (it-each ((:lf "lf") (:crlf "crlf") (:mixed "mixed") (:none "none"))
      "names line ending ~S ~S"
      (style name)
    (expect (line-ending-name style) :to-equal name)))
