;;;; t/unit/text/charset-test.lisp
;;;;
;;;; UTF-8 policies and `transcode`'s encodings. Byte vectors for
;;;; Shift_JIS and EUC-JP are the JIS code points of the characters (e.g.
;;;; 日 = JIS 0x467C = SJIS 93 FA = EUC C6 FC), cross-checked against the
;;;; generated glibc tables.
(in-package #:aitools.text.test)

(defun decode (bytes encoding &key replace)
  (decode-octets/k bytes encoding :replace replace
                                  :on-decoded (lambda (string replacements) (list string replacements))
                                  :on-invalid (lambda (position) (list :invalid position))))

(defun encode (string encoding &key replace)
  (encode-string/k string encoding :replace-unmappable replace
                                   :on-encoded (lambda (bytes replaced) (list (coerce bytes 'list) replaced))
                                   :on-unmappable (lambda (index char) (list :unmappable index char))))

(describe "aitools.text.domain UTF-8"
  (it "replaces invalid bytes and counts them, not genuine U+FFFD"
    (multiple-value-bind (string errors) (decode-utf8 (octets 104 #xFF 105 #xEF #xBF #xBD #xC3))
      (expect string :to-equal (coerce (list #\h (code-char #xFFFD) #\i (code-char #xFFFD) (code-char #xFFFD)) 'string))
      (expect errors :to-be 2)))

  (it "reports zero errors for valid text"
    (expect (nth-value 1 (decode-utf8 (string-bytes "日本語 ok"))) :to-be 0))

  (it "decodes strictly for writes, reporting the first bad byte offset"
    (expect (decode-utf8-strict/k (octets 97 98 #xC3 #x28)
                                  :on-decoded (lambda (string) string)
                                  :on-invalid (lambda (position) (list :invalid position)))
            :to-equal '(:invalid 2))
    (expect (utf8-valid-p (string-bytes "é")) :to-be-truthy)
    (expect (coerce (encode-utf8 "é") 'list) :to-equal '(#xC3 #xA9))))

(describe "aitools.text.domain legacy encodings"
  (it "names encodings canonically and accepts aliases"
    (expect (find-encoding "CP932") :to-be :shift_jis)
    (expect (find-encoding "EUC-JP") :to-be :euc-jp)
    (expect (find-encoding "latin1") :to-be :iso-8859-1)
    (expect (find-encoding "ebcdic") :to-be-falsy)
    (expect (mapcar #'encoding-name *supported-encodings*)
            :to-equal '("utf-8" "shift_jis" "euc-jp" "iso-8859-1" "utf-16le" "utf-16be")))

  (it "encodes and decodes Shift_JIS (CP932) known vectors"
    (expect (encode "日本語ｱ" :shift_jis) :to-equal '((#x93 #xFA #x96 #x7B #x8C #xEA #xB1) 0))
    (expect (decode (octets #x93 #xFA #x96 #x7B #x8C #xEA #xB1) :shift_jis) :to-equal '("日本語ｱ" 0))
    (expect (encode (string (code-char #x2460)) :shift_jis) :to-equal '((#x87 #x40) 0)))

  (it "decodes CP932's duplicate codes but encodes to the preferred one"
    (expect (decode (octets #x87 #x90) :shift_jis) :to-equal (list (string (code-char #x2252)) 0))
    (expect (encode (string (code-char #x2252)) :shift_jis) :to-equal '((#x81 #xE0) 0)))

  (it "encodes and decodes EUC-JP including half-width kana and JIS X 0212"
    (expect (encode "日ｱ丂" :euc-jp) :to-equal '((#xC6 #xFC #x8E #xB1 #x8F #xB0 #xA1) 0))
    (expect (decode (octets #xC6 #xFC #x8E #xB1 #x8F #xB0 #xA1) :euc-jp) :to-equal '("日ｱ丂" 0)))

  (it "round-trips every reversible mapping of both tables"
    (flet ((round-trip-failures (triples encoding)
             (loop for i from 0 below (length triples) by 3
                   for code = (aref triples i)
                   for char = (code-char (aref triples (1+ i)))
                   when (= 1 (aref triples (+ i 2)))
                     unless (let ((bytes (first (encode (string char) encoding))))
                              (and (= code (reduce (lambda (a b) (+ (* a 256) b)) bytes))
                                   (equal (decode (coerce bytes '(simple-array (unsigned-byte 8) (*))) encoding)
                                          (list (string char) 0))))
                       collect code)))
      (expect (round-trip-failures aitools.data:*cp932-mapping* :shift_jis) :to-equal nil)
      (expect (round-trip-failures aitools.data:*euc-jp-mapping* :euc-jp) :to-equal nil)))

  (it "reports or replaces undecodable bytes, keeping an ASCII trail byte"
    (expect (decode (octets #x41 #x81 #x20) :shift_jis) :to-equal '(:invalid 1))
    (expect (decode (octets #x41 #x81 #x20) :shift_jis :replace t)
            :to-equal (list (coerce (list #\A (code-char #xFFFD) #\Space) 'string) 1))
    (expect (decode (octets #xA1) :euc-jp) :to-equal '(:invalid 0)))

  (it "reports or replaces unmappable characters"
    (expect (encode "a日" :iso-8859-1) :to-equal (list :unmappable 1 #\日))
    (expect (encode "a日" :iso-8859-1 :replace t) :to-equal '((97 63) 1))
    (expect (encode "a😀" :shift_jis :replace t) :to-equal '((97 63) 1)))

  (it "round-trips UTF-16 with surrogate pairs and ISO-8859-1"
    (expect (encode "a😀" :utf-16le) :to-equal '((97 0 #x3D #xD8 0 #xDE) 0))
    (expect (encode "a😀" :utf-16be) :to-equal '((0 97 #xD8 #x3D #xDE 0) 0))
    (expect (decode (octets 97 0 #x3D #xD8 0 #xDE) :utf-16le) :to-equal '("a😀" 0))
    (expect (decode (octets #xE9) :iso-8859-1) :to-equal '("é" 0))
    (expect (decode (octets #x3D #xD8) :utf-16le) :to-equal '(:invalid 0))))

(describe "aitools.text.domain charset edge paths"
  (it "replaces an invalid Shift_JIS pair, swallowing its non-ASCII trail byte"
    (expect (decode-octets/k (octets #x81 #xFF #x41) :shift_jis :replace t
                             :on-decoded (lambda (string replacements) (list (map 'list #'char-code string) replacements)))
            :to-equal '((#xFFFD #x41) 1))
    (expect (decode-octets/k (octets #x41 #x81) :shift_jis :replace t
                             :on-decoded (lambda (string replacements) (list (map 'list #'char-code string) replacements)))
            :to-equal '((#x41 #xFFFD) 1)))

  (it "replaces through the Unicode codecs, counting none for ISO-8859-1"
    (expect (decode-octets/k (octets #x41 0 #x42) :utf-16le :replace t
                             :on-decoded (lambda (string replacements) (list (map 'list #'char-code string) replacements)))
            :to-equal '((#x41 #xFFFD) 1))
    (expect (decode-octets/k (octets #xE9) :iso-8859-1 :replace t :on-decoded #'list)
            :to-equal (list (string (code-char #xE9)) 0)))

  (it "returns NIL for an invalid input or unmappable character when no handler is given"
    (expect (decode-octets/k (octets #xFF) :utf-8 :on-decoded #'list) :to-be nil)
    (expect (encode-string/k (string (code-char #xD800)) :utf-8 :on-encoded #'list) :to-be nil)
    (expect (encode-string/k (string (code-char #xD800)) :utf-8 :on-encoded #'list
                             :on-unmappable (lambda (index char) (list index (char-code char))))
            :to-equal '(0 #xD800))))
