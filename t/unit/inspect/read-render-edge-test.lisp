(in-package #:aitools.inspect.test)

(defun %read-render-test-octets (&rest chunks)
  (coerce (apply #'concatenate '(vector (unsigned-byte 8)) chunks)
          '(simple-array (unsigned-byte 8) (*))))

(describe "read-render hex rows"
  (it "formats a complete row and a final partial row with absolute offsets"
    (let* ((octets (%read-render-test-octets
                    #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
           (rows (hex-rows octets 0 17 :base 32)))
      (expect (length rows) :to-be 2)
      (expect (json-object-get (first rows) "offset") :to-be 32)
      (expect (json-object-get (first rows) "hex")
              :to-equal "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f")
      (expect (json-object-get (second rows) "offset") :to-be 48)
      (expect (json-object-get (second rows) "hex") :to-equal "10"))))

(describe "read-render UTF-8 sequence validation"
  (it "decodes ASCII and two-, three-, and four-byte characters"
    (expect (multiple-value-list
             (aitools.inspect.domain::%utf8-sequence (%read-render-test-octets #(65)) 0 1))
            :to-equal (list #\A 1))
    (expect (multiple-value-list
             (aitools.inspect.domain::%utf8-sequence (%read-render-test-octets #(194 162)) 0 2))
            :to-equal (list (code-char #xA2) 2))
    (expect (multiple-value-list
             (aitools.inspect.domain::%utf8-sequence (%read-render-test-octets #(226 130 172)) 0 3))
            :to-equal (list (code-char #x20AC) 3))
    (expect (multiple-value-list
             (aitools.inspect.domain::%utf8-sequence (%read-render-test-octets #(240 159 152 128)) 0 4))
            :to-equal (list (code-char #x1F600) 4)))

  (it "rejects invalid leads, truncation, bad continuations, overlong forms, and surrogates"
    (expect (multiple-value-list
             (aitools.inspect.domain::%utf8-sequence (%read-render-test-octets #(192 128)) 0 2))
            :to-equal '(nil))
    (expect (multiple-value-list
             (aitools.inspect.domain::%utf8-sequence (%read-render-test-octets #(194)) 0 1))
            :to-equal '(nil))
    (expect (multiple-value-list
             (aitools.inspect.domain::%utf8-sequence (%read-render-test-octets #(194 65)) 0 2))
            :to-equal '(nil))
    (expect (multiple-value-list
             (aitools.inspect.domain::%utf8-sequence (%read-render-test-octets #(224 128 128)) 0 3))
            :to-equal '(nil))
    (expect (multiple-value-list
             (aitools.inspect.domain::%utf8-sequence (%read-render-test-octets #(237 160 128)) 0 3))
            :to-equal '(nil))
    (expect (multiple-value-list
             (aitools.inspect.domain::%utf8-sequence (%read-render-test-octets #(244 144 128 128)) 0 4))
            :to-equal '(nil))))

(describe "read-render string scanning edges"
  (it "keeps a trailing partial character for the next chunk"
    (let* ((partial (%read-render-test-octets #(227 129)))
           (complete (%read-render-test-octets #(227 129 130 65)))
           (found '())
           (scan (make-strings-scan
                  :min-length 2
                  :emit (lambda (offset text cut-offset)
                          (push (list offset text cut-offset) found)
                          nil))))
      (expect (scan-strings-chunk scan partial 20) :to-be 0)
      (expect (scan-strings-chunk scan complete 20 :final t) :to-be 4)
      (expect (nreverse found) :to-equal (list (list 20 (format nil "~C~C" (code-char #x3042) #\A) nil)))))

  (it "emits the kept prefix and byte offset when a run reaches its text cap"
    (let* ((found '())
           (scan (make-strings-scan
                  :min-length 2 :max-text-length 2
                  :emit (lambda (offset text cut-offset)
                          (push (list offset text cut-offset) found)
                          nil)))
           (octets (string-bytes "abcd")))
      (expect (scan-strings-chunk scan octets 100 :final t) :to-be 4)
      (expect (nreverse found) :to-equal '((100 "ab" 102)))))

  (it "stops scanning when the emitter returns :stop"
    (let ((scan (make-strings-scan :min-length 3 :emit (lambda (offset text cut-offset)
                                                        (declare (ignore offset text cut-offset))
                                                        :stop))))
      (expect (scan-strings-chunk scan (string-bytes "abc ") 0 :final t) :to-be nil)))

  (it "extracts only runs inside the requested byte window"
    (let ((found '())
          (octets (%read-render-test-octets (string-bytes "abc") #(0)
                                           (string-bytes "long") #(0)
                                           (string-bytes "tail"))))
      (aitools.inspect.domain::extract-strings
       octets :min-length 4 :start 0 :end 9
       :emit (lambda (offset text) (push (list offset text) found) nil))
      (expect (nreverse found) :to-equal '((4 "long"))))))
