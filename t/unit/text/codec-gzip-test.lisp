(in-package #:aitools.text.test)

(defun gzip-reason (function)
  (handler-case (progn (funcall function) :no-error)
    (archive-error (condition) (list (type-of condition) (archive-error-reason condition)))))

(describe "aitools.text.domain gzip member headers"
  (it "skips FEXTRA, FCOMMENT, and FHCRC and reads a Latin-1 FNAME"
    (expect (multiple-value-list
             (gzip-member-header (octets #x1f #x8b 8 (logior 2 4 8 16) 1 0 0 0 0 3 2 0 9 9 #xE9 0 67 0 #xB9 #x0B)))
            :to-equal (list (string (code-char #xE9)) 1 20)))
  (it "validates gzip member header offsets"
    (let* ((prefix (octets #xAA #xBB))
           (gzip (gzip-compress (string-bytes "payload")
                                :name "x"
                                :mtime 1700000000))
           (input (join-octets prefix gzip)))
      (expect (multiple-value-list (gzip-member-header input :start (length prefix)))
              :to-equal (list "x" 1700000000 14))
      (signals archive-error (gzip-member-header input :start -1))))

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

  (it-each (("FNAME with a path" "dir/x" "bundle.gz" "bundle")
            ("FNAME with a traversal path" "../escape" "bundle.gz" "bundle")
            ("empty FNAME" "" "bundle.gz" "bundle")
            ("a.gz without FNAME" nil "a.gz" "a")
            ("A.GZ without FNAME" nil "A.GZ" "A")
            (".gz without FNAME" nil ".gz" ".gz.out")
            ("extensionless archive without FNAME" nil "archive" "archive.out")
            ("NIL archive path without FNAME" nil nil ".out")
            ("empty archive path without FNAME" nil "" ".out")
            ("a.gz.bak without FNAME" nil "a.gz.bak" "a")
            ("dot FNAME falls back" "." "bundle.gz" "bundle")
            ("a dot fallback is unsafe" nil "..gz" ".out"))
      "names a gzip member ~A"
      (description member-name archive-path expected)
    (let ((gzip (if member-name
                    (gzip-compress (string-bytes "payload") :name member-name)
                    (gzip-compress (string-bytes "payload")))))
      (expect (gzip-member-name gzip archive-path) :to-equal expected)))

  (it "rejects every strict gzip prefix and an ISIZE that disagrees with the data"
    (let ((gz (gzip-compress (string-bytes "payload") :name "prefix")))
      (loop for end from 0 below (length gz)
            do (expect (first (gzip-reason
                               (lambda () (gzip-decompress (subseq gz 0 end)))))
                       :to-equal (quote archive-error))))
    (let ((gz (gzip-compress (string-bytes "payload"))))
      (expect (gzip-reason (lambda () (gzip-decompress (subseq gz 0 (- (length gz) 2)))))
              :to-equal (list (quote archive-error) "field is truncated"))
      (let ((wrong-size (copy-seq gz)))
        (setf (aref wrong-size (- (length wrong-size) 4)) 99)
        (expect (gzip-reason (lambda () (gzip-decompress wrong-size)))
                :to-equal (list (quote archive-error) "gzip length mismatch"))))))

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

  (it "round-trips a multibyte UTF-8 FNAME"
    (let* ((name "日本語.txt")
           (payload (string-bytes "payload"))
           (gzip (gzip-compress payload :name name)))
      (expect (gzip-decompress gzip) :to-equalp payload)
      (multiple-value-bind (read-name mtime data-start)
          (gzip-member-header gzip)
        (declare (ignore mtime))
        (expect read-name :to-equal name)
        (expect (subseq gzip 10 (1- data-start))
                :to-equalp (encode-utf8 name)))))

  (it "concatenates members and tolerates trailing zeros"
    (let ((two (join-octets (gzip-compress (string-bytes "ab")) (gzip-compress (string-bytes "cd")) (octets 0 0))))
      (expect (gzip-decompress two) :to-equalp (string-bytes "abcd"))))

  (it "preserves gzip error categories across malformed headers and data"
    (let* ((gzip (gzip-compress (string-bytes "payload")))
           (corrupt (copy-seq gzip)))
      (setf (aref corrupt (- (length corrupt) 6))
            (logxor 1 (aref corrupt (- (length corrupt) 6))))
      (expect (gzip-reason (lambda () (gzip-decompress corrupt)))
              :to-equal (list (quote archive-error) "gzip checksum mismatch"))
      (let* ((header (octets #x1f #x8b 8 (logior 2 4 8 16)
                             1 0 0 0 0 3 2 0 9 9 #xE9 0 67 0 #xB9 #x0B))
             (with-header (join-octets header (subseq gzip 10)))
             (bad-header (copy-seq with-header)))
        (setf (aref bad-header 18) (logxor 1 (aref bad-header 18)))
        (expect (gzip-reason (lambda () (gzip-decompress bad-header)))
                :to-equal (list (quote archive-error) "gzip checksum mismatch"))
        (expect (gzip-reason (lambda () (gzip-member-header bad-header)))
                :to-equal (list (quote archive-error) "gzip header CRC-32 mismatch")))
      (expect (gzip-reason
               (lambda ()
                 (gzip-decompress (octets #x1f #x8b 7 0 0 0 0 0 0 3))))
              :to-equal (list (quote archive-unsupported) "gzip compression method"))
      (let ((bad-deflate (copy-seq gzip)))
        (setf (aref bad-deflate 10) 7)
        (expect (first (gzip-reason
                        (lambda () (gzip-decompress bad-deflate))))
                :to-equal (quote archive-error)))
      (signals archive-error (gzip-decompress (octets 1 2 3 4 5 6 7 8 9 10 11)))
      (signals archive-error (gzip-decompress (join-octets gzip (octets 9 9))))))

  (it "copies full-capacity kit output to simple octets and enforces limits"
    (let* ((single-data
            (let ((data (make-array 1048576
                                    :element-type (quote (unsigned-byte 8))))
                  (state #x12345678))
              (dotimes (index (length data) data)
                (setf state
                      (logand #xffffffff (+ (* state 1664525) 1013904223))
                      (aref data index)
                      (ldb (byte 8 24) state)))))
           (single (gzip-compress single-data))
           (decoded (gzip-decompress single)))
      (multiple-value-bind (kit-output header next)
          (deflate-kit:gzip-decode-prefix single :max-output most-positive-fixnum)
        (declare (ignore header next))
        (expect (and (adjustable-array-p kit-output)
                     (= (fill-pointer kit-output) (array-total-size kit-output)))
                :to-equal t)
        (expect (typep decoded '(simple-array (unsigned-byte 8) (*))) :to-equal t))
      (expect decoded :to-equalp single-data)
      (signals archive-limit-exceeded (gzip-decompress single :max-output 1000))
      (expect (gzip-reason (lambda () (gzip-decompress single :max-output -1)))
              :to-equal (list (quote archive-error) "invalid gzip output limit"))
      (expect (gzip-reason (lambda () (gzip-decompress single :max-output "bad")))
              :to-equal (list (quote archive-error) "invalid gzip output limit")))
    (let* ((first (make-array 600 :element-type '(unsigned-byte 8) :initial-element 1))
           (second (make-array 600 :element-type '(unsigned-byte 8) :initial-element 2))
           (first-member (gzip-compress first))
           (second-member (gzip-compress second))
           (both (join-octets first-member second-member)))
      (expect (gzip-decompress first-member :max-output 1000) :to-equalp first)
      (expect (gzip-decompress second-member :max-output 1000) :to-equalp second)
      (progn
        (signals archive-limit-exceeded (gzip-decompress both :max-output 1000))
        (signals archive-limit-exceeded (gzip-decompress both :max-output 1199)))
      (let ((decoded (gzip-decompress both :max-output 1200)))
        (expect (typep decoded (quote (simple-array (unsigned-byte 8) (*)))) :to-equal t)
        (expect decoded :to-equalp (join-octets first second))))))
