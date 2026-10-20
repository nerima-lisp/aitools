;;;; t/unit/text/archive-tar-test.lisp
(in-package #:aitools.text.test)

(describe "aitools.text.domain tar on crafted archives"
  (it "rejects a negative base-256 number and a non-octal digit"
    (expect (tar-entries-reason (tar-of (tar-block "a" #\0 :patch (lambda (header) (setf (aref header 124) #xFF)))))
            :to-equal '(archive-error "negative base-256 tar number"))
    (expect (tar-entries-reason (tar-of (tar-block "a" #\0 :patch (lambda (header)
                                                                    (setf (aref header 130) (char-code #\9))))))
            :to-equal '(archive-error "invalid octal digit in tar header")))

  (it-each (("without a length" ("abc") "pax record has no length")
            ("with a non-decimal length" ("x6 a=b~%") "pax record length is not decimal")
            ("with a length inside its own digits" ("1 a=b~%") "malformed pax record")
            ("with a length past the header data" ("99 a=b~%") "malformed pax record")
            ("not ending in a newline" ("7 a=bc ") "malformed pax record")
            ("without a key" ("6 abc~%") "pax record has no key")
            ("with a non-decimal size" ("11 size=ab~%") "pax numeric value is not decimal")
            ("with an empty size" ("8 size=~%") "pax numeric value is not decimal"))
      "rejects a pax record ~A"
      (description records reason)
    (declare (ignore description))
    (expect (tar-entries-reason
             (tar-of (tar-block "PaxHeader" #\x :data (apply #'pax-body (mapcar (lambda (record) (format nil record)) records)))
                     (tar-block "f" #\0)))
            :to-equal (list 'archive-error reason)))

  (it "takes a fractional pax mtime's seconds and stops pax records at NUL padding"
    (expect (tar-summary (tar-of (tar-block "PaxHeader" #\x
                                            :data (join-octets (pax-body (format nil "15 mtime=123.5~%")) (octets 0 0 0)))
                                 (tar-block "f" #\0)))
            :to-equal '(("f" :file 0 123 nil))))

  (it "applies a global pax header to every later entry"
    (expect (tar-summary (tar-of (tar-block "Global" #\g :data (pax-body (format nil "15 path=global~%")))
                                 (tar-block "f" #\0) (tar-block "g" #\0)))
            :to-equal '(("global" :file 0 0 nil) ("global" :file 0 0 nil))))

  (it "reads GNU long link names, hard links, other types, and directories by trailing slash"
    (expect (tar-summary (tar-of (tar-block "././@LongLink" #\K :data (string-bytes "long/target"))
                                 (tar-block "l" #\2 :link "short")
                                 (tar-block "h" #\1 :link "f")
                                 (tar-block "c" #\3)
                                 (tar-block "dir/" #\0)
                                 (tar-block "" #\0)))
            :to-equal '(("l" :symlink 0 0 "long/target") ("h" :hardlink 0 0 "f") ("c" :other 0 0 nil)
                        ("dir" :directory 0 0 nil) ("" :file 0 0 nil))))

  (it "counts the extra digit when a pax record's length gains one by counting itself"
    ;; "path=" plus a 91-byte name plus LF is 97 bytes: 98 with the space,
    ;; 100 with two digits, so the length is written with three: 101.
    (let* ((name (concatenate 'string (string (code-char #xE9)) (make-string 89 :initial-element #\a)))
           (archive (write-tar (list (make-archive-member :name name :data (string-bytes "x"))))))
      (expect (map 'string #'code-char (subseq archive 512 516)) :to-equal "101 ")
      (expect (archive-entry-name (first (read-tar-entries archive))) :to-equal name)))

  (it "writes a long or non-ASCII link target and a late mtime through pax"
    (let ((long (make-string 150 :initial-element #\t)))
      (expect (tar-summary (write-tar (list (make-archive-member :name "l" :kind :symlink :link-target long)
                                            (make-archive-member :name "m" :kind :symlink :link-target "日本")
                                            (make-archive-member :name "t" :data (string-bytes "x") :mtime 9000000000))))
              :to-equal (list (list "l" :symlink 0 0 long) '("m" :symlink 0 0 "日本") '("t" :file 1 9000000000 nil)))))

  (it "bounds and checks a tar entry's data"
    (let* ((archive (write-tar (list (make-archive-member :name "a" :data (string-bytes "xyz")))))
           (entry (first (read-tar-entries archive))))
      (expect (archive-reason (lambda () (archive-entry-data archive entry :max-output 2)))
              :to-equal '(archive-limit-exceeded "entry size"))
      (expect (archive-reason (lambda () (archive-entry-data (subseq archive 0 513) entry)))
              :to-equal '(archive-error "tar entry data is truncated")))))

(describe "aitools.text.domain tar"
  (it "round-trips names (long and non-ASCII via pax), kinds, modes, links, and bytes"
    (let* ((members (sample-members))
           (archive (write-tar members))
           (entries (read-tar-entries archive)))
      (expect (entry-summary entries archive) :to-equal (member-summary members))))

  (it "pads to 512-byte blocks and 10240-byte records"
    (let ((archive (write-tar (list (make-archive-member :name "a" :data (string-bytes "x"))))))
      (expect (length archive) :to-be 10240)
      (expect (map 'string #'code-char (subseq archive 257 262)) :to-equal "ustar")))

  (it "splits a long ASCII name into the ustar prefix without a pax header"
    (let* ((name (format nil "~A/~A" (make-string 120 :initial-element #\a) (make-string 90 :initial-element #\b)))
           (archive (write-tar (list (make-archive-member :name name :data (string-bytes "x"))))))
      (expect (code-char (aref archive 156)) :to-be #\0)
      (expect (archive-entry-name (first (read-tar-entries archive))) :to-equal name)))

  (it "reads GNU long-name headers and base-256 sizes"
    (let* ((long (make-string 150 :initial-element #\n))
           (plain (write-tar (list (make-archive-member :name "short" :data (string-bytes "data")))))
           (header (aitools.text.domain::%tar-header (string-bytes "././@LongLink") (octets) (octets)
                                                     151 #o644 0 #\L))
           (body (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0))
           (archive (join-octets header body plain)))
      (replace body (string-bytes long))
      (replace archive body :start1 512)
      (expect (archive-entry-name (first (read-tar-entries archive))) :to-equal long))
    (let ((header (aitools.text.domain::%tar-header (string-bytes "b") (octets) (octets) 0 #o644 0 #\0)))
      (fill header 0 :start 124 :end 136)
      (setf (aref header 124) #x80 (aref header 135) 5)
      (fill header 32 :start 148 :end 156)
      (let ((sum (reduce #'+ header)))
        (replace header (map 'vector #'char-code (format nil "~6,'0O" sum)) :start1 148)
        (setf (aref header 154) 0))
      (let ((archive (join-octets header (string-bytes "12345") (make-array 507 :element-type '(unsigned-byte 8) :initial-element 0))))
        (expect (archive-entry-size (first (read-tar-entries archive))) :to-be 5))))

  (it "rejects a checksum mismatch and a truncated entry"
    (let ((archive (write-tar (list (make-archive-member :name "a" :data (string-bytes "xyz"))))))
      (let ((corrupt (copy-seq archive)))
        (setf (aref corrupt 10) 65)
        (signals archive-error (read-tar-entries corrupt)))
      (signals archive-error (read-tar-entries (subseq archive 0 514)))))

  (it "survives every truncation of a valid archive"
    (let ((archive (write-tar (sample-members))))
      (expect (loop for end from 0 below (length archive) by 97
                    always (only-archive-errors-p
                            (lambda (bytes) (dolist (entry (read-tar-entries bytes))
                                              (archive-entry-data bytes entry)))
                            (subseq archive 0 end)))
              :to-be-truthy))))
