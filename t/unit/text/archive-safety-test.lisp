(in-package #:aitools.text.test)

(describe "aitools.text.domain archive format detection on odd input"
  (it "names nothing for a block whose checksum is not a number or a too-short prefix"
    (let ((block (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
      (setf (aref block 0) 97)
      (fill block (char-code #\9) :start 148 :end 156)
      (expect (detect-archive-format block "x") :to-be-falsy))
    (expect (detect-archive-format (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0) "x")
            :to-be-falsy)
    (expect (detect-archive-format (octets #x50) "a") :to-be-falsy))

  (it "calls a gzip whose content does not inflate, or with a short name, plain gzip"
    (expect (detect-archive-format (octets #x1f #x8b 8 0 0 0 0 0 0 3 7) "x.gz") :to-be :gz)
    (expect (detect-archive-format (gzip-compress (string-bytes "x")) "a") :to-be :gz)))

(describe "aitools.text.domain archive format detection"
  (it "detects zip, tar, gzip, and tar inside gzip by content"
    (let ((tar (write-tar (list (make-archive-member :name "a.txt" :data (string-bytes "abc"))))))
      (expect (detect-archive-format (write-zip (sample-members)) "x.zip") :to-be :zip)
      (expect (detect-archive-format tar "x.tar") :to-be :tar)
      (expect (detect-archive-format tar "renamed.bin") :to-be :tar)
      (expect (detect-archive-format (gzip-compress tar) "bundle.gz") :to-be :tar-gz)
      (expect (detect-archive-format (gzip-compress (string-bytes "plain text")) "notes.gz") :to-be :gz)
      (expect (detect-archive-format (gzip-compress (string-bytes "plain text")) "notes.tgz") :to-be :tar-gz)
      (expect (detect-archive-format (string-bytes "plain text") "notes.txt") :to-be-falsy)))

  (it "accepts a v7 tar without the ustar magic when its header checksum is valid"
    (let ((old (v7-tar (make-archive-member :name "a.txt" :data (string-bytes "abc")))))
      (expect (map 'string #'code-char (subseq old 257 262)) :not :to-equal "ustar")
      (expect (archive-entry-name (first (read-tar-entries old))) :to-equal "a.txt")
      (expect (detect-archive-format old "old.tar") :to-be :tar)
      (expect (detect-archive-format old "old") :to-be :tar)
      (expect (detect-archive-format (gzip-compress old) "old.gz") :to-be :tar-gz)))

  (it "names each format"
    (expect (mapcar #'archive-format-name '(:zip :tar :tar-gz :gz)) :to-equal (list "zip" "tar" "tar.gz" "gz"))))

(describe "aitools.text.domain archive path and reader limits"
  (it "flags absolute, traversing, backslashed, NUL, and empty names"
    (expect (archive-entry-path-problem "a/b.txt") :to-be-falsy)
    (expect (archive-entry-path-problem "a/..b/c") :to-be-falsy)
    (expect (archive-entry-path-problem "/etc/passwd") :to-be :absolute)
    (expect (archive-entry-path-problem "C:/x") :to-be :absolute)
    (expect (archive-entry-path-problem "1:/x") :to-be-falsy)
    (expect (archive-entry-path-problem "a/../../b") :to-be :parent-traversal)
    (expect (archive-entry-path-problem "..") :to-be :parent-traversal)
    (expect (archive-entry-path-problem "a\\..\\b") :to-be :backslash)
    (expect (archive-entry-path-problem (format nil "a~Cb" (code-char 0))) :to-be :nul)
    (expect (archive-entry-path-problem "/") :to-be :empty))

  (it "accepts the path byte limit and rejects one byte more in tar and zip"
    (flet ((path-with-lengths (lengths)
             (format nil "~{~A~^/~}"
                     (mapcar (lambda (length) (make-string length :initial-element #\a))
                             lengths)))
           (tar-name (name)
             (read-tar-entries (write-tar (list (make-archive-member :name name :data (octets))))))
           (zip-name (name)
             (read-zip-entries (write-zip (list (make-archive-member :name name :data (octets)))))))
      (let ((exact (path-with-lengths (cons 239 (make-list 16 :initial-element 240))))
            (over (path-with-lengths (make-list 17 :initial-element 240))))
        (expect (length (tar-name exact)) :to-be 1)
        (expect (length (zip-name exact)) :to-be 1)
        (signals archive-limit-exceeded (tar-name over))
        (signals archive-limit-exceeded (zip-name over)))))

  (it "measures component limits in UTF-8 bytes for tar and zip"
    (flet ((tar-name (name)
             (read-tar-entries (write-tar (list (make-archive-member :name name :data (octets))))))
           (zip-name (name)
             (read-zip-entries (write-zip (list (make-archive-member :name name :data (octets)))))))
      (let ((exact (make-string +archive-max-component-bytes+ :initial-element #\a))
            (exact-utf8 (concatenate 'string
                                     (make-string 251 :initial-element (code-char 97))
                                     (string (code-char #x1F600))))
            (over (make-string (1+ +archive-max-component-bytes+) :initial-element #\a))
            (over-utf8 (make-string 64 :initial-element (code-char #x1F600))))
        (expect (length (tar-name exact)) :to-be 1)
        (expect (length (zip-name exact)) :to-be 1)
        (expect (length (tar-name exact-utf8)) :to-be 1)
        (expect (length (zip-name exact-utf8)) :to-be 1)
        (signals archive-limit-exceeded (tar-name over))
        (signals archive-limit-exceeded (zip-name over))
        (signals archive-limit-exceeded (tar-name over-utf8))
        (signals archive-limit-exceeded (zip-name over-utf8)))))

  (it "accepts the maximum-depth path at the byte limit and rejects a path over both limits in tar and zip"
    (flet ((path-with-depth (depth)
             (format nil "~{~A~^/~}" (make-list depth :initial-element "a")))
           (tar-name (name)
             (read-tar-entries (write-tar (list (make-archive-member :name name :data (octets))))))
           (zip-name (name)
             (read-zip-entries (write-zip (list (make-archive-member :name name :data (octets)))))))
      (let ((exact (path-with-depth +archive-max-depth+))
            (over (path-with-depth (1+ +archive-max-depth+))))
        (expect (length (tar-name exact)) :to-be 1)
        (expect (length (zip-name exact)) :to-be 1)
        (signals archive-limit-exceeded (tar-name over))
        (signals archive-limit-exceeded (zip-name over)))))

  (it "bounds PAX and ZIP symlink targets and PAX hardlink targets"
    (flet ((path-with-lengths (lengths)
             (format nil "~{~A~^/~}"
                     (mapcar (lambda (length) (make-string length :initial-element #\a))
                             lengths)))
           (path-with-depth (depth)
             (format nil "~{~A~^/~}" (make-list depth :initial-element "a")))
           (tar-symlink (target)
             (read-tar-entries
              (write-tar (list (make-archive-member :name "link" :kind :symlink :link-target target)))))
           (zip-symlink (target)
             (read-zip-entries
              (write-zip (list (make-archive-member :name "link" :kind :symlink :link-target target)))))
           (tar-hardlink (target)
             (read-tar-entries
              (tar-of (tar-block "PaxHeader" #\x
                                 :data (aitools.text.domain::%pax-record "linkpath" target))
                      (tar-block "hard" #\1 :link "short"))))
           (entries-with-link (entries)
             (archive-entry-link-target (first entries))))
      (let ((exact (path-with-lengths (cons 239 (make-list 16 :initial-element 240))))
            (over (path-with-lengths (make-list 17 :initial-element 240)))
            (component-exact (make-string +archive-max-component-bytes+ :initial-element #\a))
            (component-over (make-string (1+ +archive-max-component-bytes+) :initial-element #\a))
            (depth-exact (path-with-depth +archive-max-depth+))
            (depth-over (path-with-depth (1+ +archive-max-depth+))))
        (expect (entries-with-link (tar-symlink exact)) :to-equal exact)
        (expect (entries-with-link (zip-symlink exact)) :to-equal exact)
        (expect (entries-with-link (tar-hardlink exact)) :to-equal exact)
        (expect (entries-with-link (tar-hardlink component-exact)) :to-equal component-exact)
        (expect (entries-with-link (tar-hardlink depth-exact)) :to-equal depth-exact)
        (signals archive-limit-exceeded (tar-symlink over))
        (signals archive-limit-exceeded (zip-symlink over))
        (signals archive-limit-exceeded (tar-hardlink over))
        (signals archive-limit-exceeded (tar-hardlink component-over))
        (signals archive-limit-exceeded (tar-hardlink depth-over)))))

  (it "checks raw PAX, GNU long-name, ZIP-name, and ZIP symlink bytes before decoding"
    (flet ((invalid-pax-record (key)
             (let* ((record (aitools.text.domain::%pax-record key
                                                              (make-string 256 :initial-element #\a)))
                    (value-start (+ 1 (position 61 record))))
               (fill record #xFF :start value-start :end (1- (length record)))
               record)))
      (let* ((pax-path (invalid-pax-record "path"))
             (pax-link (invalid-pax-record "linkpath"))
             (gnu-long-name (make-array 4096 :element-type '(unsigned-byte 8) :initial-element #xFF))
             (gnu-long-link (make-array 4096 :element-type '(unsigned-byte 8) :initial-element #xFF)))
        (signals archive-limit-exceeded
          (read-tar-entries
           (tar-of (tar-block "PaxHeader" #\x :data pax-path)
                   (tar-block "file" #\0))))
        (signals archive-limit-exceeded
          (read-tar-entries
           (tar-of (tar-block "PaxHeader" #\x :data pax-link)
                   (tar-block "hard" #\1 :link "short"))))
        (signals archive-limit-exceeded
          (read-tar-entries (tar-of (tar-block "LongLink" #\L :data gnu-long-name)
                                    (tar-block "file" #\0))))
        (signals archive-limit-exceeded
          (read-tar-entries (tar-of (tar-block "LongLink" #\K :data gnu-long-link)
                                    (tar-block "link" #\2)))))
      (let* ((name (make-string 256 :initial-element #\a))
             (archive (write-zip (list (make-archive-member :name name :data (octets)))))
             (eocd (- (length archive) 22))
             (directory-offset (logior (aref archive (+ eocd 16))
                                       (ash (aref archive (+ eocd 17)) 8)))
             (name-start (+ directory-offset 46)))
        (put-u16 archive (+ directory-offset 8) #x0800)
        (fill archive #xFF :start name-start :end (+ name-start 256))
        (signals archive-limit-exceeded (read-zip-entries archive)))
      (let* ((raw-target (make-array 256 :element-type '(unsigned-byte 8) :initial-element #xFF))
             (archive (write-zip
                       (list (make-archive-member :name "link" :kind :symlink
                                                  :link-target (make-string 256 :initial-element #\a)))))
             (eocd (- (length archive) 22))
             (directory-offset (logior (aref archive (+ eocd 16))
                                       (ash (aref archive (+ eocd 17)) 8)))
             (data-offset (+ 30 (aitools.text.domain::%u16le archive 26)
                             (aitools.text.domain::%u16le archive 28)))
             (checksum (aitools.text.domain:crc32 raw-target)))
        (fill archive #xFF :start data-offset :end (+ data-offset 256))
        (put-u32 archive 14 checksum)
        (put-u32 archive (+ directory-offset 16) checksum)
        (signals archive-limit-exceeded (read-zip-entries archive)))))

  (it "counts every nonzero tar header and rejects header 200001 before checksum parsing"
    (let* ((limit +archive-max-tar-records+)
           (header (aitools.text.domain::%tar-header (string-bytes "global") (octets) (octets)
                                                     0 #o644 0 #\g))
           (archive (make-array (+ (* (1+ limit) 512) 1024)
                                :element-type '(unsigned-byte 8) :initial-element 0))
           (over-offset (* limit 512)))
      (loop for offset from 0 below (* (1+ limit) 512) by 512
            do (replace archive header :start1 offset))
      (fill archive 0 :start over-offset :end (+ over-offset 512))
      (expect (read-tar-entries archive) :to-equal '())
      (replace archive header :start1 over-offset)
      (setf (aref archive over-offset) 65)
      (signals archive-limit-exceeded (read-tar-entries archive))))

  (it "accepts exactly 65534 ZIP entries and rejects either 65535 EOCD count"
    (let* ((member (make-archive-member :name "x" :data (octets)))
           (archive (write-zip (make-list +archive-max-zip-records+ :initial-element member)))
           (eocd (- (length archive) 22)))
      (expect (length (read-zip-entries archive)) :to-be +archive-max-zip-records+)
      (put-u16 archive (+ eocd 8) #xFFFF)
      (signals archive-limit-exceeded (read-zip-entries archive))
      (put-u16 archive (+ eocd 8) +archive-max-zip-records+)
      (put-u16 archive (+ eocd 10) #xFFFF)
      (signals archive-limit-exceeded (read-zip-entries archive))))
  (it "accepts raw gzip FNAME limits and checks optional fields on overlong names"
    (flet ((gzip-header (name flags &optional (extra (octets)) (comment (octets)) crc)
             (let* ((header (join-octets (octets #x1F #x8B 8 flags 0 0 0 0 0 3) extra
                                         (join-octets name (octets 0)) comment))
                    (checksum (logand (aitools.text.domain:crc32 header) #xFFFF)))
               (if (logtest flags 2)
                   (join-octets header (or crc (octets (logand checksum #xFF) (ash checksum -8))))
                   header))))
      (let ((over (make-array (1+ +archive-max-path-bytes+)
                              :element-type '(unsigned-byte 8) :initial-element 0)))
        (loop for index from 0 below (length over) by 2
              do (setf (aref over index) #xC3 (aref over (1+ index)) #xA9))
        (let ((exact (subseq over 0 +archive-max-path-bytes+)))
          (setf (aref exact (1- +archive-max-path-bytes+)) 65)
          (expect (length (nth-value 0 (gzip-member-header (gzip-header exact 8)))) :to-be 2048)
          (signals archive-limit-exceeded (gzip-member-header (gzip-header over 8)))
          (expect (multiple-value-list
                   (gzip-member-header (gzip-header (octets 65) #x1E (octets 2 0 9 9) (octets 67 0))))
                  :to-equal (list "A" 0 20))
          (signals archive-limit-exceeded
            (gzip-member-header (gzip-header over #x1E (octets 2 0 9 9) (octets 67 0))))
          (expect (gzip-reason (lambda () (gzip-member-header
                                           (gzip-header over #x1C (octets #xFF #xFF 9 9) (octets 67 0)))))
                  :to-equal (list 'archive-error "field is truncated"))
          (expect (gzip-reason (lambda () (gzip-member-header (gzip-header over #x18 (octets) (octets 67)))))
                  :to-equal (list 'archive-error "unterminated gzip header string"))
          (expect (gzip-reason (lambda () (gzip-member-header (gzip-header over #x0A (octets) (octets) (octets 0)))))
                  :to-equal (list 'archive-error "gzip header is truncated"))
          (let ((bad-crc (gzip-header over #x0A)))
            (setf (aref bad-crc (1- (length bad-crc)))
                  (logxor (aref bad-crc (1- (length bad-crc))) 1))
            (expect (gzip-reason (lambda () (gzip-member-header bad-crc)))
                    :to-equal (list 'archive-error "gzip header CRC-32 mismatch"))))))))

(describe "aitools.text.domain archive-entry constructor guards"
  (it "rejects a negative archive-entry size"
    (let ((invalid-size (1- (parse-integer "0"))))
      (signals type-error
        (aitools.text.domain::make-archive-entry :size invalid-size)))))
