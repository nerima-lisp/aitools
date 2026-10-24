;;;; t/unit/text/archive-zip-test.lisp
(in-package #:aitools.text.test)

(describe "aitools.text.domain zip on crafted archives"
          (it-each
           (("a second disk" :deflated (:disk 1) (archive-unsupported "multi-disk zip"))
            ("a central directory on a second disk" :deflated (:directory-disk 1) (archive-unsupported "multi-disk zip"))
            ("a split central-directory count" :deflated (:directory-count 0) (archive-unsupported "multi-disk zip"))
            ("an entry starting on a second disk" :deflated (:entry-disk 1) (archive-unsupported "multi-disk zip"))
            ("a ZIP64 central-directory count" :deflated (:directory-count #xFFFF) (archive-unsupported "ZIP64"))
            ("a ZIP64 entry start disk" :deflated (:entry-disk #xFFFF) (archive-unsupported "ZIP64"))
            ("a central directory past its end record" :deflated
                                                       (:directory-size 5000)
                                                       (archive-error "central directory lies outside the archive"))
            ("a directory too short for its entry" :deflated
                                                   (:directory-size 46)
                                                   (archive-error "central directory entry exceeds declared size"))
            ("more entries than the directory holds" :deflated
                                                     (:directory-count 2 :count 2)
                                                     (archive-error "central directory entry exceeds declared size"))
            ("a name longer than the archive" :deflated
                                              (:name-length 60000)
                                              (archive-error "central directory entry is truncated"))
            ("a ZIP64 compressed size" :deflated (:compressed #xFFFFFFFF) (archive-unsupported "ZIP64"))
            ("a local header offset off its signature" :deflated
                                                       (:local 1)
                                                       (archive-error "bad local header signature"))
            ("data past the end" :deflated (:compressed 5000) (archive-error "entry data lies outside the archive"))
            ("an encrypted entry" :deflated (:flags 1) (archive-unsupported "encrypted zip entry"))
            ("an unknown method" :deflated (:method 12) (archive-unsupported "zip compression method"))
            ("a deflated entry larger than declared" :deflated
                                                     (:size 5)
                                                     (archive-error "entry inflates past its declared size"))
            ("a deflated entry smaller than declared" :deflated (:size 50) (archive-error "entry size mismatch"))
            ("a stored entry whose sizes disagree" :stored (:size 4) (archive-error "stored entry sizes disagree")))
           "rejects ~A"
           (description member patches expected)
           (declare (ignore description))
           (expect
            (zip-entries-reason
             (apply #'one-entry-zip
                    (ecase member
                      (:deflated (compressible-member))
                      (:stored (make-archive-member :name "s" :data (string-bytes "xyz"))))
                    patches))
            :to-equal
            expected))
          (it "derives kind and mode from the creator's attributes"
              (flet ((kind-and-mode (made-by external name)
                       (let ((entry
                              (first
                               (read-zip-entries
                                (one-entry-zip (make-archive-member :name name :data (string-bytes "xyz"))
                                               :made-by
                                               made-by
                                               :external
                                               external)))))
                         (list (archive-entry-name entry) (archive-entry-kind entry) (archive-entry-mode entry)))))
                (expect (kind-and-mode #x0014 0 "f") :to-equal '("f" :file nil))
                (expect (kind-and-mode #x0014 #x10 "d") :to-equal '("d" :directory nil))
                (expect (kind-and-mode #x0314 0 "r/") :to-equal '("r" :directory nil))
                (expect (kind-and-mode #x0314 (ash #o010644 16) "p") :to-equal '("p" :other #o644))
                (expect (kind-and-mode #x0314 (ash #o010755 16) "q/") :to-equal '("q" :directory #o755))))
          (it "accepts the maximum UTF-8 filename and rejects the next byte"
              (expect (read-zip-entries (write-zip '())) :to-be nil)
              (let* ((name (concatenate 'string "é" (make-string 65533 :initial-element #\a)))
                     (encoded (encode-utf8 name))
                     (entry
                      (first
                       (read-zip-entries (write-zip (list (make-archive-member :name name :data (string-bytes "x"))))))))
                (expect (length encoded) :to-be 65535)
                (expect (archive-entry-name entry) :to-equal name))
              (let* ((name (concatenate 'string "é" (make-string 65534 :initial-element #\a)))
                     (encoded (encode-utf8 name))
                     (reason
                      (archive-reason
                       (lambda ()
                         (write-zip (list (make-archive-member :name name :data (string-bytes "x"))))))))
                (expect (length encoded) :to-be 65536)
                (expect reason :to-equal '(archive-unsupported "zip filename exceeds 65535 bytes")))
              (flet ((empty-name-kind (made-by external)
                       (let ((entry
                              (first
                               (read-zip-entries
                                (one-entry-zip (make-archive-member :name "q" :data (string-bytes "xyz"))
                                               :name-length
                                               0
                                               :made-by
                                               made-by
                                               :external
                                               external
                                               :directory-size
                                               46)))))
                         (list (archive-entry-name entry) (archive-entry-kind entry)))))
                (expect (empty-name-kind #x0314 (ash #o010644 16)) :to-equal '("" :other))
                (expect (empty-name-kind #x0014 0) :to-equal '("" :file))))
          (it "clamps a time past 2107 to MS-DOS's last representable second"
              (expect
               (archive-entry-mtime
                (first
                 (read-zip-entries
                  (write-zip (list (make-archive-member :name "late" :data (string-bytes "x") :mtime 5000000000))))))
               :to-be
               (aitools.kernel.domain:universal-time-to-unix-seconds (encode-universal-time 58 59 23 31 12 2107 0))))
          (it "rejects the ZIP64 entry-count sentinel before processing members"
              (dolist (count '(65535 65536))
                (expect
                 (archive-reason
                  (lambda ()
                    (write-zip (make-list count :initial-element nil))))
                 :to-equal
                 '(archive-unsupported "65535 or more zip entries")))))

(describe "aitools.text.domain zip"
  (it "round-trips names, kinds, modes, symlinks, and bytes"
    (let* ((members (sample-members))
           (archive (write-zip members))
           (entries (read-zip-entries archive)))
      (expect (entry-summary entries archive) :to-equal (member-summary members))
      (expect (mapcar #'archive-entry-format entries) :to-equal (make-list (length members) :initial-element :zip))))

  (it "stores MS-DOS times at two-second resolution as UTC"
    (let ((entries (read-zip-entries (write-zip (sample-members)))))
      (expect (archive-entry-mtime (second entries)) :to-be 1700000000)
      (expect (archive-entry-size (third entries)) :to-be 3000)))

  (it "deflates compressible data and stores incompressible data"
    (let* ((archive (write-zip (sample-members))) (entries (read-zip-entries archive)))
      (expect (aitools.text.domain::archive-entry-method (third entries)) :to-be 8)
      (expect (aitools.text.domain::archive-entry-method (fourth entries)) :to-be 0)))

  (it "detects corrupted entry data by CRC-32"
    (let* ((archive (write-zip (list (make-archive-member :name "a" :data (string-bytes "abcdefgh")))))
           (entry (first (read-zip-entries archive))))
      (setf (aref archive (+ 3 (aitools.text.domain::archive-entry-data-offset entry))) 0)
      (signals archive-error (archive-entry-data archive entry))))
  (it "accepts central-directory signatures and rejects invalid boundaries"
    (let* ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 65))
           (archive (write-zip (list (make-archive-member :name "a" :data data))))
           (entry (first (read-zip-entries archive)))
           (compressed (aitools.text.domain::archive-entry-compressed-size entry))
           (data-end (+ (aitools.text.domain::archive-entry-data-offset entry) compressed))
           (eocd (- (length archive) 22))
           (directory-offset (logior (aref archive (+ eocd 16))
                                     (ash (aref archive (+ eocd 17)) 8)))
           (local-offset (search #(#x50 #x4B #x03 #x04)
                                 archive :end2 data-end :from-end t))
           (with-junk (concatenate '(vector (unsigned-byte 8))
                                   (subseq archive 0 data-end)
                                   (make-array 1 :element-type '(unsigned-byte 8)
                                               :initial-element #xA5)
                                   (subseq archive data-end))))
      (expect (aitools.text.domain::archive-entry-method entry) :to-be 8)
      (put-u32 with-junk (+ local-offset 18) (1+ compressed))
      (put-u32 with-junk (+ directory-offset 21) (1+ compressed))
      (put-u32 with-junk (+ eocd 17) (1+ directory-offset))
      (expect (zip-entries-reason with-junk)
              :to-equal '(archive-error "deflated entry has trailing data")))
    (let* ((archive (write-zip
                     (list (make-archive-member :name "a" :data (string-bytes "x")))))
           (eocd (- (length archive) 22))
           (directory-offset (logior (aref archive (+ eocd 16))
                                     (ash (aref archive (+ eocd 17)) 8)))
           (directory-size (- eocd directory-offset))
           (with-padding (concatenate '(vector (unsigned-byte 8))
                                      (subseq archive 0 eocd)
                                      #(0)
                                      (subseq archive eocd))))
      (put-u32 with-padding (+ eocd 13) (1+ directory-size))
      (expect (zip-entries-reason with-padding)
              :to-equal '(archive-error "parsed entries do not consume declared directory size")))
    (let* ((archive (write-zip
                     (list (make-archive-member :name "a" :data (string-bytes "x")))))
           (eocd (- (length archive) 22))
           (directory-offset (logior (aref archive (+ eocd 16))
                                     (ash (aref archive (+ eocd 17)) 8)))
           (directory-size (- eocd directory-offset))
           (directory-end (+ directory-offset directory-size))
           (signature (octets #x50 #x4B #x05 #x05 3 0 1 2 3))
           (signed (concatenate '(vector (unsigned-byte 8))
                                (subseq archive 0 directory-end)
                                signature
                                (subseq archive directory-end)))
           (signed-eocd (+ eocd (length signature))))
      (put-u32 signed (+ signed-eocd 12) (+ directory-size (length signature)))
      (let ((entry (first (read-zip-entries signed))))
        (expect (archive-entry-name entry) :to-equal "a")
        (expect (archive-entry-data signed entry) :to-equalp (string-bytes "x"))
        (let ((invalid-signature (copy-seq signed)))
          (put-u16 invalid-signature (+ directory-end 4) 2)
          (expect (zip-entries-reason invalid-signature)
                  :to-equal '(archive-error "parsed entries do not consume declared directory size")))))
    (let* ((archive (write-zip
                     (list (make-archive-member :name "a" :data (string-bytes "payload")))))
           (eocd (- (length archive) 22))
           (directory-offset (logior (aref archive (+ eocd 16))
                                     (ash (aref archive (+ eocd 17)) 8)))
           (local-offset (- directory-offset 30)))
      (fill archive 0 :start local-offset :end directory-offset)
      (put-u32 archive local-offset #x04034b50)
      (put-u32 archive (+ directory-offset 42) local-offset)
      (progn
        (expect (zip-entries-reason archive)
                :to-equal (list (quote archive-error) "entry data overlaps central directory"))
        (let* ((short-header (copy-seq archive))
               (short-local (- directory-offset 29)))
          (fill short-header 0 :start short-local :end directory-offset)
          (put-u32 short-header short-local #x04034b50)
          (put-u32 short-header (+ directory-offset 42) short-local)
          (expect (zip-entries-reason short-header)
                  :to-equal (list (quote archive-error) "local entry header overlaps central directory"))))))

  (it "refuses to inflate past MAX-OUTPUT"
    (let* ((archive (write-zip (list (make-archive-member :name "z" :data (make-array 100000 :element-type '(unsigned-byte 8) :initial-element 0)))))
           (entry (first (read-zip-entries archive))))
      (signals archive-limit-exceeded (archive-entry-data archive entry :max-output 1000))))

  (it "reports ZIP64 and garbage as typed archive errors"
    (let ((archive (write-zip (list (make-archive-member :name "a" :data (string-bytes "x"))))))
      (let ((zip64 (copy-seq archive)))
        ;; the entry count field of the end-of-central-directory record
        (setf (aref zip64 (- (length zip64) 12)) #xFF (aref zip64 (- (length zip64) 11)) #xFF)
        (signals archive-unsupported (read-zip-entries zip64))))
    (signals archive-error (read-zip-entries (string-bytes "not a zip archive at all, no"))))

  (it "rejects every truncation of a valid archive"
    (let ((archive (write-zip (subseq (sample-members) 0 3))))
      (expect (loop for end from 0 below (length archive)
                    for prefix = (subseq archive 0 end)
                    always
                      (eq (handler-case
                              (progn
                                (dolist (entry (read-zip-entries prefix))
                                  (archive-entry-data prefix entry))
                                :returned)
                            (archive-error () :archive-error)
                            (error () :other))
                          :archive-error))
              :to-be-truthy))))

