;;;; t/unit/text/archive-test.lisp
;;;;
;;;; zip and tar round trips through our own writer and reader, the
;;;; zip-slip path checks `archive extract` relies on, and robustness of both readers
;;;; against truncated or corrupted input. Interoperability with Info-ZIP,
;;;; GNU tar, and bsdtar is the integration test's job.
(in-package #:aitools.text.test)

(defun sample-members ()
  (list (make-archive-member :name "dir" :kind :directory :mode #o755 :mtime 1700000000)
        (make-archive-member :name "dir/hello.txt" :data (string-bytes (format nil "hello~%")) :mtime 1700000000)
        (make-archive-member :name "dir/big.bin" :data (dynamic-vector-input) :mode #o600 :mtime 1700000002)
        (make-archive-member :name "dir/noise.bin" :data (pseudo-random-octets 5000 7) :mtime 1700000004)
        (make-archive-member :name "dir/empty" :data (octets) :mtime 1700000000)
        (make-archive-member :name "日本語/ファイル.txt" :data (string-bytes "unicode") :mtime 1700000000)
        (make-archive-member :name "dir/link" :kind :symlink :link-target "hello.txt" :mode #o777
                             :mtime 1700000000)
        (make-archive-member :name (format nil "~{~A~^/~}" (loop for i from 0 below 30 collect (format nil "segment~2,'0D" i)))
                             :data (string-bytes "deep") :mtime 1700000000)))

(defun entry-summary (entries archive)
  (mapcar (lambda (entry)
            (list (archive-entry-name entry) (archive-entry-kind entry) (archive-entry-mode entry)
                  (archive-entry-link-target entry)
                  (and (eq (archive-entry-kind entry) :file) (coerce (archive-entry-data archive entry) 'list))))
          entries))

(defun member-summary (members &key mode-of-symlink)
  (mapcar (lambda (member)
            (list (archive-member-name member) (archive-member-kind member)
                  (if (and mode-of-symlink (eq (archive-member-kind member) :symlink))
                      mode-of-symlink
                      (archive-member-mode member))
                  (archive-member-link-target member)
                  (and (eq (archive-member-kind member) :file) (coerce (archive-member-data member) 'list))))
          members))

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

  (it "survives every truncation of a valid archive"
    (let ((archive (write-zip (subseq (sample-members) 0 3))))
      (expect (loop for end from 0 below (length archive) by 7
                    always (only-archive-errors-p
                            (lambda (bytes) (dolist (entry (read-zip-entries bytes))
                                              (archive-entry-data bytes entry)))
                            (subseq archive 0 end)))
              :to-be-truthy))))

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

(describe "aitools.text.domain archive paths (zip slip)"
  (it "flags absolute, traversing, backslashed, NUL, and empty names"
    (expect (archive-entry-path-problem "a/b.txt") :to-be-falsy)
    (expect (archive-entry-path-problem "a/..b/c") :to-be-falsy)
    (expect (archive-entry-path-problem "/etc/passwd") :to-be :absolute)
    (expect (archive-entry-path-problem "C:/x") :to-be :absolute)
    (expect (archive-entry-path-problem "a/../../b") :to-be :parent-traversal)
    (expect (archive-entry-path-problem "..") :to-be :parent-traversal)
    (expect (archive-entry-path-problem "a\\..\\b") :to-be :backslash)
    (expect (archive-entry-path-problem (format nil "a~Cb" (code-char 0))) :to-be :nul)
    (expect (archive-entry-path-problem "/") :to-be :empty))

  (it "flags symlink targets that leave the extraction directory"
    (expect (archive-link-target-problem "a/b/link" "../c") :to-be-falsy)
    (expect (archive-link-target-problem "a/b/link" "../../c") :to-be-falsy)
    (expect (archive-link-target-problem "a/b/link" "../../../c") :to-be :escapes)
    (expect (archive-link-target-problem "link" "x/../../y") :to-be :escapes)
    (expect (archive-link-target-problem "link" "/etc") :to-be :absolute))

  (it "flags a target whose `..` climbs out of another symlink of the same archive"
    ;; x/d -> .. is harmless alone, but s -> x/d/.. then resolves to the
    ;; parent of the extraction root, not to x.
    (let ((symlinks (list "x/d" "s")))
      (expect (archive-link-target-problem "x/d" ".." :symlink-names symlinks) :to-be-falsy)
      (expect (archive-link-target-problem "s" "x/d/.." :symlink-names symlinks) :to-be :escapes)
      (expect (archive-link-target-problem "x/d/t" "../y" :symlink-names symlinks) :to-be :escapes)
      (expect (archive-link-target-problem "s2" "x/d/y" :symlink-names symlinks) :to-be-falsy)
      (expect (archive-link-target-problem "x/e" "../x/f" :symlink-names symlinks) :to-be-falsy))))

(defun v7-tar (member)
  "WRITE-TAR's output for the one MEMBER with the ustar magic and version
cleared and the checksum recomputed: the pre-POSIX header layout."
  (let ((archive (write-tar (list member))))
    (fill archive 0 :start 257 :end 265)
    (fill archive 32 :start 148 :end 156)
    (let ((sum (reduce #'+ archive :end 512)))
      (replace archive (map 'vector #'char-code (format nil "~6,'0O" sum)) :start1 148)
      (setf (aref archive 154) 0))
    archive))

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

(defun put-u16 (octets offset value)
  (setf (aref octets offset) (ldb (byte 8 0) value) (aref octets (1+ offset)) (ldb (byte 8 8) value))
  octets)

(defun put-u32 (octets offset value)
  (put-u16 octets offset (ldb (byte 16 0) value))
  (put-u16 octets (+ offset 2) (ldb (byte 16 16) value)))

(defun one-entry-zip (member &rest patches)
  "WRITE-ZIP of MEMBER alone, then PATCHES: (FIELD VALUE) pairs naming a
central directory field (:made-by :flags :method :compressed :size :name-length
:external :local) or an end-of-central-directory one (:disk :count
:directory-size :directory-offset)."
  (let* ((archive (write-zip (list member)))
         (eocd (- (length archive) 22))
         (entry (logior (aref archive (+ eocd 16)) (ash (aref archive (+ eocd 17)) 8))))
    (loop for (field value) on patches by #'cddr
          do (ecase field
               (:made-by (put-u16 archive (+ entry 4) value))
               (:flags (put-u16 archive (+ entry 8) value))
               (:method (put-u16 archive (+ entry 10) value))
               (:compressed (put-u32 archive (+ entry 20) value))
               (:size (put-u32 archive (+ entry 24) value))
               (:name-length (put-u16 archive (+ entry 28) value))
               (:external (put-u32 archive (+ entry 38) value))
               (:local (put-u32 archive (+ entry 42) value))
               (:disk (put-u16 archive (+ eocd 4) value))
               (:count (put-u16 archive (+ eocd 10) value))
               (:directory-size (put-u32 archive (+ eocd 12) value))
               (:directory-offset (put-u32 archive (+ eocd 16) value))))
    archive))

(defun archive-reason (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (archive-error (condition) (list (type-of condition) (archive-error-reason condition)))))

(defun zip-entries-reason (archive)
  (archive-reason (lambda () (dolist (entry (read-zip-entries archive)) (archive-entry-data archive entry)))))

(defun reseal-tar-header (header)
  "HEADER (a 512-byte tar header) with its checksum recomputed."
  (fill header 32 :start 148 :end 156)
  (replace header (map 'vector #'char-code (format nil "~6,'0O" (reduce #'+ header))) :start1 148)
  (setf (aref header 154) 0)
  header)

(defun tar-block (name type &key (data (octets)) (size (length data)) (mode #o644) (mtime 0) (link "") patch)
  "A tar header for NAME of TYPE (a character) followed by DATA padded to
whole blocks. PATCH, a function of the header, edits it before the checksum
is recomputed."
  (let ((header (aitools.text.domain::%tar-header (string-bytes name) (octets) (string-bytes link) size mode mtime type)))
    (when patch (funcall patch header))
    (join-octets (reseal-tar-header header) data
                 (make-array (mod (- (length data)) 512) :element-type '(unsigned-byte 8) :initial-element 0))))

(defun tar-of (&rest blocks)
  (apply #'join-octets (append blocks (list (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))))

(defun pax-body (&rest records)
  (apply #'join-octets (mapcar #'string-bytes records)))

(defun tar-entries-reason (archive)
  (archive-reason (lambda () (read-tar-entries archive))))

(defun compressible-member ()
  (make-archive-member :name "a" :data (string-bytes "abcabcabcabcabcabcabc")))

(describe "aitools.text.domain zip on crafted archives"
  (it-each (("a second disk" :deflated (:disk 1) (archive-unsupported "multi-disk zip"))
            ("a central directory past its end record" :deflated (:directory-size 5000)
             (archive-error "central directory lies outside the archive"))
            ("more entries than the directory holds" :deflated (:count 2)
             (archive-error "bad central directory entry signature"))
            ("a name longer than the archive" :deflated (:name-length 60000)
             (archive-error "central directory entry is truncated"))
            ("a ZIP64 compressed size" :deflated (:compressed #xFFFFFFFF) (archive-unsupported "ZIP64"))
            ("a local header offset off its signature" :deflated (:local 1) (archive-error "bad local header signature"))
            ("data past the end" :deflated (:compressed 5000) (archive-error "entry data lies outside the archive"))
            ("an encrypted entry" :deflated (:flags 1) (archive-unsupported "encrypted zip entry"))
            ("an unknown method" :deflated (:method 12) (archive-unsupported "zip compression method"))
            ("a deflated entry larger than declared" :deflated (:size 5)
             (archive-error "entry inflates past its declared size"))
            ("a deflated entry smaller than declared" :deflated (:size 50) (archive-error "entry size mismatch"))
            ("a stored entry whose sizes disagree" :stored (:size 4) (archive-error "stored entry sizes disagree")))
      "rejects ~A"
      (description member patches expected)
    (declare (ignore description))
    (expect (zip-entries-reason (apply #'one-entry-zip
                                       (ecase member
                                         (:deflated (compressible-member))
                                         (:stored (make-archive-member :name "s" :data (string-bytes "xyz"))))
                                       patches))
            :to-equal expected))

  (it "derives kind and mode from the creator's attributes"
    (flet ((kind-and-mode (made-by external name)
             (let ((entry (first (read-zip-entries
                                  (one-entry-zip (make-archive-member :name name :data (string-bytes "xyz"))
                                                 :made-by made-by :external external)))))
               (list (archive-entry-name entry) (archive-entry-kind entry) (archive-entry-mode entry)))))
      (expect (kind-and-mode #x0014 0 "f") :to-equal '("f" :file nil))
      (expect (kind-and-mode #x0014 #x10 "d") :to-equal '("d" :directory nil))
      (expect (kind-and-mode #x0314 0 "r/") :to-equal '("r" :directory nil))
      (expect (kind-and-mode #x0314 (ash #o010644 16) "p") :to-equal '("p" :other #o644))
      (expect (kind-and-mode #x0314 (ash #o010755 16) "q/") :to-equal '("q" :directory #o755))))

  (it "reads an archive of no entries and entries with an empty name"
    (expect (read-zip-entries (write-zip '())) :to-be nil)
    (flet ((empty-name-kind (made-by external)
             (let ((entry (first (read-zip-entries
                                  (one-entry-zip (make-archive-member :name "q" :data (string-bytes "xyz"))
                                                 :name-length 0 :made-by made-by :external external)))))
               (list (archive-entry-name entry) (archive-entry-kind entry)))))
      (expect (empty-name-kind #x0314 (ash #o010644 16)) :to-equal '("" :other))
      (expect (empty-name-kind #x0014 0) :to-equal '("" :file))))

  (it "clamps a time past 2107 to MS-DOS's last representable second"
    (expect (archive-entry-mtime (first (read-zip-entries
                                         (write-zip (list (make-archive-member :name "late" :data (string-bytes "x")
                                                                               :mtime 5000000000))))))
            :to-be (aitools.kernel.domain:universal-time-to-unix-seconds
                    (encode-universal-time 58 59 23 31 12 2107 0))))

  (it "refuses more entries than a non-ZIP64 directory counts"
    (expect (archive-reason (lambda ()
                              (write-zip (make-list 65536 :initial-element
                                                    (make-archive-member :name "d" :kind :directory)))))
            :to-equal '(archive-unsupported "more than 65535 zip entries"))))

(defun tar-summary (archive)
  (mapcar (lambda (entry)
            (list (archive-entry-name entry) (archive-entry-kind entry) (archive-entry-size entry)
                  (archive-entry-mtime entry) (archive-entry-link-target entry)))
          (read-tar-entries archive)))

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
