;;;; t/unit/text/archive-test.lisp
;;;;
;;;; Shared archive fixtures and construction helpers for text unit tests.
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

(defun put-u16 (octets offset value)
  (setf (aref octets offset) (ldb (byte 8 0) value) (aref octets (1+ offset)) (ldb (byte 8 8) value))
  octets)

(defun put-u32 (octets offset value)
  (put-u16 octets offset (ldb (byte 16 0) value))
  (put-u16 octets (+ offset 2) (ldb (byte 16 16) value)))

(defun one-entry-zip (member &rest patches)
  "WRITE-ZIP of MEMBER alone, then PATCHES are (FIELD VALUE) pairs naming
central directory fields (:made-by :flags :method :compressed :size :name-length
:external :local :entry-disk) or end-of-central-directory fields (:disk
:directory-disk :directory-count :count :directory-size :directory-offset)."
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
               ((:disk :directory-disk :directory-count :entry-disk)
                (ecase field
                  (:disk (put-u16 archive (+ eocd 4) value))
                  (:directory-disk (put-u16 archive (+ eocd 6) value))
                  (:directory-count (put-u16 archive (+ eocd 8) value))
                  (:entry-disk (put-u16 archive (+ entry 34) value))))
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

(defun tar-summary (archive)
  (mapcar (lambda (entry)
            (list (archive-entry-name entry) (archive-entry-kind entry) (archive-entry-size entry)
                  (archive-entry-mtime entry) (archive-entry-link-target entry)))
          (read-tar-entries archive)))

