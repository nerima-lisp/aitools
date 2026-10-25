;;;;
;;;; zip archives (PKWARE APPNOTE): the central directory is authoritative
;;;; for reading; stored and deflated entries are supported. ZIP64,
;;;; encryption, multi-disk archives, and other methods raise
;;;; ARCHIVE-UNSUPPORTED rather than being misread. Entry times are MS-DOS
;;;; local times without a zone; they are read and written as UTC so the same
;;;; archive lists the same way on every host.
(in-package #:aitools.text.domain)

(defun %dos-time-to-unix (time date)
  (let* ((year (+ 1980 (ldb (byte 7 9) date)))
         (month (max 1 (min 12 (ldb (byte 4 5) date))))
         (day (max 1 (min 31 (ldb (byte 5 0) date))))
         (hour (min 23 (ldb (byte 5 11) time)))
         (minute (min 59 (ldb (byte 6 5) time)))
         (second (* 2 (min 29 (ldb (byte 5 0) time)))))
    (cl-date-kit:local-date-time-to-epoch-second
     (cl-date-kit:local-date-time-plus-days
      (cl-date-kit:local-date-time-of year month 1 hour minute second)
      (1- day))
     (cl-date-kit:zone-offset-utc))))

(defun %zip-signature-p (octets offset signature)
  (and (<= 0 offset) (<= (+ offset 4) (length octets))
       (= (%u32le octets offset) signature)))

(defun %find-end-of-central-directory (octets)
  (let ((length (length octets)))
    (when (< length 22) (%archive-fail "too short to be a zip archive"))
    (loop for offset from (- length 22) downto (max 0 (- length 22 65535))
          when (and (%zip-signature-p octets offset #x06054b50)
                    (<= (+ offset 22 (%u16le octets (+ offset 20))) length))
            return offset
          finally (%archive-fail "no zip end-of-central-directory record"))))

(defun %zip-kind-and-mode (made-by external name)
  (let ((unix-mode (and (= (ash made-by -8) 3) (ash external -16))))
    (if (and unix-mode (plusp unix-mode))
        (values (case (logand unix-mode #o170000)
                  (#o040000 :directory)
                  (#o120000 :symlink)
                  (#o100000 :file)
                  (t (if (and (plusp (length name)) (char= (char name (1- (length name))) #\/))
                         :directory :other)))
                (logand unix-mode #o7777))
        (values (if (or (logbitp 4 external)
                        (and (plusp (length name)) (char= (char name (1- (length name))) #\/)))
                    :directory :file)
                nil))))

(defun read-zip-entries (octets)
  "The ARCHIVE-ENTRY list of the zip archive OCTETS in central directory
order. Directory names lose their trailing '/'. A symlink's target is read
from its (small) data."
  (declare (type octets octets))
  (let* ((eocd (%find-end-of-central-directory octets))
         (disk (%u16le octets (+ eocd 4)))
         (count (%u16le octets (+ eocd 10)))
         (directory-size (%u32le octets (+ eocd 12)))
         (directory-offset (%u32le octets (+ eocd 16)))
         (directory-end (+ directory-offset directory-size)))
    (when (or (%zip-signature-p octets (- eocd 20) #x07064b50)
              (or (= count #xFFFF) (= (%u16le octets (+ eocd 8)) #xFFFF))
              (= directory-size #xFFFFFFFF)
              (= directory-offset #xFFFFFFFF))
      (%archive-unsupported "ZIP64"))
    (unless (and (zerop disk)
                 (zerop (%u16le octets (+ eocd 6)))
                 (= count (%u16le octets (+ eocd 8))))
      (%archive-unsupported "multi-disk zip"))
    (when (> directory-end eocd)
      (%archive-fail "central directory lies outside the archive"))
    (let ((offset directory-offset) (entries '()))
      (dotimes (i count)
        (when (> (+ offset 46) directory-end)
          (%archive-fail "central directory entry exceeds declared size"))
        (unless (%zip-signature-p octets offset #x02014b50)
          (%archive-fail "bad central directory entry signature"))
        (let* ((made-by (%u16le octets (+ offset 4)))
               (flags (%u16le octets (+ offset 8)))
               (method (%u16le octets (+ offset 10)))
               (time (%u16le octets (+ offset 12)))
               (date (%u16le octets (+ offset 14)))
               (crc (%u32le octets (+ offset 16)))
               (compressed (%u32le octets (+ offset 20)))
               (size (%u32le octets (+ offset 24)))
               (name-length (%u16le octets (+ offset 28)))
               (extra-length (%u16le octets (+ offset 30)))
               (comment-length (%u16le octets (+ offset 32)))
               (external (%u32le octets (+ offset 38)))
               (local (%u32le octets (+ offset 42)))
               (name-start (+ offset 46))
               (next (+ name-start name-length extra-length comment-length)))
          (when (> next (length octets))
            (%archive-fail "central directory entry is truncated"))
          (when (> next directory-end)
            (%archive-fail "central directory entry exceeds declared size"))
          (cond
            ((or (= compressed #xFFFFFFFF)
                 (= size #xFFFFFFFF)
                 (= local #xFFFFFFFF)
                 (= (%u16le octets (+ offset 34)) #xFFFF))
             (%archive-unsupported "ZIP64"))
            ((/= (%u16le octets (+ offset 34)) 0)
             (%archive-unsupported "multi-disk zip")))
          (when (> (+ local 30) directory-offset)
            (%archive-fail "local entry header overlaps central directory"))
          (unless (%zip-signature-p octets local #x04034b50)
            (%archive-fail "bad local header signature"))
          (let* ((data-offset (+ local 30
                                 (%u16le octets (+ local 26))
                                 (%u16le octets (+ local 28))))
                 (data-end (+ data-offset compressed))
                 (raw-name (if (logbitp 11 flags)
                               (decode-utf8 octets :start name-start :end (+ name-start name-length))
                               (%name-from-octets octets name-start (+ name-start name-length)))))
            (when (> data-end (length octets))
              (%archive-fail "entry data lies outside the archive"))
            (when (> data-end directory-offset)
              (%archive-fail "entry data overlaps central directory"))
            (multiple-value-bind (kind mode) (%zip-kind-and-mode made-by external raw-name)
              (let ((entry (make-archive-entry
                            :format :zip
                            :name (string-right-trim "/" raw-name)
                            :kind kind
                            :size size
                            :mode mode
                            :mtime (%dos-time-to-unix time date)
                            :data-offset data-offset
                            :compressed-size compressed
                            :method method
                            :flags flags
                            :crc crc)))
                (push (if (eq kind :symlink)
                          (make-archive-entry
                           :format :zip
                           :name (archive-entry-name entry)
                           :kind kind
                           :size size
                           :mode mode
                           :mtime (archive-entry-mtime entry)
                           :link-target
                           (decode-utf8 (archive-entry-data octets entry :max-output 4096))
                           :data-offset data-offset
                           :compressed-size compressed
                           :method method
                           :flags flags
                           :crc crc)
                          entry)
                      entries))))
          (setf offset next)))
      (unless (= offset directory-end)
        (unless (and (<= (+ offset 6) directory-end)
                     (%zip-signature-p octets offset #x05054b50))
          (%archive-fail "parsed entries do not consume declared directory size"))
        (let ((signature-end (+ offset 6 (%u16le octets (+ offset 4)))))
          (unless (= signature-end directory-end)
            (%archive-fail "parsed entries do not consume declared directory size"))
          (setf offset signature-end)))
      (nreverse entries))))

(defun %zip-entry-data (octets entry max-output)
  (let ((size (archive-entry-size entry))
        (start (archive-entry-data-offset entry))
        (compressed (archive-entry-compressed-size entry)))
    (when (logbitp 0 (archive-entry-flags entry)) (%archive-unsupported "encrypted zip entry"))
    (when (and max-output (> size max-output))
      (error 'archive-limit-exceeded :limit max-output :reason "entry size"))
    (let ((data (case (archive-entry-method entry)
                  (0 (unless (= size compressed) (%archive-fail "stored entry sizes disagree"))
                   (subseq octets start (+ start size)))
                  (8 (handler-case
                           (multiple-value-bind (data consumed)
                               (inflate octets
                                        :start start
                                        :end (+ start compressed)
                                        :max-output size)
                             (unless (eql consumed (+ start compressed))
                               (%archive-fail "deflated entry has trailing data"))
                             data)
                         (archive-limit-exceeded ()
                           (%archive-fail "entry inflates past its declared size"))))
                  (t (%archive-unsupported "zip compression method")))))
      (unless (= (length data) size) (%archive-fail "entry size mismatch"))
      (unless (= (crc32 data) (archive-entry-crc entry)) (%archive-fail "zip CRC-32 mismatch"))
      data)))

;;; ------------------------------------------------------------ writing

