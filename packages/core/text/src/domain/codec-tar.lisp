;;;;
;;;; tar archives. Reading accepts POSIX ustar (with the 155-byte prefix),
;;;; pax extended headers (`x` per entry, `g` global: path, linkpath, size,
;;;; mtime), GNU long names and link names (`L`, `K`), GNU base-256 numbers,
;;;; and pre-POSIX v7 headers. Writing produces ustar; a name or link target
;;;; that does not fit the ustar fields or is not ASCII, and a size or mtime
;;;; beyond 11 octal digits, is carried in a pax `x` header (POSIX.1-2001),
;;;; which GNU tar, bsdtar, and Python's tarfile all read. Output ends with
;;;; two zero blocks and is padded to 10240-byte records like GNU tar's.
(in-package #:aitools.text.domain)

(defconstant +tar-block+ 512)
(defconstant +tar-record+ 10240)
(defconstant +tar-max-pax-mtime+ (1- (* 127 (expt 256 11))))

(defun %tar-number (octets offset length)
  "A tar numeric field: octal digits ended by space or NUL, or GNU base-256
when the first byte has its high bit set."
  (let ((first (aref octets offset)))
    (cond
      ((= first #xFF) (%archive-fail "negative base-256 tar number"))
      ((logbitp 7 first)
       (loop with value = (logand first #x7F)
             for i from (1+ offset) below (+ offset length)
             do (setf value (+ (* value 256) (aref octets i)))
             finally (return value)))
      (t
       (let ((value 0) (seen nil))
         (loop for i from offset below (+ offset length)
               for byte = (aref octets i)
               do (cond ((<= 48 byte 55) (setf value (+ (* value 8) (- byte 48)) seen t))
                        ((and (= byte 32) (not seen)))
                        ((or (= byte 0) (= byte 32)) (return))
                        (t (%archive-fail "invalid octal digit in tar header"))))
         value)))))

(defun %tar-string (octets offset length &optional link-target-p)
  (let ((end (or (position 0 octets :start offset :end (+ offset length))
                 (+ offset length))))
    (when (and link-target-p
               (> (- end offset) +archive-max-link-target-bytes+))
      (error 'archive-limit-exceeded
             :limit +archive-max-link-target-bytes+
             :reason "tar link target"))
    (let ((problem (%archive-path-octets-problem octets offset end)))
      (when problem
        (error 'archive-limit-exceeded
               :limit (ecase problem
                        (:path-too-long +archive-max-path-bytes+)
                        (:component-too-long +archive-max-component-bytes+)
                        (:too-deep +archive-max-depth+))
               :reason (if link-target-p "tar link target" "tar path"))))
    (%name-from-octets octets offset end)))

(defun %tar-checksum-ok-p (octets offset)
  (let ((stored (%tar-number octets (+ offset 148) 8))
        (unsigned 0) (signed 0))
    (loop for i from offset below (+ offset +tar-block+)
          for byte = (if (<= (+ offset 148) i (+ offset 155)) 32 (aref octets i))
          do (incf unsigned byte)
             (incf signed (if (>= byte 128) (- byte 256) byte)))
    (or (= stored unsigned) (= stored signed))))

(defun %parse-pax-records (octets start end)
  "An alist of recognized pax records from OCTETS[START,END)."
  (let ((position start)
        (records '())
        (maximum-mtime +tar-max-pax-mtime+))
    (labels ((ascii-key-p (key-start key-end name)
                          (and (= (- key-end key-start) (length name))
                               (loop for index from key-start below key-end
                                     for name-index from 0
                                     always (= (aref octets index)
                                               (char-code (char name name-index))))))
             (pax-key (key-start key-end)
                      (cond ((ascii-key-p key-start key-end "path") "path")
                            ((ascii-key-p key-start key-end "linkpath") "linkpath")
                            ((ascii-key-p key-start key-end "size") "size")
                            ((ascii-key-p key-start key-end "mtime") "mtime")))
             (pax-number (value-start value-end maximum fractional-p reason)
                         (let* ((dot (and fractional-p
                                          (position 46 octets :start value-start :end value-end)))
                                (integer-end (or dot value-end))
                                (value 0))
                           (when (= integer-end value-start)
                             (%archive-fail "pax numeric value is not decimal"))
                           (loop for index from value-start below integer-end
                                 for byte = (aref octets index)
                                 do (unless (<= 48 byte 57)
                                      (%archive-fail "pax numeric value is not decimal"))
                                 (let ((digit (- byte 48)))
                                   (when (> value (floor (- maximum digit) 10))
                                     (error 'archive-limit-exceeded
                                            :limit maximum
                                            :reason reason))
                                   (setf value (+ (* value 10) digit))))
                           (when dot
                             (loop for index from (1+ dot) below value-end
                                   for byte = (aref octets index)
                                   unless (<= 48 byte 57)
                                   do (%archive-fail "pax numeric value is not decimal")))
                           value)))
      (loop while (< position end)
            do (when (zerop (aref octets position)) (return))
            (let* ((space (or (position 32 octets :start position :end end)
                              (%archive-fail "pax record has no length")))
                   (length
                    (let ((available (- end position))
                          (value 0)
                          (too-large nil))
                      (loop for index from position below space
                            for byte = (aref octets index)
                            do (unless (<= 48 byte 57)
                                 (%archive-fail "pax record length is not decimal"))
                            (unless too-large
                              (let ((digit (- byte 48)))
                                (if (> value (floor (- available digit) 10))
                                    (setf too-large t)
                                  (setf value (+ (* value 10) digit))))))
                      (when too-large (%archive-fail "malformed pax record"))
                      value))
                   (record-end (+ position length)))
              (when (or (<= length (- space position)) (> record-end end)
                        (/= (aref octets (1- record-end)) 10))
                (%archive-fail "malformed pax record"))
              (let* ((equals (or (position 61 octets :start (1+ space) :end record-end)
                                 (%archive-fail "pax record has no key")))
                     (key-start (1+ space))
                     (value-start (1+ equals))
                     (value-end (1- record-end))
                     (key (pax-key key-start equals))
                     (duplicate-p (and key (assoc key records :test #'string=)))
                     (value
                      (when (and key
                                 (or (not duplicate-p)
                                     (member key '("path" "linkpath") :test #'string=)))
                        (cond
                         ((member key '("path" "linkpath") :test #'string=)
                          (let ((link-target-p (string= key "linkpath")))
                            (when (and link-target-p
                                       (> (- value-end value-start)
                                          +archive-max-link-target-bytes+))
                              (error 'archive-limit-exceeded
                                     :limit +archive-max-link-target-bytes+
                                     :reason "tar link target"))
                            (let ((problem (%archive-path-octets-problem
                                            octets value-start value-end)))
                              (when problem
                                (error 'archive-limit-exceeded
                                       :limit (ecase problem
                                                (:path-too-long +archive-max-path-bytes+)
                                                (:component-too-long +archive-max-component-bytes+)
                                                (:too-deep +archive-max-depth+))
                                       :reason (if link-target-p
                                                   "tar link target"
                                                 "tar path"))))
                            (decode-utf8 octets :start value-start :end value-end)))
                         ((string= key "size")
                          (pax-number value-start value-end (- (length octets) end)
                                      nil "tar pax size"))
                         ((string= key "mtime")
                          (pax-number value-start value-end maximum-mtime
                                      t "tar pax mtime")))))) (when (and key (not duplicate-p))
                       (push (cons key value) records))
                (setf position record-end)))) (nreverse records)
      )))

(defun %pax-integer (records key) "The integer part of recognized PAX numeric KEY." (let ((value (cdr (assoc key records :test (function string=))))) (and (integerp value) value)))

(defun read-tar-entries (octets)
  "The ARCHIVE-ENTRY list of the tar archive OCTETS, ending at the first
zero block or the end of the data. Metadata-only headers (pax, GNU long
names) are consumed, not listed. Directory names lose their trailing '/'."
  (declare (type octets octets))
  (let ((position 0) (entries '()) (global '()) (pax '())
        (long-name nil) (long-link nil) (path-metadata-bytes 0) (record-count 0))
    (loop
     (when (> (+ position +tar-block+) (length octets))
       (when (< position (length octets)) (%archive-fail "tar archive ends inside a header"))
       (return))
     (when (loop for index from position below (+ position +tar-block+)
                 always (zerop (aref octets index)))
       (return))
     (incf record-count)
     (when (> record-count +archive-max-tar-records+)
       (error 'archive-limit-exceeded
              :limit +archive-max-tar-records+
              :reason "tar records"))
     (unless (%tar-checksum-ok-p octets position) (%archive-fail "tar header checksum mismatch"))
     (let* ((type (code-char (aref octets (+ position 156))))
            (records (append pax global))
            (size (or (and (not (member type '(#\x #\g))) (%pax-integer records "size"))
                      (%tar-number octets (+ position 124) 12)))
            (data-start (+ position +tar-block+))
            (next (+ data-start (* +tar-block+ (ceiling size +tar-block+)))))
       (when (> (+ data-start size) (length octets)) (%archive-fail "tar entry data is truncated"))
       (case type
         (#\x (setf pax (%parse-pax-records octets data-start (+ data-start size))))
         (#\g (setf global (let ((new-records (%parse-pax-records octets data-start (+ data-start size)))) (append new-records (remove-if (lambda (record) (assoc (car record) new-records :test (function string=))) global)))))
         (#\L (setf long-name (%tar-string octets data-start size)))
         (#\K (setf long-link (%tar-string octets data-start size t)))
         (t
          (let* ((ustar (and (= (aref octets (+ position 257)) 117)
                             (string= "ustar" (map 'string #'code-char (subseq octets (+ position 257) (+ position 262))))
                             (= (aref octets (+ position 262)) 0)))
                 (field-name (%tar-string octets position 100))
                 (prefix (and ustar (%tar-string octets (+ position 345) 155)))
                 (name (or (cdr (assoc "path" records :test #'string=))
                           long-name
                           (if (and prefix (plusp (length prefix)))
                               (concatenate 'string prefix "/" field-name)
                             field-name)))
                 (link (or (cdr (assoc "linkpath" records :test #'string=))
                           long-link
                           (%tar-string octets (+ position 157) 100 t)))
                 (kind (case type
                         ((#\0 #\7 #\Nul) (if (and (plusp (length name))
                                                   (char= (char name (1- (length name))) #\/))
                                              :directory :file))
                         (#\1 :hardlink)
                         (#\2 :symlink)
                         (#\5 :directory)
                         (t :other))))
            (progn
              (setf path-metadata-bytes
                    (%archive-path-metadata-total
                     path-metadata-bytes
                     (+ (%archive-utf8-byte-length (string-right-trim "/" name))
                        (if (member kind '(:symlink :hardlink))
                            (%archive-utf8-byte-length link)
                          0))))
              (push (make-archive-entry :format :tar
                                        :name (string-right-trim "/" name)
                                        :kind kind
                                        :size size
                                        :mode (logand (%tar-number octets (+ position 100) 8) #o7777)
                                        :mtime (or (%pax-integer records "mtime")
                                                   (%tar-number octets (+ position 136) 12))
                                        :link-target (and (member kind '(:symlink :hardlink)) link)
                                        :data-offset data-start
                                        :compressed-size size)
                    entries))
            (setf pax '() long-name nil long-link nil))))
       (setf position next)))
    (nreverse entries)))
