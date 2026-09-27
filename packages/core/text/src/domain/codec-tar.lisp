;;;; packages/core/text/src/domain/codec-tar.lisp
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

(defun %tar-string (octets offset length)
  (let ((end (or (position 0 octets :start offset :end (+ offset length)) (+ offset length))))
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
  "An alist of (KEY . VALUE) strings from pax records in OCTETS[START,END)."
  (let ((position start) (records '()))
    (loop while (< position end)
          do (when (zerop (aref octets position)) (return))
             (let* ((space (or (position 32 octets :start position :end end)
                               (%archive-fail "pax record has no length")))
                    (length (loop with value = 0
                                  for i from position below space
                                  for byte = (aref octets i)
                                  do (unless (<= 48 byte 57) (%archive-fail "pax record length is not decimal"))
                                     (setf value (+ (* value 10) (- byte 48)))
                                  finally (return value)))
                    (record-end (+ position length)))
               (when (or (<= length (- space position)) (> record-end end)
                         (/= (aref octets (1- record-end)) 10))
                 (%archive-fail "malformed pax record"))
               (let ((equals (or (position 61 octets :start (1+ space) :end record-end)
                                 (%archive-fail "pax record has no key"))))
                 (push (cons (decode-utf8 octets :start (1+ space) :end equals)
                             (decode-utf8 octets :start (1+ equals) :end (1- record-end)))
                       records))
               (setf position record-end)))
    (nreverse records)))

(defun %pax-integer (records key)
  "The integer part of pax KEY's decimal value (mtime may carry a fraction)."
  (let ((value (cdr (assoc key records :test #'string=))))
    (when value
      (let ((end (or (position #\. value) (length value))))
        (unless (and (plusp end) (every (lambda (char) (char<= #\0 char #\9)) (subseq value 0 end)))
          (%archive-fail "pax numeric value is not decimal"))
        (parse-integer value :end end)))))

(defun read-tar-entries (octets)
  "The ARCHIVE-ENTRY list of the tar archive OCTETS, ending at the first
zero block or the end of the data. Metadata-only headers (pax, GNU long
names) are consumed, not listed. Directory names lose their trailing `/`."
  (declare (type octets octets))
  (let ((position 0) (entries '()) (global '()) (pax '()) (long-name nil) (long-link nil))
    (loop
      (when (> (+ position +tar-block+) (length octets))
        (when (< position (length octets)) (%archive-fail "tar archive ends inside a header"))
        (return))
      (when (every #'zerop (subseq octets position (+ position +tar-block+)))
        (return))
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
          (#\g (setf global (append (%parse-pax-records octets data-start (+ data-start size)) global)))
          (#\L (setf long-name (%tar-string octets data-start size)))
          (#\K (setf long-link (%tar-string octets data-start size)))
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
                            (%tar-string octets (+ position 157) 100)))
                  (kind (case type
                          ((#\0 #\7 #\Nul) (if (and (plusp (length name))
                                                              (char= (char name (1- (length name))) #\/))
                                                         :directory :file))
                          (#\1 :hardlink)
                          (#\2 :symlink)
                          (#\5 :directory)
                          (t :other))))
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
                   entries)
             (setf pax '() long-name nil long-link nil))))
        (setf position next)))
    (nreverse entries)))

;;; ------------------------------------------------------------ writing

(defun %ascii-octets-p (octets)
  (every (lambda (byte) (< 0 byte 128)) octets))

(defun %split-ustar-name (octets)
  "(VALUES PREFIX NAME) byte vectors fitting ustar's 155/100-byte fields, or
NIL when OCTETS cannot be split at a `/` to fit."
  (if (<= (length octets) 100)
      (values (make-array 0 :element-type '(unsigned-byte 8)) octets)
      (loop for i from 1 below (min (length octets) 156)
            when (and (= (aref octets i) 47) (<= (- (length octets) i 1) 100) (< i (1- (length octets))))
              return (values (subseq octets 0 i) (subseq octets (1+ i))))))

(defun %pax-record (key value)
  "One pax record, `LENGTH KEY=VALUE\n`, LENGTH counting its own digits."
  (let* ((body (concatenate 'octets (encode-utf8 key) #(61) (encode-utf8 value) #(10)))
         (base (1+ (length body)))
         (total (+ base (length (princ-to-string base)))))
    (when (> (length (princ-to-string total)) (length (princ-to-string base)))
      (incf total))
    (concatenate 'octets (encode-utf8 (princ-to-string total)) #(32) body)))

(defun %octal-field (value width)
  "VALUE as WIDTH-1 zero-padded octal digits and a NUL."
  (let ((text (format nil "~v,'0O" (1- width) value)))
    (concatenate 'octets (map 'octets #'char-code text) #(0))))

(defun %tar-header (name-octets prefix-octets link-octets size mode mtime type)
  (let ((header (make-array +tar-block+ :element-type '(unsigned-byte 8) :initial-element 0)))
    (flet ((field (offset octets) (replace header octets :start1 offset)))
      (field 0 (subseq name-octets 0 (min 100 (length name-octets))))
      (field 100 (%octal-field mode 8))
      (field 108 (%octal-field 0 8))
      (field 116 (%octal-field 0 8))
      (field 124 (%octal-field size 12))
      (field 136 (%octal-field mtime 12))
      (field 148 (make-array 8 :element-type '(unsigned-byte 8) :initial-element 32))
      (setf (aref header 156) (char-code type))
      (field 157 (subseq link-octets 0 (min 100 (length link-octets))))
      (field 257 (map 'octets #'char-code "ustar"))
      (field 263 (map 'octets #'char-code "00"))
      (field 329 (%octal-field 0 8))
      (field 337 (%octal-field 0 8))
      (field 345 (subseq prefix-octets 0 (min 155 (length prefix-octets))))
      (let ((sum (reduce #'+ header)))
        (field 148 (map 'octets #'char-code (format nil "~6,'0O" sum)))
        (setf (aref header 154) 0 (aref header 155) 32)))
    header))

(defun write-tar (members)
  "A tar archive holding MEMBERS (ARCHIVE-MEMBERs) in order, with uid/gid 0
and empty owner names so the output depends only on MEMBERS."
  (let ((sink (%byte-sink))
        (octal-limit (expt 8 11)))
    (flet ((put-padded (octets)
             (%put-octets sink octets)
             (loop repeat (mod (- (length octets)) +tar-block+) do (vector-push-extend 0 sink))))
      (dolist (member members)
        (let* ((kind (archive-member-kind member))
               (name (encode-utf8 (if (eq kind :directory)
                                      (concatenate 'string (archive-member-name member) "/")
                                      (archive-member-name member))))
               (link (encode-utf8 (or (and (eq kind :symlink) (archive-member-link-target member)) "")))
               (data (if (eq kind :file) (archive-member-data member) #()))
               (mtime (archive-member-mtime member))
               (records '()))
          (multiple-value-bind (prefix short-name) (and (%ascii-octets-p name) (%split-ustar-name name))
            (unless short-name
              (push (%pax-record "path" (archive-member-name member)) records)
              (setf prefix (make-array 0 :element-type '(unsigned-byte 8))
                    short-name (map 'octets (lambda (byte) (if (< 0 byte 128) byte 95))
                                    (subseq name (max 0 (- (length name) 100))))))
            (when (or (> (length link) 100) (not (%ascii-octets-p link)))
              (push (%pax-record "linkpath" (archive-member-link-target member)) records))
            (when (>= (length data) octal-limit)
              (push (%pax-record "size" (princ-to-string (length data))) records))
            (when (>= mtime octal-limit)
              (push (%pax-record "mtime" (princ-to-string mtime)) records))
            (when records
              (let ((body (apply #'concatenate 'octets (reverse records)))
                    (pax-name (map 'octets #'char-code "PaxHeader/entry")))
                (put-padded (%tar-header pax-name #() #() (length body) #o644 (min mtime (1- octal-limit)) #\x))
                (put-padded body)))
            (put-padded (%tar-header short-name prefix link (min (length data) (1- octal-limit))
                                     (archive-member-mode member)
                                     (min mtime (1- octal-limit))
                                     (ecase kind (:file #\0) (:directory #\5) (:symlink #\2))))
            (put-padded data))))
      (loop repeat (* 2 +tar-block+) do (vector-push-extend 0 sink))
      (loop repeat (mod (- (fill-pointer sink)) +tar-record+) do (vector-push-extend 0 sink)))
    (coerce sink 'octets)))
