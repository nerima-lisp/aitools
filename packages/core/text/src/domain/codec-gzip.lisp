;;;;
;;;; gzip compatibility wrappers over cl-deflate-kit. Reading accepts
;;;; concatenated members and trailing zero padding. FNAME is written as
;;;; UTF-8 and read as UTF-8 when valid, else bytewise.
(in-package #:aitools.text.domain)

(defun %u16le (octets offset)
  (when (> (+ offset 2) (length octets)) (%archive-fail "field is truncated"))
  (logior (aref octets offset) (ash (aref octets (1+ offset)) 8)))

(defun %u32le (octets offset)
  (when (> (+ offset 4) (length octets)) (%archive-fail "field is truncated"))
  (logior (aref octets offset) (ash (aref octets (+ offset 1)) 8)
          (ash (aref octets (+ offset 2)) 16) (ash (aref octets (+ offset 3)) 24)))

(defun %name-from-octets (octets start end)
  "Archive member names: UTF-8 when valid, else one character per byte."
  (flet ((bytewise (position)
           (declare (ignore position))
           (map 'string #'code-char (subseq octets start end))))
    (declare (dynamic-extent #'bytewise))
    (decode-utf8-strict/k octets :start start :end end :on-decoded #'identity :on-invalid #'bytewise)))

(defun %gzip-member-header-prefix-p (octets start end)
  (and (integerp start)
       (<= 0 start)
       (<= (+ start 10) end)
       (= (aref octets start) #x1F)
       (= (aref octets (+ start 1)) #x8B)
       (= (aref octets (+ start 2)) 8)))

(defun %gzip-zero-terminated-end (octets start end)
  (loop for index from start below end
        when (zerop (aref octets index))
          return index))

(defun %gzip-extra-field-end (octets index end)
  (when (<= (+ index 2) end)
    (let ((extra-end (+ index 2 (%u16le octets index))))
      (and (<= extra-end end) extra-end))))

(defun %gzip-overlong-name-tail (octets start flags index end)
  (when (logtest flags 16)
    (setf index (%gzip-zero-terminated-end octets index end))
    (unless index
      (return-from %gzip-overlong-name-tail :unterminated-comment))
    (incf index))
  (when (logtest flags 2)
    (when (> (+ index 2) end)
      (return-from %gzip-overlong-name-tail :truncated-crc))
    (unless (= (%u16le octets index)
               (logand (crc32 octets :start start :end index) #xFFFF))
      (return-from %gzip-overlong-name-tail :bad-crc)))
  :too-long)

(defun %gzip-overlong-fname-problem (octets start)
  (let ((end (length octets)))
    (unless (%gzip-member-header-prefix-p octets start end)
      (return-from %gzip-overlong-fname-problem nil))
    (let* ((flags (aref octets (+ start 3)))
           (index (+ start 10)))
      (when (logtest flags #xE0)
        (return-from %gzip-overlong-fname-problem nil))
      (when (logtest flags 4)
        (setf index (%gzip-extra-field-end octets index end))
        (unless index
          (return-from %gzip-overlong-fname-problem nil)))
      (unless (logtest flags 8)
        (return-from %gzip-overlong-fname-problem nil))
      (let ((fname-end (%gzip-zero-terminated-end octets index end)))
        (unless fname-end
          (return-from %gzip-overlong-fname-problem nil))
        (unless (> (- fname-end index) +archive-max-path-bytes+)
          (return-from %gzip-overlong-fname-problem nil))
        (%gzip-overlong-name-tail octets start flags (1+ fname-end) end)))))

(defun gzip-member-header (octets &key (start 0))
  "Parse a gzip member header at START. Return its name, mtime, and data index."
  (declare (type octets octets))
  (when (and (integerp start)
             (<= 0 start)
             (<= (+ start 10) (length octets))
             (= (aref octets start) #x1F)
             (= (aref octets (+ start 1)) #x8B)
             (/= (aref octets (+ start 2)) 8))
    (%archive-unsupported "gzip compression method"))
  (case (%gzip-overlong-fname-problem octets start)
    (:unterminated-comment (%archive-fail "unterminated gzip header string"))
    (:truncated-crc (%archive-fail "gzip header is truncated"))
    (:bad-crc (%archive-fail "gzip header CRC-32 mismatch"))
    (:too-long
     (error 'archive-limit-exceeded
            :limit +archive-max-path-bytes+
            :reason "gzip member name")))
  (handler-case
      (multiple-value-bind (header data-start)
          (deflate-kit:gzip-member-header-at octets :start start)
        (let ((name (deflate-kit:gzip-member-header-fname header)))
          (values (and name (%name-from-octets name 0 (length name)))
                  (deflate-kit:gzip-member-header-mtime header)
                  data-start)))
    (deflate-kit:invalid-container-error (condition)
      (%archive-fail
       (%gzip-archive-error-reason
        (deflate-kit:deflate-error-reason condition)
        "invalid gzip header")))
    (deflate-kit:checksum-error ()
      (%archive-fail "gzip header CRC-32 mismatch"))
    (deflate-kit:deflate-error ()
      (%archive-fail "invalid gzip header"))))

(defun %gzip-safe-member-name-p (name)
  (and (plusp (length name))
       (not (string= name "."))
       (null (archive-entry-path-problem name))
       (not (find #\/ name))))

(defun %gzip-archive-error-reason (reason fallback)
  (cond
    ((member reason
             '("empty stream" "truncated member header" "truncated header CRC")
             :test #'string=)
     "gzip header is truncated")
    ((string= reason "invalid member signature or compression method")
     "not a gzip stream")
    ((string= reason "reserved header flags are set")
     "reserved gzip flags are set")
    ((member reason
             '("truncated extra field length" "truncated extra field"
               "truncated member trailer")
             :test #'string=)
     "field is truncated")
    ((string= reason "unterminated gzip header field")
     "unterminated gzip header string")
    ((string= reason "member input size mismatch")
     "gzip length mismatch")
    (t fallback)))

(defun %path-basename (path)
  (subseq path (1+ (or (position #\/ path :from-end t) -1))))

(defun gzip-member-name (octets archive-path)
  "The safe leaf name used for a gzip member from OCTETS.

The FNAME header is used only when it is a non-empty, safe leaf. Unsafe or
missing FNAME falls back to ARCHIVE-PATH's basename without its final `.gz`,
or to `.out` when that fallback is unsafe or has no basename."
  (let ((name (nth-value 0 (gzip-member-header octets))))
    (if (and name (%gzip-safe-member-name-p name))
        name
        (let* ((base (%path-basename (or archive-path "")))
               (dot (search ".gz" base :from-end t :test #'char-equal))
               (fallback (if (and dot (plusp dot))
                             (subseq base 0 dot)
                             (concatenate 'string base ".out"))))
          (if (%gzip-safe-member-name-p fallback) fallback ".out")))))

(defun gzip-decompress (octets &key max-output)
  "Decode every gzip member in OCTETS, checking CRCs and the cumulative limit."
  (declare (type octets octets))
  (when (and max-output
             (not (and (integerp max-output) (>= max-output 0))))
    (%archive-fail "invalid gzip output limit"))
  (when (and (>= (length octets) 10)
             (= (aref octets 0) #x1F)
             (= (aref octets 1) #x8B)
             (/= (aref octets 2) 8))
    (%archive-unsupported "gzip compression method"))
  (handler-case
      (let ((index 0)
            (member-count 0)
            (output-size 0)
            (members '()))
        (when (zerop (length octets))
          (%archive-fail "gzip header is truncated"))
        (loop while (< index (length octets))
              do (when (and (plusp member-count)
                            (loop for position from index below (length octets)
                                  always (zerop (aref octets position))))
                   (return))
                 (multiple-value-bind (member header next)
                     (deflate-kit:gzip-decode-prefix
                      octets :start index
                      :max-output (if max-output
                                      (1+ (- max-output output-size))
                                      most-positive-fixnum))
                   (declare (ignore header))
                   (unless next
                     (error 'archive-limit-exceeded
                            :limit max-output :reason "gzip output limit"))
                   (push member members)
                   (incf member-count)
                   (incf output-size (length member))
                   (setf index next)))
        (let ((members (nreverse members)))
          (if (null (cdr members))
              (let ((member (car members)))
                (if (typep member 'octets)
                    member
                    (let ((result (make-array output-size
                                              :element-type '(unsigned-byte 8))))
                      (replace result member)
                      result)))
              (let ((result (make-array output-size
                                        :element-type '(unsigned-byte 8)))
                    (offset 0))
                (dolist (member members result)
                  (replace result member :start1 offset)
                  (incf offset (length member)))))))
    (deflate-kit:invalid-container-error (condition)
      (%archive-fail
       (%gzip-archive-error-reason
        (deflate-kit:deflate-error-reason condition)
        "invalid gzip stream")))
    (deflate-kit:checksum-error ()
      (%archive-fail "gzip checksum mismatch"))
    (deflate-kit:inflate-invalid-data (condition)
      (%archive-fail (deflate-kit:deflate-error-message condition)))
    (deflate-kit:deflate-error ()
      (%archive-fail "invalid gzip stream"))))

(defun gzip-compress (octets &key name (mtime 0))
  "OCTETS as one gzip member with an optional UTF-8 NAME and MTIME."
  (declare (type octets octets))
  (deflate-kit:gzip-encode octets
                           :level 6
                           :filename (and name (encode-utf8 name))
                           :mtime mtime))
