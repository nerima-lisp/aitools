(in-package #:aitools.text.domain)

(defconstant +tar-max-pax-mtime+ (1- (* 127 (expt 256 11))))

(defun %parse-pax-number (octets value-start value-end maximum fractional-p reason)
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
          (when (> value (floor (- maximum (- byte 48)) 10))
            (error 'archive-limit-exceeded
                   :limit maximum
                   :reason reason))
          (setf value (+ (* value 10) (- byte 48))))
    (when dot
      (loop for index from (1+ dot) below value-end
            for byte = (aref octets index)
            unless (<= 48 byte 57)
            do (%archive-fail "pax numeric value is not decimal")))
    value))

(defun %parse-pax-record (octets position end records pax-key maximum-mtime)
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
                              (setf value (+ (* value 10) digit)))))
                   finally (when too-large
                             (%archive-fail "malformed pax record")))
             value))
         (record-end (+ position length)))
    (when (or (<= length (- space position))
              (> record-end end)
              (/= (aref octets (1- record-end)) 10))
      (%archive-fail "malformed pax record"))
    (let* ((equals (or (position 61 octets :start (1+ space) :end record-end)
                       (%archive-fail "pax record has no key")))
           (key-start (1+ space))
           (value-start (1+ equals))
           (value-end (1- record-end))
           (key (funcall pax-key key-start equals))
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
                                        (:component-too-long +archive-max-component-bytes+))
                               :reason (if link-target-p
                                           "tar link target"
                                           "tar path"))))
                    (decode-utf8 octets :start value-start :end value-end)))
                 ((string= key "size")
                  (%parse-pax-number octets value-start value-end
                                     (- (length octets) end) nil "tar pax size"))
                 (t
                  (%parse-pax-number octets value-start value-end
                                     maximum-mtime t "tar pax mtime"))))))
      (values record-end key duplicate-p value))))

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
                     ((ascii-key-p key-start key-end "mtime") "mtime"))))
      (loop while (< position end)
            do (when (zerop (aref octets position))
                 (return))
               (multiple-value-bind (record-end key duplicate-p value)
                   (%parse-pax-record octets position end records #'pax-key maximum-mtime)
                 (when (and key (not duplicate-p))
                   (push (cons key value) records))
                 (setf position record-end))))
    (nreverse records)))

(defun %pax-integer (records key) "The integer part of recognized PAX numeric KEY." (let ((value (cdr (assoc key records :test (function string=))))) (and (integerp value) value)))
