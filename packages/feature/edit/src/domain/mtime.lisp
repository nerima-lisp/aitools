(in-package #:aitools.edit.domain)

;;; `touch --mtime` times
(defparameter +iso-time-pattern+
  (format nil
          "~A~A"
          "^([0-9]{4})-([0-9]{2})-([0-9]{2})(?:[T ]([0-9]{2}):([0-9]{2})(?::([0-9]{2})"
          "(?:\\.[0-9]+)?)?)?(Z|[+-][0-9]{2}:?[0-9]{2})?$"))

(defun %mtime-zone-offset-seconds (zone)
  (if (or (null zone) (string= zone "Z"))
      0
      (let ((digits (remove #\: (subseq zone 1))))
        (*
         (if (char= (char zone 0) #\-)
             -1
             1)
         (+ (* 3600 (parse-integer digits :end 2)) (* 60 (parse-integer digits :start 2)))))))

(defun %parse-iso-mtime-match (match text)
  (flet ((group (number)
           (let ((value (cl-regex-kit:match-group-string match number text)))
             (if value
                 (parse-integer value)
                 0))))
    (let ((zone (cl-regex-kit:match-group-string match 7 text))
          (fields (list (group 6) (group 5) (group 4) (group 3) (group 2) (group 1))))
      (handler-case (let ((universal (apply #'encode-universal-time (append fields '(0)))))
                      (when (equal (subseq (multiple-value-list (decode-universal-time universal 0)) 0 6) fields)
                        (- (aitools.kernel.domain:universal-time-to-unix-seconds universal)
                           (%mtime-zone-offset-seconds zone))))
        (error ()
          nil)))))

(defun parse-mtime (text)
  "Unix seconds for TEXT: digits, @digits, or ISO 8601
YYYY-MM-DD[THH:MM[:SS[.fraction]]][Z|+HH:MM|-HH:MM] (UTC without a zone);
NIL when malformed."
  (flet ((digits-p (string)
           (and (plusp (length string)) (every #'%ascii-digit-p string))))
    (cond
      ((digits-p text) (parse-integer text))
      ((and (> (length text) 1) (char= (char text 0) #\@) (digits-p (subseq text 1))) (parse-integer text :start 1))
      (t
       (let ((match (cl-regex-kit:scan (cl-regex-kit:compile-regex +iso-time-pattern+) text)))
         (when match
           (%parse-iso-mtime-match match text)))))))

(defun iso-utc (unix-seconds)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (aitools.kernel.domain:unix-seconds-to-universal-time unix-seconds) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year month day hour minute second)))
