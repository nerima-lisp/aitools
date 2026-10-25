;;;;
;;;; Proleptic Gregorian calendar arithmetic over Unix epoch milliseconds
;;;; (the one instant representation the env context uses), ISO 8601
;;;; rendering, and the `human` duration format of `time diff`. Date and
;;;; time conversions use cl-date-kit.
(in-package #:aitools.env.domain)

(defconstant +milliseconds-per-second+ 1000)
(declaim (inline ascii-digit-value))
(defun ascii-digit-value (char)
  "CHAR's value when it is one of U+0030..U+0039, else NIL. DIGIT-CHAR-P is
not used: it also accepts fullwidth and other non-ASCII decimal digits."
  (and (char<= #\0 char #\9) (- (char-code char) (char-code #\0))))

(defun ascii-digits-p (text &key (start 0) (end (length text)))
  (and (< start end)
       (loop for index from start below end
             always (ascii-digit-value (char text index)))))

(defun parse-ascii-integer (text start end)
  "The nonnegative integer spelled by TEXT[START,END), which must be ASCII
digits only (checked by the caller)."
  (let ((value 0))
    (loop for index from start below end
          do (setf value (+ (* value 10) (ascii-digit-value (char text index)))))
    value))

(defun civil-to-epoch-milliseconds (year month day hour minute second millisecond)
  (let ((date-time
          (cl-date-kit:local-date-time-of
           year month day hour minute second (* millisecond 1000000))))
    (+ (* (cl-date-kit:local-date-time-to-epoch-second
           date-time (cl-date-kit:zone-offset-utc))
          +milliseconds-per-second+)
       (floor (cl-date-kit:local-date-time-nanosecond date-time) 1000000))))

(defparameter *minimum-epoch-milliseconds* (civil-to-epoch-milliseconds 0 1 1 0 0 0 0))
(defparameter *maximum-epoch-milliseconds* (civil-to-epoch-milliseconds 9999 12 31 23 59 59 999))

(defun epoch-milliseconds-in-range-p (epoch-milliseconds)
  "True when EPOCH-MILLISECONDS renders as a four-digit ISO 8601 year, the
only years this context reads or writes."
  (<= *minimum-epoch-milliseconds* epoch-milliseconds *maximum-epoch-milliseconds*))

(defun format-utc-offset (offset-seconds)
  (let ((text
          (cl-date-kit:format-zone-offset
           (cl-date-kit:zone-offset-of-total-seconds offset-seconds))))
    (if (string= text "Z") "+00:00" text)))

(defun format-iso8601 (epoch-milliseconds offset-seconds &key utc-designator)
  (let* ((local-date-time
           (cl-date-kit:local-date-time-of-instant
            (cl-date-kit:instant-of-epoch-millis
             (+ epoch-milliseconds (* offset-seconds 1000)))
            (cl-date-kit:zone-offset-utc)))
         (text (cl-date-kit:format-local-date-time local-date-time))
         (fraction-start (position #\. text))
         (date-time-text
           (subseq text 0 (if fraction-start (+ fraction-start 4) (length text))))
         (offset-text
           (if (and utc-designator (zerop offset-seconds))
               "Z"
               (format-utc-offset offset-seconds))))
    (concatenate 'string date-time-text offset-text)))

(defun format-human-duration (milliseconds)
  "`1h23m`, `2d4h`, `1s500ms`, `0s`; a negative value gets a leading `-`.
Zero components are omitted."
  (if (zerop milliseconds)
      "0s"
      (let ((rest (abs milliseconds)))
        (with-output-to-string (out)
          (when (minusp milliseconds) (write-char #\- out))
          (loop for (unit . size) in '(("d" . 86400000) ("h" . 3600000) ("m" . 60000)
                                       ("s" . 1000) ("ms" . 1))
                do (multiple-value-bind (count remainder) (floor rest size)
                     (when (plusp count) (format out "~D~A" count unit))
                     (setf rest remainder)))))))
