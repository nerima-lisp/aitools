;;;; packages/feature/env/src/domain/civil-time.lisp
;;;;
;;;; Proleptic Gregorian calendar arithmetic over Unix epoch milliseconds
;;;; (the one instant representation the env context uses), ISO 8601
;;;; rendering, and the `human` duration format of `time diff`. The day
;;;; conversions are Howard Hinnant's `days_from_civil`/`civil_from_days`,
;;;; exact for every year this context accepts.
(in-package #:aitools.env.domain)

(defconstant +milliseconds-per-second+ 1000)
(defconstant +seconds-per-day+ 86400)
(defconstant +milliseconds-per-day+ 86400000)

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

(defun leap-year-p (year)
  (and (zerop (mod year 4)) (or (plusp (mod year 100)) (zerop (mod year 400)))))

(defun days-in-month (year month)
  (if (= month 2)
      (if (leap-year-p year) 29 28)
      (nth (1- month) '(31 28 31 30 31 30 31 31 30 31 30 31))))

(defun civil-from-days (days)
  "(VALUES YEAR MONTH DAY) for DAYS since 1970-01-01."
  (let* ((z (+ days 719468))
         (era (floor z 146097))
         (doe (- z (* era 146097)))
         (yoe (floor (- doe (floor doe 1460) (- (floor doe 36524)) (floor doe 146096)) 365))
         (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (day (1+ (- doy (floor (+ (* 153 mp) 2) 5))))
         (month (if (< mp 10) (+ mp 3) (- mp 9))))
    (values (+ yoe (* era 400) (if (<= month 2) 1 0)) month day)))

(defun weekday-from-days (days)
  "0 for Sunday through 6 for Saturday; 1970-01-01 was a Thursday."
  (mod (+ days 4) 7))

(defun civil-to-epoch-milliseconds (year month day hour minute second millisecond)
  (+ (* (days-from-civil year month day) +milliseconds-per-day+)
     (* (+ (* hour 3600) (* minute 60) second) +milliseconds-per-second+)
     millisecond))

(defun epoch-milliseconds-to-civil (epoch-milliseconds)
  "(VALUES YEAR MONTH DAY HOUR MINUTE SECOND MILLISECOND)."
  (multiple-value-bind (days day-milliseconds) (floor epoch-milliseconds +milliseconds-per-day+)
    (multiple-value-bind (year month day) (civil-from-days days)
      (multiple-value-bind (seconds millisecond) (floor day-milliseconds 1000)
        (multiple-value-bind (hour rest) (floor seconds 3600)
          (multiple-value-bind (minute second) (floor rest 60)
            (values year month day hour minute second millisecond)))))))

(defparameter *minimum-epoch-milliseconds* (civil-to-epoch-milliseconds 0 1 1 0 0 0 0))
(defparameter *maximum-epoch-milliseconds* (civil-to-epoch-milliseconds 9999 12 31 23 59 59 999))

(defun epoch-milliseconds-in-range-p (epoch-milliseconds)
  "True when EPOCH-MILLISECONDS renders as a four-digit ISO 8601 year, the
only years this context reads or writes."
  (<= *minimum-epoch-milliseconds* epoch-milliseconds *maximum-epoch-milliseconds*))

(defun format-utc-offset (offset-seconds)
  "`+09:00`, `-04:00`, or `+05:30`; seconds are shown only when nonzero."
  (multiple-value-bind (hours rest) (floor (abs offset-seconds) 3600)
    (multiple-value-bind (minutes seconds) (floor rest 60)
      (if (zerop seconds)
          (format nil "~:[+~;-~]~2,'0D:~2,'0D" (minusp offset-seconds) hours minutes)
          (format nil "~:[+~;-~]~2,'0D:~2,'0D:~2,'0D" (minusp offset-seconds) hours minutes seconds)))))

(defun format-iso8601 (epoch-milliseconds offset-seconds &key utc-designator)
  "EPOCH-MILLISECONDS as local time at OFFSET-SECONDS, with the offset
suffix; `Z` instead of `+00:00` when UTC-DESIGNATOR is true. Milliseconds
appear only when nonzero, so whole-second values keep the common shape."
  (multiple-value-bind (year month day hour minute second millisecond)
      (epoch-milliseconds-to-civil (+ epoch-milliseconds (* offset-seconds 1000)))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~@[.~3,'0D~]~A"
            year month day hour minute second (and (plusp millisecond) millisecond)
            (if (and utc-designator (zerop offset-seconds)) "Z" (format-utc-offset offset-seconds)))))

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
