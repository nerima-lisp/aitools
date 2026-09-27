;;;; packages/feature/env/src/domain/time-input.lisp
;;;;
;;;; `time convert`'s automatic input-format detection: one parser for every time value `time
;;;; convert` and `time diff` accept.
;;;;
;;;; - All ASCII digits (optionally with a `.fraction`): Unix epoch. Twelve or
;;;;   more integer digits means milliseconds, fewer means seconds; a
;;;;   12-digit second count would be past the year 5000, while a 12-digit
;;;;   millisecond count is 1973 onward. An all-digit value is never read as
;;;;   a basic-format date (`20260308`); write it with dashes instead.
;;;; - ISO 8601 extended (`2026-03-08`, `2026-03-08T01:30`,
;;;;   `2026-03-08 01:30:00.250+09:00`) and basic with a `T`
;;;;   (`20260308T013000Z`). Without an offset the value is a local time,
;;;;   resolved by the caller in the requested zone.
;;;;
;;;; A value starting with `-` never reaches here as a value, so there are no
;;;; negative epochs.
(in-package #:aitools.env.domain)

(define-condition time-syntax-error (error)
  ((text :initarg :text :reader time-syntax-error-text)
   (reason :initarg :reason :reader time-syntax-error-reason))
  (:report (lambda (condition stream)
             (format stream "cannot read ~S as a time: ~A"
                     (time-syntax-error-text condition) (time-syntax-error-reason condition)))))

(defstruct (time-input (:constructor make-time-input (kind format milliseconds))
                       (:copier nil))
  "KIND is :INSTANT (MILLISECONDS is Unix epoch milliseconds) or :LOCAL
(MILLISECONDS is the wall-clock fields encoded as if they were UTC, still to
be resolved against a zone). FORMAT is \"epoch_s\", \"epoch_ms\", or
\"iso8601\"."
  (kind nil :type (member :instant :local) :read-only t)
  (format nil :type string :read-only t)
  (milliseconds 0 :type integer :read-only t))

(defparameter *maximum-time-input-length* 64
  "Longer text is rejected before any parsing: every accepted spelling is
far shorter, and the bound keeps digit runs from building huge integers.")

(defun %time-syntax-error (text reason)
  (error 'time-syntax-error :text text :reason reason))

(defun %fraction-milliseconds (text start end)
  "Milliseconds from the fraction digits TEXT[START,END); digits past the
third are truncated."
  (let ((digits (min 3 (- end start))))
    (* (parse-ascii-integer text start (+ start digits)) (expt 10 (- 3 digits)))))

(defun %parse-epoch (text)
  (let* ((dot (position #\. text))
         (integer-end (or dot (length text))))
    ;; PARSE-TIME-INPUT passes only digits and dots, starting with a digit, so
    ;; only the fraction can be malformed: empty, or holding a second dot.
    (when (and dot (not (ascii-digits-p text :start (1+ dot))))
      (%time-syntax-error text "not an epoch number"))
    (when (> integer-end 16)
      (%time-syntax-error text "epoch value out of range"))
    (let* ((milliseconds-p (>= integer-end 12))
           (whole (parse-ascii-integer text 0 integer-end))
           (fraction (if dot (%fraction-milliseconds text (1+ dot) (length text)) 0))
           (value (if milliseconds-p
                      whole
                      (+ (* whole 1000) fraction))))
      (unless (epoch-milliseconds-in-range-p value)
        (%time-syntax-error text "epoch value out of range"))
      (make-time-input :instant (if milliseconds-p "epoch_ms" "epoch_s") value))))

(defun %read-digits (text position count)
  "The integer spelled by exactly COUNT ASCII digits at POSITION, or NIL."
  (let ((end (+ position count)))
    (when (and (<= end (length text)) (ascii-digits-p text :start position :end end))
      (parse-ascii-integer text position end))))

(defun %parse-offset (text position)
  "(VALUES OFFSET-SECONDS END) for `Z`, `+HH`, `+HHMM`, or `+HH:MM` at
POSITION, or NIL when POSITION is at the end of TEXT (no offset)."
  (when (< position (length text))
    (let ((char (char text position)))
      (cond
        ((char-equal char #\Z) (values 0 (1+ position)))
        ((find char "+-")
         (let* ((sign (if (char= char #\-) -1 1))
                (hours (%read-digits text (1+ position) 2))
                (after-hours (+ position 3))
                (colon-p (and (< after-hours (length text)) (char= (char text after-hours) #\:)))
                (minutes (cond ((= after-hours (length text)) 0)
                               (colon-p (%read-digits text (1+ after-hours) 2))
                               (t (%read-digits text after-hours 2))))
                (end (cond ((= after-hours (length text)) after-hours)
                           (colon-p (+ after-hours 3))
                           (t (+ after-hours 2)))))
           (unless (and hours minutes (<= hours 23) (<= minutes 59))
             (%time-syntax-error text "invalid UTC offset"))
           (values (* sign (+ (* hours 3600) (* minutes 60))) end)))
        (t (%time-syntax-error text "unexpected text after the time"))))))

(defun %parse-iso8601 (text)
  (let* ((basic-p (and (>= (length text) 8) (ascii-digits-p text :end 8)))
         (year (%read-digits text 0 4))
         (month (if basic-p (%read-digits text 4 2) (and (> (length text) 4) (char= (char text 4) #\-) (%read-digits text 5 2))))
         (day (if basic-p
                  (%read-digits text 6 2)
                  (and (> (length text) 7) (char= (char text 7) #\-) (%read-digits text 8 2))))
         (position (if basic-p 8 10))
         (hour 0) (minute 0) (second 0) (millisecond 0) (offset nil))
    (unless (and year month day (<= 1 month 12) (<= 1 day (days-in-month year month)))
      (%time-syntax-error text "invalid date"))
    (when (< position (length text))
      (unless (find (char text position) "Tt ")
        (%time-syntax-error text "expected T between the date and the time"))
      (incf position)
      (setf hour (%read-digits text position 2))
      (incf position 2)
      (unless basic-p
        (unless (and (< position (length text)) (char= (char text position) #\:))
          (%time-syntax-error text "expected HH:MM"))
        (incf position))
      (setf minute (%read-digits text position 2))
      (incf position 2)
      (when (and hour minute (< position (length text))
                 (if basic-p
                     (ascii-digit-value (char text position))
                     (char= (char text position) #\:)))
        (unless basic-p (incf position))
        (setf second (%read-digits text position 2))
        (incf position 2)
        (when (and second (< position (length text)) (find (char text position) ".,"))
          (let ((end (or (position-if-not #'ascii-digit-value text :start (1+ position)) (length text))))
            (when (or (= end (1+ position)) (> (- end position 1) 9))
              (%time-syntax-error text "invalid fraction of a second"))
            (setf millisecond (%fraction-milliseconds text (1+ position) end)
                  position end))))
      (unless (and hour minute second (<= hour 23) (<= minute 59) (<= second 59))
        (%time-syntax-error text "invalid time of day"))
      (multiple-value-bind (value end) (%parse-offset text position)
        (when (and value (/= end (length text)))
          (%time-syntax-error text "unexpected text after the offset"))
        (setf offset value)))
    (let ((local (civil-to-epoch-milliseconds year month day hour minute second millisecond)))
      (if offset
          (make-time-input :instant "iso8601" (- local (* offset 1000)))
          (make-time-input :local "iso8601" local)))))

(defun parse-time-input (text)
  "Parse TEXT per the rules in this file's header. Returns a TIME-INPUT or
signals TIME-SYNTAX-ERROR."
  (cond
    ((or (zerop (length text)) (> (length text) *maximum-time-input-length*))
     (%time-syntax-error text "empty or too long"))
    ((and (ascii-digit-value (char text 0))
          (every (lambda (char) (or (ascii-digit-value char) (char= char #\.))) text))
     (%parse-epoch text))
    (t (%parse-iso8601 text))))
