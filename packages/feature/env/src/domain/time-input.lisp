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

(define-condition time-syntax-error
  (error)
  ((text :initarg :text :reader time-syntax-error-text)
   (reason :initarg :reason :reader time-syntax-error-reason))
  (:report
   (lambda (condition stream)
     (format stream
             "cannot read ~S as a time: ~A"
             (time-syntax-error-text condition)
             (time-syntax-error-reason condition)))))

(defstruct (time-input (:constructor make-time-input (kind format milliseconds)) (:copier nil))
  "KIND is :INSTANT (MILLISECONDS is Unix epoch milliseconds) or :LOCAL
(MILLISECONDS is the wall-clock fields encoded as if they were UTC, still to
be resolved against a zone). FORMAT is \"epoch_s\", \"epoch_ms\", or
\"iso8601\"."
  (kind nil :type (member :instant :local) :read-only t)
  (format nil :type string :read-only t)
  (milliseconds 0 :type integer :read-only t))

(defparameter *maximum-time-input-length*
  64
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

(defun %iso-date-shape (text)
  (cond
    ((and (>= (length text) 8) (ascii-digits-p text :end 8)) (values 8 t))
    ((and (>= (length text) 10)
          (ascii-digits-p text :end 4)
          (char= (char text 4) #\-)
          (ascii-digits-p text :start 5 :end 7)
          (char= (char text 7) #\-)
          (ascii-digits-p text :start 8 :end 10)) (values 10 nil))))

(defun %normalize-kit-time (text basic-p)
  (let* ((raw-fraction-start
          (position-if
           (lambda (char)
             (find char ".,"))
           text))
         (fraction-start
          (and raw-fraction-start
               (not
                (position-if-not
                 (lambda (char)
                   (or (ascii-digit-value char) (char= char #\:)))
                 text
                 :end
                 raw-fraction-start))
               raw-fraction-start))
         (time-end
          (if fraction-start
              (position-if-not #'ascii-digit-value text :start (1+ fraction-start))
              (position-if-not
               (lambda (char)
                 (or (ascii-digit-value char) (char= char #\:)))
               text)))
         (accepted-time (subseq text 0 (or time-end (length text))))
         (fraction (and fraction-start (subseq accepted-time fraction-start)))
         (clock (subseq accepted-time 0 (or fraction-start (length accepted-time))))
         (normalized-clock
          (cond
            ((and basic-p (= (length clock) 4) (ascii-digits-p clock))
             (concatenate 'string (subseq clock 0 2) ":" (subseq clock 2) ":00"))
            ((and basic-p (= (length clock) 6) (ascii-digits-p clock))
             (concatenate 'string (subseq clock 0 2) ":" (subseq clock 2 4) ":" (subseq clock 4)))
            ((and (not basic-p) (= (length clock) 5) (char= (char clock 2) #\:))
             (concatenate 'string clock ":00"))
            (t clock))))
    (values
     (concatenate 'string
                  normalized-clock
                  (if fraction
                      (concatenate 'string "." (substitute #\. #\, (subseq fraction 1)))
                      ""))
     (and time-end (subseq text time-end)))))

(defun %local-time-error-reason (text basic-p)
  (let* ((fraction-start
          (position-if
           (lambda (char)
             (find char ".,"))
           text))
         (fraction (and fraction-start (subseq text (1+ fraction-start))))
         (fraction-length (and fraction (length fraction))))
    (cond
      ((and fraction
            (or (zerop fraction-length) (> fraction-length 9) (not (ascii-digits-p fraction))))
       "invalid fraction of a second")
      ((and (not basic-p) (or (< (length text) 3) (not (char= (char text 2) #\:))))
       "expected HH:MM")
      (t "invalid time of day"))))

(defun %parse-kit-local-date-time (date time basic-p original trailing)
  (let ((date-time
         (handler-case (cl-date-kit:parse-local-date-time (concatenate 'string date "T" time))
           (cl-date-kit:date-time-parse-error ()
             (%time-syntax-error original (%local-time-error-reason time basic-p))))))
    (when trailing
      (%time-syntax-error original "unexpected text after the time"))
    date-time))

(defun %parse-kit-offset-date-time (date time suffix original)
  (let* ((offset-end
          (if (and (plusp (length suffix)) (char-equal (char suffix 0) #\Z))
              1
              (or
               (position-if-not
                (lambda (char)
                  (or (ascii-digit-value char) (char= char #\:)))
                suffix
                :start
                1)
               (length suffix))))
         (offset-text (subseq suffix 0 offset-end))
         (offset-date-time
          (progn
            (unless
                (or (and (= offset-end 1) (char-equal (char suffix 0) #\Z))
                    (member offset-end '(3 5 6)))
              (%time-syntax-error original "invalid UTC offset"))
            (handler-case (cl-date-kit:parse-offset-date-time
                           (concatenate 'string date "T" time offset-text))
              (cl-date-kit:date-time-parse-error ()
                (%time-syntax-error original "invalid UTC offset"))))))
    (when (< offset-end (length suffix))
      (%time-syntax-error original "unexpected text after the offset"))
    offset-date-time))

(defun %local-date-time-milliseconds (date-time offset)
  (+ (* (cl-date-kit:local-date-time-to-epoch-second date-time offset) +milliseconds-per-second+)
     (floor (cl-date-kit:local-date-time-nanosecond date-time) 1000000)))

(defun %parse-iso8601 (text)
  (multiple-value-bind (date-end basic-p) (%iso-date-shape text)
    (unless date-end
      (%time-syntax-error text "invalid date"))
    (let ((date (subseq text 0 date-end)))
      (handler-case (cl-date-kit:parse-local-date date)
        (cl-date-kit:date-time-parse-error ()
          (%time-syntax-error text "invalid date")))
      (let ((has-time (< date-end (length text))))
        (when (and has-time (not (find (char text date-end) "Tt ")))
          (%time-syntax-error text "expected T between the date and the time"))
        (let* ((time-start
                (if has-time
                    (1+ date-end)
                    date-end))
               (offset-start
                (and has-time
                     (position-if
                      (lambda (char)
                        (find char "+-Zz"))
                      text
                      :start
                      time-start)))
               (raw-time
                (if has-time
                    (subseq text time-start (or offset-start (length text)))
                    "00:00:00"))
               (suffix (and offset-start (subseq text offset-start))))
          (multiple-value-bind (time trailing) (%normalize-kit-time raw-time basic-p)
            (let ((local-date-time (%parse-kit-local-date-time date time basic-p text trailing)))
              (if suffix
                  (let* ((offset-date-time (%parse-kit-offset-date-time date time suffix text))
                         (offset-local-date-time
                          (cl-date-kit:offset-date-time-local-date-time offset-date-time))
                         (offset (cl-date-kit:offset-date-time-offset offset-date-time)))
                    (make-time-input :instant
                                     "iso8601"
                                     (%local-date-time-milliseconds offset-local-date-time offset)))
                  (make-time-input :local
                                   "iso8601"
                                   (%local-date-time-milliseconds local-date-time
                                                                  (cl-date-kit:zone-offset-utc)))))))))))

(defun parse-time-input (text)
  "Parse TEXT per the rules in this file's header. Returns a TIME-INPUT or
signals TIME-SYNTAX-ERROR."
  (cond
    ((or (zerop (length text)) (> (length text) *maximum-time-input-length*))
     (%time-syntax-error text "empty or too long"))
    ((and (ascii-digit-value (char text 0))
          (every
           (lambda (char)
             (or (ascii-digit-value char) (char= char #\.)))
           text)) (%parse-epoch text))
    (t (%parse-iso8601 text))))
