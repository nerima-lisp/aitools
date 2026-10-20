(in-package #:aitools.env.domain)

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
  (let* ((length (length text))
         (clock-end
          (cond
            ((and basic-p (>= length 6) (ascii-digits-p text :end 6)) 6)
            ((and basic-p (>= length 4) (ascii-digits-p text :end 4)) 4)
            ((and (not basic-p)
                  (>= length 8)
                  (ascii-digits-p text :end 2)
                  (char= (char text 2) #\:)
                  (ascii-digits-p text :start 3 :end 5)
                  (char= (char text 5) #\:)
                  (ascii-digits-p text :start 6 :end 8))
             8)
            ((and (not basic-p)
                  (>= length 5)
                  (ascii-digits-p text :end 2)
                  (char= (char text 2) #\:)
                  (ascii-digits-p text :start 3 :end 5))
             5)))
         (seconds-p (and clock-end (= clock-end (if basic-p 6 8))))
         (fraction-start
          (and seconds-p
               (< clock-end length)
               (find (char text clock-end) ".,")
               clock-end))
         (time-end
          (cond
            (fraction-start
             (position-if-not #'ascii-digit-value text :start (1+ fraction-start)))
            (clock-end clock-end)
            (t
             (position-if-not
              (lambda (char)
                (or (ascii-digit-value char) (char= char #\:)))
              text))))
         (accepted-time (subseq text 0 (or time-end length)))
         (fraction (and fraction-start (subseq accepted-time fraction-start)))
         (clock (subseq accepted-time 0 (or clock-end (length accepted-time))))
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
     (cond
       ((or
         (and (not basic-p)
              (eql clock-end 5)
              (< clock-end length)
              (char= (char text clock-end) #\:))
         (and basic-p
              (eql clock-end 4)
              (< clock-end length)
              (ascii-digit-value (char text clock-end))))
        :invalid-time-of-day)
       ((and time-end (< time-end length))
        (subseq text time-end))))))

(defun %local-time-error-reason (text basic-p)
  (let* ((text-length (length text))
         (seconds-end (if basic-p 6 8))
         (seconds-p
          (and (>= text-length seconds-end)
               (if basic-p
                   (ascii-digits-p text :end seconds-end)
                   (and
                    (ascii-digits-p text :end 2)
                    (char= (char text 2) #\:)
                    (ascii-digits-p text :start 3 :end 5)
                    (char= (char text 5) #\:)
                    (ascii-digits-p text :start 6 :end seconds-end)))))
         (fraction-start
          (and seconds-p
               (< seconds-end text-length)
               (find (char text seconds-end) ".,")
               seconds-end))
         (fraction (and fraction-start (subseq text (1+ fraction-start))))
         (fraction-length (and fraction (length fraction))))
    (cond
      ((and fraction
            (or (zerop fraction-length) (> fraction-length 9) (not (ascii-digits-p fraction))))
       "invalid fraction of a second")
      ((and (not basic-p) (or (< text-length 3) (not (char= (char text 2) #\:))))
       "expected HH:MM")
      ((and (plusp text-length)
            (find (char text 0) "+-Zz"))
       "invalid time of day")
      (t "invalid time of day"))))

(defun %parse-kit-local-date-time (date time raw-time basic-p original trailing)
  (let ((date-time
         (handler-case (cl-date-kit:parse-local-date-time (concatenate 'string date "T" time))
           (cl-date-kit:date-time-parse-error ()
             (%time-syntax-error original (%local-time-error-reason raw-time basic-p))))))
    (when trailing
      (%time-syntax-error
       original
       (if (eq trailing :invalid-time-of-day)
           "invalid time of day"
           "unexpected text after the time")))
    date-time))

(defun %parse-kit-offset-seconds (suffix original)
  (let* ((suffix-length (length suffix))
         (utc-designator-p
          (and (plusp suffix-length) (char-equal (char suffix 0) #\Z)))
         (offset-end
          (cond
            (utc-designator-p 1)
            ((and (>= suffix-length 3)
                  (find (char suffix 0) "+-")
                  (ascii-digits-p suffix :start 1 :end 3))
             (cond
               ((= suffix-length 3) 3)
               ((char= (char suffix 3) #\:)
                (if (and (>= suffix-length 6) (ascii-digits-p suffix :start 4 :end 6))
                    6
                    (%time-syntax-error original "invalid UTC offset")))
               ((and (>= suffix-length 5) (ascii-digits-p suffix :start 3 :end 5)) 5)
               (t (%time-syntax-error original "invalid UTC offset"))))
            (t (%time-syntax-error original "invalid UTC offset"))))
         (offset-seconds
          (if utc-designator-p
              0
              (let* ((hours (parse-ascii-integer suffix 1 3))
                     (minutes
                      (case offset-end
                        (3 0)
                        (5 (parse-ascii-integer suffix 3 5))
                        (6 (parse-ascii-integer suffix 4 6))))
                     (sign (if (char= (char suffix 0) #\-) -1 1)))
                (unless (and (< hours 24) (<= minutes 59))
                  (%time-syntax-error original "invalid UTC offset"))
                (* sign (+ (* hours 3600) (* minutes 60)))))))
    (when (< offset-end suffix-length)
      (%time-syntax-error original "unexpected text after the offset"))
    offset-seconds))

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
        (let* ((time-start (if has-time (1+ date-end) date-end))
               (offset-start
                (and has-time
                     (position-if
                      (lambda (char)
                        (find char "+-Zz"))
                      text
                      :start (min (length text) (1+ time-start)))))
               (raw-time
                (if has-time
                    (subseq text time-start (or offset-start (length text)))
                    "00:00:00"))
               (suffix (and offset-start (subseq text offset-start))))
          (multiple-value-bind (time trailing) (%normalize-kit-time raw-time basic-p)
            (let ((local-date-time (%parse-kit-local-date-time date time raw-time basic-p text trailing)))
              (if suffix
                  (let ((offset-seconds (%parse-kit-offset-seconds suffix text)))
                    (make-time-input
                     :instant
                     "iso8601"
                     (- (%local-date-time-milliseconds
                         local-date-time
                         (cl-date-kit:zone-offset-utc))
                        (* offset-seconds +milliseconds-per-second+))))
                  (make-time-input
                   :local
                   "iso8601"
                   (%local-date-time-milliseconds
                    local-date-time
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
