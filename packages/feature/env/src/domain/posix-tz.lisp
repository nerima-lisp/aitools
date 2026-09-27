;;;; packages/feature/env/src/domain/posix-tz.lisp
;;;;
;;;; The POSIX TZ string of a TZif footer (RFC 8536 3.3, POSIX.1 8.3 with the
;;;; RFC's extensions: `<...>` quoted names and transition times from -167 to
;;;; 167 hours). It governs every instant after a zone's last explicit
;;;; transition, so e.g. America/New_York in 2040 is answered from
;;;; `EST5EDT,M3.2.0,M11.1.0`.
;;;;
;;;; POSIX offsets count hours WEST of Greenwich; everything returned here is
;;;; the usual seconds EAST (`EST5` -> -18000).
(in-package #:aitools.env.domain)

(define-condition posix-tz-syntax-error (error)
  ((text :initarg :text :reader posix-tz-syntax-error-text))
  (:report (lambda (condition stream)
             (format stream "invalid POSIX TZ string ~S" (posix-tz-syntax-error-text condition)))))

(defstruct (tz-rule (:constructor make-tz-rule (kind month week day-number time))
                    (:copier nil))
  "KIND :MONTH-WEEK-DAY uses MONTH, WEEK (1-5, 5 = last) and DAY-NUMBER
(0 = Sunday); :JULIAN-1 uses DAY-NUMBER 1-365 ignoring Feb 29; :JULIAN-0
uses DAY-NUMBER 0-365 counting Feb 29. TIME is seconds after local midnight."
  (kind nil :read-only t)
  (month 0 :read-only t)
  (week 0 :read-only t)
  (day-number 0 :read-only t)
  (time 7200 :read-only t))

(defstruct (posix-tz (:constructor make-posix-tz
                         (standard-name standard-offset &optional daylight-name daylight-offset start end))
                     (:copier nil))
  (standard-name "" :type string :read-only t)
  (standard-offset 0 :type integer :read-only t)
  (daylight-name nil :read-only t)
  (daylight-offset nil :read-only t)
  (start nil :read-only t)
  (end nil :read-only t))

(defun %tz-fail (text)
  (error 'posix-tz-syntax-error :text text))

(defun %ascii-letter-p (char)
  (or (char<= #\a char #\z) (char<= #\A char #\Z)))

(defun %parse-tz-name (text position)
  "(VALUES NAME END) for an unquoted alphabetic name or a `<...>` name."
  (cond
    ((>= position (length text)) (%tz-fail text))
    ((char= (char text position) #\<)
     (let ((close (position #\> text :start position)))
       (unless (and close (>= (- close position 1) 3)) (%tz-fail text))
       (values (subseq text (1+ position) close) (1+ close))))
    (t
     (let ((end (or (position-if-not #'%ascii-letter-p text :start position) (length text))))
       (unless (>= (- end position) 3) (%tz-fail text))
       (values (subseq text position end) end)))))

(defun %parse-tz-hms (text position maximum-hours)
  "(VALUES SECONDS END) for `[+-]hh[:mm[:ss]]` at POSITION."
  (let ((sign 1) (index position))
    (when (and (< index (length text)) (find (char text index) "+-"))
      (when (char= (char text index) #\-) (setf sign -1))
      (incf index))
    (flet ((read-number ()
             (let ((end (or (position-if-not #'ascii-digit-value text :start index) (length text))))
               (unless (<= 1 (- end index) 3) (%tz-fail text))
               (prog1 (parse-ascii-integer text index end) (setf index end)))))
      (let ((hours (read-number)) (minutes 0) (seconds 0))
        (when (and (< index (length text)) (char= (char text index) #\:))
          (incf index)
          (setf minutes (read-number))
          (when (and (< index (length text)) (char= (char text index) #\:))
            (incf index)
            (setf seconds (read-number))))
        (unless (and (<= hours maximum-hours) (<= minutes 59) (<= seconds 59)) (%tz-fail text))
        (values (* sign (+ (* hours 3600) (* minutes 60) seconds)) index)))))

(defun %parse-tz-rule (text position)
  "(VALUES TZ-RULE END) for `Jn`, `n`, or `Mm.w.d`, each with optional `/time`."
  (let ((index position) kind (month 0) (week 0) (day-number 0))
    (flet ((read-integer ()
             (let ((end (or (position-if-not #'ascii-digit-value text :start index) (length text))))
               (unless (<= 1 (- end index) 3) (%tz-fail text))
               (prog1 (parse-ascii-integer text index end) (setf index end))))
           (expect (char)
             (unless (and (< index (length text)) (char= (char text index) char)) (%tz-fail text))
             (incf index)))
      (cond
        ((and (< index (length text)) (char= (char text index) #\M))
         (incf index)
         (setf kind :month-week-day month (read-integer))
         (expect #\.)
         (setf week (read-integer))
         (expect #\.)
         (setf day-number (read-integer))
         (unless (and (<= 1 month 12) (<= 1 week 5) (<= 0 day-number 6)) (%tz-fail text)))
        ((and (< index (length text)) (char= (char text index) #\J))
         (incf index)
         (setf kind :julian-1 day-number (read-integer))
         (unless (<= 1 day-number 365) (%tz-fail text)))
        (t
         (setf kind :julian-0 day-number (read-integer))
         (unless (<= 0 day-number 365) (%tz-fail text))))
      (let ((time 7200))
        (when (and (< index (length text)) (char= (char text index) #\/))
          (multiple-value-setq (time index) (%parse-tz-hms text (1+ index) 167)))
        (values (make-tz-rule kind month week day-number time) index)))))

(defun parse-posix-tz (text)
  "Parse TEXT into a POSIX-TZ. Signals POSIX-TZ-SYNTAX-ERROR. A daylight
name without rules gets the US rules tzcode itself defaults to
(`M3.2.0,M11.1.0`)."
  (multiple-value-bind (standard-name index) (%parse-tz-name text 0)
    (multiple-value-bind (west index) (%parse-tz-hms text index 24)
      (if (= index (length text))
          (make-posix-tz standard-name (- west))
          (multiple-value-bind (daylight-name index) (%parse-tz-name text index)
            (let ((daylight-offset (+ (- west) 3600)))
              (when (and (< index (length text)) (char/= (char text index) #\,))
                (multiple-value-bind (daylight-west end) (%parse-tz-hms text index 24)
                  (setf daylight-offset (- daylight-west) index end)))
              (if (= index (length text))
                  (make-posix-tz standard-name (- west) daylight-name daylight-offset
                                 (make-tz-rule :month-week-day 3 2 0 7200)
                                 (make-tz-rule :month-week-day 11 1 0 7200))
                  (progn
                    (unless (char= (char text index) #\,) (%tz-fail text))
                    (multiple-value-bind (start index) (%parse-tz-rule text (1+ index))
                      (unless (and (< index (length text)) (char= (char text index) #\,)) (%tz-fail text))
                      (multiple-value-bind (end index) (%parse-tz-rule text (1+ index))
                        (unless (= index (length text)) (%tz-fail text))
                        (make-posix-tz standard-name (- west) daylight-name daylight-offset
                                       start end)))))))))))

(defun %rule-day (rule year)
  "Days since the epoch of RULE's date in YEAR."
  (ecase (tz-rule-kind rule)
    (:julian-0 (+ (days-from-civil year 1 1) (tz-rule-day-number rule)))
    (:julian-1 (let ((n (tz-rule-day-number rule)))
                 (+ (days-from-civil year 1 1) (1- n) (if (and (leap-year-p year) (>= n 60)) 1 0))))
    (:month-week-day
     (let* ((month (tz-rule-month rule))
            (first (days-from-civil year month 1))
            (day (+ 1 (mod (- (tz-rule-day-number rule) (weekday-from-days first)) 7)
                    (* 7 (1- (tz-rule-week rule))))))
       (loop while (> day (days-in-month year month)) do (decf day 7))
       (+ first (1- day))))))

(defun posix-tz-offset-at (tz epoch-seconds)
  "(VALUES OFFSET-SECONDS DAYLIGHT-P ABBREVIATION) in effect at EPOCH-SECONDS."
  (let ((standard (posix-tz-standard-offset tz)))
    (if (null (posix-tz-daylight-name tz))
        (values standard nil (posix-tz-standard-name tz))
        (let* ((daylight (posix-tz-daylight-offset tz))
               (year (nth-value 0 (civil-from-days (floor (+ epoch-seconds standard) +seconds-per-day+))))
               (start (- (+ (* (%rule-day (posix-tz-start tz) year) +seconds-per-day+)
                            (tz-rule-time (posix-tz-start tz)))
                         standard))
               (end (- (+ (* (%rule-day (posix-tz-end tz) year) +seconds-per-day+)
                          (tz-rule-time (posix-tz-end tz)))
                       daylight))
               (daylight-p (if (< start end)
                               (and (<= start epoch-seconds) (< epoch-seconds end))
                               (not (and (<= end epoch-seconds) (< epoch-seconds start))))))
          (if daylight-p
              (values daylight t (posix-tz-daylight-name tz))
              (values standard nil (posix-tz-standard-name tz)))))))
