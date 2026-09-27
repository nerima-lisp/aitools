;;;; packages/feature/env/src/domain/tzif.lisp
;;;;
;;;; A reader for TZif files (RFC 8536), the compiled IANA time zone database
;;;; under /usr/share/zoneinfo, and the zone arithmetic built on it.
;;;;
;;;; Version 2+ files carry a second, 64-bit data block and a POSIX TZ footer;
;;;; the 64-bit block is used whenever present. Leap-second records (only in
;;;; the `right/` zones) are skipped: aitools reports POSIX time, which has no
;;;; leap seconds. Every count is checked against the remaining length before
;;;; anything is read, so a truncated or hostile file fails as
;;;; TZIF-FORMAT-ERROR instead of reading out of bounds.
(in-package #:aitools.env.domain)

(define-condition tzif-format-error (error)
  ((reason :initarg :reason :reader tzif-format-error-reason))
  (:report (lambda (condition stream)
             (format stream "invalid TZif data: ~A" (tzif-format-error-reason condition)))))

(defstruct (zone-type (:constructor make-zone-type (offset daylight-p abbreviation))
                      (:copier nil))
  (offset 0 :type integer :read-only t)
  (daylight-p nil :read-only t)
  (abbreviation "" :type string :read-only t))

(defstruct (zone (:constructor %make-zone (name transitions type-indices types footer))
                 (:copier nil))
  "TRANSITIONS is a simple-vector of ascending epoch seconds; TYPE-INDICES
gives, per transition, the index into TYPES in effect from it on. FOOTER is
a POSIX-TZ or NIL."
  (name "" :type string :read-only t)
  (transitions #() :type simple-vector :read-only t)
  (type-indices #() :type simple-vector :read-only t)
  (types #() :type simple-vector :read-only t)
  (footer nil :read-only t))

(defun make-fixed-zone (name offset-seconds)
  "A zone with a single type and no transitions, e.g. the built-in UTC."
  (%make-zone name #() #() (vector (make-zone-type offset-seconds nil name)) nil))

(defun %tzif-fail (reason)
  (error 'tzif-format-error :reason reason))

(defun %read-unsigned (octets position size)
  "Callers bound POSITION + SIZE against the data first (the header against
44 bytes, a data block against its declared length)."
  (let ((value 0))
    (dotimes (index size value)
      (setf value (+ (* value 256) (aref octets (+ position index)))))))

(defun %read-signed (octets position size)
  (let ((value (%read-unsigned octets position size))
        (limit (expt 2 (1- (* 8 size)))))
    (if (>= value limit) (- value (* 2 limit)) value)))

(defun %read-tzif-header (octets position)
  "(VALUES VERSION ISUTCNT ISSTDCNT LEAPCNT TIMECNT TYPECNT CHARCNT) for the
44-byte header at POSITION."
  (unless (and (<= (+ position 44) (length octets))
               (= (aref octets position) (char-code #\T))
               (= (aref octets (+ position 1)) (char-code #\Z))
               (= (aref octets (+ position 2)) (char-code #\i))
               (= (aref octets (+ position 3)) (char-code #\f)))
    (%tzif-fail "missing TZif magic"))
  (let ((version (aref octets (+ position 4))))
    (apply #'values
           (if (zerop version) 1 (- version (char-code #\0)))
           (loop for index from 0 below 6
                 collect (%read-unsigned octets (+ position 20 (* 4 index)) 4)))))

(defun %data-block-length (time-size isutcnt isstdcnt leapcnt timecnt typecnt charcnt)
  (+ (* timecnt time-size) timecnt (* typecnt 6) charcnt
     (* leapcnt (+ time-size 4)) isstdcnt isutcnt))

(defun %abbreviation-at (octets start end index)
  (unless (< index (- end start)) (%tzif-fail "abbreviation index out of range"))
  (let ((terminator (or (position 0 octets :start (+ start index) :end end) end)))
    (map 'string #'code-char (subseq octets (+ start index) terminator))))

(defun %read-data-block (octets position time-size isutcnt isstdcnt leapcnt timecnt typecnt charcnt)
  (declare (ignore isutcnt isstdcnt leapcnt))
  (when (zerop typecnt) (%tzif-fail "no local time types"))
  (let* ((index-start (+ position (* timecnt time-size)))
         (type-start (+ index-start timecnt))
         (chars-start (+ type-start (* typecnt 6))))
    ;; Bound the declared block against the file length before allocating any
    ;; array sized by a header count, so a crafted TZif that names a huge
    ;; timecnt/typecnt fails here instead of requesting the allocation first.
    (when (> (+ chars-start charcnt) (length octets)) (%tzif-fail "truncated"))
    (let ((transitions (make-array timecnt))
          (indices (make-array timecnt))
          (types (make-array typecnt)))
      (dotimes (i timecnt)
        (setf (aref transitions i) (%read-signed octets (+ position (* i time-size)) time-size))
        (when (and (plusp i) (<= (aref transitions i) (aref transitions (1- i))))
          (%tzif-fail "transitions not ascending"))
        (let ((type-index (aref octets (+ index-start i))))
          (unless (< type-index typecnt) (%tzif-fail "type index out of range"))
          (setf (aref indices i) type-index)))
      (dotimes (i typecnt)
        (let ((base (+ type-start (* i 6))))
          (setf (aref types i)
                (make-zone-type (%read-signed octets base 4)
                                (plusp (aref octets (+ base 4)))
                                (%abbreviation-at octets chars-start (+ chars-start charcnt)
                                                  (aref octets (+ base 5)))))))
      (values transitions indices types))))

(defun %read-footer (octets position)
  "The POSIX-TZ from the `\\nTZ\\n` footer at POSITION, or NIL when the
footer is empty or absent."
  (when (and (< position (length octets)) (= (aref octets position) 10))
    (let ((end (position 10 octets :start (1+ position))))
      (unless end (%tzif-fail "unterminated footer"))
      (when (> end (1+ position))
        (handler-case (parse-posix-tz (map 'string #'code-char (subseq octets (1+ position) end)))
          (posix-tz-syntax-error () (%tzif-fail "invalid footer TZ string")))))))

(defun parse-tzif (name octets)
  "Parse OCTETS, a vector of (UNSIGNED-BYTE 8) holding a TZif file, into a
ZONE named NAME. Signals TZIF-FORMAT-ERROR."
  (multiple-value-bind (version isutcnt isstdcnt leapcnt timecnt typecnt charcnt)
      (%read-tzif-header octets 0)
    (if (< version 2)
        (multiple-value-bind (transitions indices types)
            (%read-data-block octets 44 4 isutcnt isstdcnt leapcnt timecnt typecnt charcnt)
          (%make-zone name transitions indices types nil))
        (let ((second-header (+ 44 (%data-block-length 4 isutcnt isstdcnt leapcnt timecnt typecnt charcnt))))
          (multiple-value-bind (version-2 isutcnt isstdcnt leapcnt timecnt typecnt charcnt)
              (%read-tzif-header octets second-header)
            (declare (ignore version-2))
            (let ((data-start (+ second-header 44)))
              (multiple-value-bind (transitions indices types)
                  (%read-data-block octets data-start 8 isutcnt isstdcnt leapcnt timecnt typecnt charcnt)
                (%make-zone name transitions indices types
                            (%read-footer octets (+ data-start
                                                    (%data-block-length 8 isutcnt isstdcnt leapcnt
                                                                        timecnt typecnt charcnt)))))))))))

(defun zone-offset-at (zone epoch-milliseconds)
  "(VALUES OFFSET-SECONDS DAYLIGHT-P ABBREVIATION) in effect in ZONE at the
instant EPOCH-MILLISECONDS. Before the first transition the first time type
applies (RFC 8536 3.2); after the last one the footer rule does, when the
file has one."
  (let* ((seconds (floor epoch-milliseconds 1000))
         (transitions (zone-transitions zone))
         (count (length transitions)))
    (flet ((type-values (type)
             (values (zone-type-offset type) (zone-type-daylight-p type) (zone-type-abbreviation type))))
      (cond
        ((or (zerop count) (and (zone-footer zone) (>= seconds (aref transitions (1- count)))))
         (if (zone-footer zone)
             (posix-tz-offset-at (zone-footer zone) seconds)
             (type-values (aref (zone-types zone) 0))))
        ((< seconds (aref transitions 0))
         (type-values (aref (zone-types zone) 0)))
        (t
         (let ((low 0) (high (1- count)))
           ;; Largest index whose transition is <= SECONDS.
           (loop while (< low high)
                 do (let ((middle (ceiling (+ low high) 2)))
                      (if (<= (aref transitions middle) seconds)
                          (setf low middle)
                          (setf high (1- middle)))))
           (type-values (aref (zone-types zone) (aref (zone-type-indices zone) low)))))))))

(defun zone-local-to-epoch-milliseconds (zone local-milliseconds)
  "The instant whose wall-clock time in ZONE is LOCAL-MILLISECONDS (wall
fields encoded as if UTC). A wall time that occurs twice (the hour repeated
when DST ends) resolves to the earlier instant; a wall time skipped by a
gap (the hour lost when DST starts) is read with the offset in effect before
the gap, so 02:30 on a spring-forward day becomes 03:30 daylight time."
  (let* ((before (zone-offset-at zone (- local-milliseconds +milliseconds-per-day+)))
         (after (zone-offset-at zone (+ local-milliseconds +milliseconds-per-day+)))
         (valid (loop for offset in (remove-duplicates (list before after))
                      for instant = (- local-milliseconds (* offset 1000))
                      when (= (zone-offset-at zone instant) offset)
                        collect instant)))
    (if valid
        (reduce #'min valid)
        (- local-milliseconds (* before 1000)))))
