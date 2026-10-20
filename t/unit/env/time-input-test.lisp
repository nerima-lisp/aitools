;;;; t/unit/env/time-input-test.lisp
(in-package #:aitools.env.test)

(defun parsed (text)
  (let ((input (parse-time-input text)))
    (list (time-input-kind input) (time-input-format input) (time-input-milliseconds input))))

(describe "aitools.env.domain time input detection"
  (it "reads epoch seconds and milliseconds by digit count"
    (expect (parsed "1700000000") :to-equal '(:instant "epoch_s" 1700000000000))
    (expect (parsed "1700000000123") :to-equal '(:instant "epoch_ms" 1700000000123))
    (expect (parsed "1700000000.5") :to-equal '(:instant "epoch_s" 1700000000500))
    (expect (parsed "0") :to-equal '(:instant "epoch_s" 0)))

  (it "reads ISO 8601 with Z and numeric offsets as instants"
    (expect (parsed "2024-02-29T12:34:56Z")
            :to-equal (list :instant "iso8601" 1709210096000))
    (expect (parsed "2024-04-30T23:59:59Z")
            :to-equal (list :instant "iso8601" 1714521599000))
    (expect (parsed "2026-03-08T06:30:00Z")
            :to-equal (list :instant "iso8601" 1772951400000))
    (expect (parsed "2026-03-08T15:30:00+09:00")
            :to-equal (list :instant "iso8601" 1772951400000))
    (expect (parsed "2026-03-08T01:30:00-05:00")
            :to-equal (list :instant "iso8601" 1772951400000))
    (expect (parsed "2026-03-08T06:30:00.250Z")
            :to-equal (list :instant "iso8601" 1772951400250))
    (expect (parsed "20260308T063000Z")
            :to-equal (list :instant "iso8601" 1772951400000)))

  (it "reads an offset-less time or a bare date as local wall time"
    (expect (parsed "2026-03-08T01:30") :to-equal (list :local "iso8601" (- 1772951400000 (* 5 3600000))))
    (expect (parsed "2026-03-08") :to-equal (list :local "iso8601" (* 20520 86400000))))

  (it-each (("") ("garbage") ("not-a-time") ("2026-02-30") ("2026-13-01") ("2026-03-08T25:00")
            ("2026-03-08T01:30:00+24:00") ("2026-03-08T01:30:00Zjunk") ("12.3.4")
            ("2026-03-08T01:30:00.") ("１７００００００００") ("99999999999999999"))
      "rejects ~S with time-syntax-error"
      (text)
    (signals time-syntax-error (parse-time-input text))))

(defun rejection-reason (text)
  (handler-case (progn (parse-time-input text) :parsed)
    (time-syntax-error (condition)
      (expect (time-syntax-error-text condition) :to-equal text)
      (expect (princ-to-string condition)
              :to-equal (format nil "cannot read ~S as a time: ~A" text (time-syntax-error-reason condition)))
      (time-syntax-error-reason condition))))

(describe "aitools.env.domain time input offsets and bounds"
  (it-each (("2026-03-08T10:30+09" 1772933400000)
            ("2026-03-08T10:30:00+0930" 1772931600000)
            ("2026-03-08T01:30-05" 1772951400000)
            ("2026-03-08T06:30:00.123456789Z" 1772951400123)
            ("20260308T0630Z" 1772951400000)
            ("2024-02-29 12:34:56,789Z" 1709210096789))
      "reads ~S as the instant ~D"
      (text milliseconds)
    (expect (parsed text) :to-equal (list :instant "iso8601" milliseconds)))

  (it "reads the largest and smallest epochs it accepts"
    (expect (parsed "253402300799999") :to-equal '(:instant "epoch_ms" 253402300799999))
    (expect (parsed "99999999999") :to-equal '(:instant "epoch_s" 99999999999000)))

  (it-each (("1700000000." "not an epoch number")
            ("1.2.3" "not an epoch number")
            ("253402300800000" "epoch value out of range")
            ("9999999999999999" "epoch value out of range")
            ("99999999999999999" "epoch value out of range")
            ("2026-03-08T01:30+9" "invalid UTC offset")
            ("2026-03-08T01:30+09:6" "invalid UTC offset")
            ("2026-03-08T01:30+093" "invalid UTC offset")
            ("2026-03-08T01:30+24" "invalid UTC offset")
            ("2026-03-08T01:30+09:60" "invalid UTC offset")
            ("2026-03-08T01:30X" "unexpected text after the time")
            ("2026-03-08T01:30+09:00junk" "unexpected text after the offset")
            ("2026-03-08T0130" "expected HH:MM")
            ("2026-03-08X01:30" "expected T between the date and the time")
            ("2026-03-08T01:30:00.1234567890Z" "invalid fraction of a second")
            ("2026-03-08T01:3" "invalid time of day")
            ("2026-03-08T01" "expected HH:MM")
            ("2026-3-08" "invalid date")
            ("not-a-time" "invalid date"))
      "rejects ~S: ~A"
      (text reason)
    (expect (rejection-reason text) :to-equal reason))

  (it "rejects text longer than the input bound before parsing it"
    (expect (rejection-reason (make-string 65 :initial-element #\1)) :to-equal "empty or too long")))

(describe "aitools.env.domain pre-epoch time input"
  (it "keeps fractional milliseconds before the Unix epoch"
    (let ((input (parse-time-input "1969-12-31T23:59:59.750Z")))
      (expect (time-input-kind input) :to-be :instant)
      (expect (time-input-milliseconds input) :to-be -250)
      (expect (format-iso8601 (time-input-milliseconds input) 0 :utc-designator t)
              :to-equal "1969-12-31T23:59:59.750Z"))))
