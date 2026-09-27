;;;; t/unit/env/civil-time-test.lisp
(in-package #:aitools.env.test)

(describe "aitools.env.domain civil time"
  (it "maps the epoch and known dates to day numbers"
    (expect (days-from-civil 1970 1 1) :to-be 0)
    ;; 2000-03-01 is day 11017 (date -u -d 2000-03-01 +%s / 86400).
    (expect (days-from-civil 2000 3 1) :to-be 11017)
    (expect (days-from-civil 1969 12 31) :to-be -1))

  (it "round-trips day numbers across leap and century years"
    (dolist (days '(-719468 -1 0 10957 11016 11017 20520 2932896))
      (multiple-value-bind (year month day) (civil-from-days days)
        (expect (days-from-civil year month day) :to-be days))))

  (it "knows February's length"
    (expect (days-in-month 2024 2) :to-be 29)
    (expect (days-in-month 1900 2) :to-be 28)
    (expect (days-in-month 2000 2) :to-be 29))

  (it "formats ISO 8601 with offsets and optional milliseconds"
    (expect (format-iso8601 1772951400250 0 :utc-designator t) :to-equal "2026-03-08T06:30:00.250Z")
    (expect (format-iso8601 1772951400000 -18000) :to-equal "2026-03-08T01:30:00-05:00")
    (expect (format-iso8601 0 19800) :to-equal "1970-01-01T05:30:00+05:30")
    (expect (format-iso8601 0 0) :to-equal "1970-01-01T00:00:00+00:00"))

  (it "formats the human duration of time diff"
    (expect (format-human-duration 4980000) :to-equal "1h23m")
    (expect (format-human-duration 0) :to-equal "0s")
    (expect (format-human-duration 1500) :to-equal "1s500ms")
    (expect (format-human-duration 90061001) :to-equal "1d1h1m1s1ms")
    (expect (format-human-duration -3600000) :to-equal "-1h")))

(describe "aitools.env.domain UTC offsets"
  (it-each ((0 "+00:00") (32400 "+09:00") (-14400 "-04:00") (19800 "+05:30") (-19815 "-05:30:15") (59 "+00:00:59"))
      "formats ~D seconds as ~S"
      (seconds text)
    (expect (format-utc-offset seconds) :to-equal text)))
