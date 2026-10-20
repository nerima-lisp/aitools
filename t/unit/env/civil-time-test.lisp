;;;; t/unit/env/civil-time-test.lisp
(in-package #:aitools.env.test)

(describe "aitools.env.domain civil time"
  (it "converts known dates to day numbers through the date kit"
    (expect (civil-to-epoch-milliseconds 1970 1 1 0 0 0 0) :to-be 0)
    (expect (civil-to-epoch-milliseconds 2000 3 1 0 0 0 0) :to-be (* 11017 86400000))
    (expect (civil-to-epoch-milliseconds 1969 12 31 0 0 0 0) :to-be -86400000))

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

(aitools/test:it-property
    "round-trips generated instants through ISO formatting and parsing"
    ((milliseconds
       (aitools/test:gen-integer :min -1000000000000 :max 2000000000000)))
  (let* ((text (format-iso8601 milliseconds 19800))
         (input (parse-time-input text)))
    (expect (time-input-kind input) :to-be :instant)
    (expect (time-input-format input) :to-equal "iso8601")
    (expect (time-input-milliseconds input) :to-be milliseconds)))
