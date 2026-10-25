(in-package #:aitools.env.test)

(progn
  (defparameter *new-york*
    (parse-tzif "America/New_York" (octets-from-hex *new-york-tzif-hex*)))
  (defparameter *tokyo*
    (parse-tzif "Asia/Tokyo" (octets-from-hex *tokyo-tzif-hex*)))

  (defun utc-ms (year month day hour minute second)
    (civil-to-epoch-milliseconds year month day hour minute second 0))

  (defun offset-at (zone instant)
    (multiple-value-list (zone-offset-at zone instant)))

  (describe "aitools.env.domain TZif adapter"
    (it "retains the zone name and reads the transition table"
      (expect (zone-name *new-york*) :to-equal "America/New_York")
      (expect (offset-at *new-york* (utc-ms 2026 3 8 6 59 59))
              :to-equal '(-18000 nil "EST"))
      (expect (offset-at *new-york* (utc-ms 2026 3 8 7 0 0))
              :to-equal '(-14400 t "EDT")))

    (it "applies the footer rule beyond the explicit table"
      (expect (offset-at *new-york* (utc-ms 2040 3 11 6 59 59))
              :to-equal '(-18000 nil "EST"))
      (expect (offset-at *new-york* (utc-ms 2040 3 11 7 0 0))
              :to-equal '(-14400 t "EDT"))
      (expect (offset-at *new-york* (utc-ms 2040 11 4 6 0 0))
              :to-equal '(-18000 nil "EST")))

    (it "preserves Tokyo's standard and historical daylight offsets"
      (expect (offset-at *tokyo* (utc-ms 2026 3 8 0 0 0))
              :to-equal '(32400 nil "JST"))
      (expect (offset-at *tokyo* (utc-ms 1949 7 1 0 0 0))
              :to-equal '(36000 t "JDT")))

    (it "uses the initial type before the first transition"
      (expect (first (offset-at *new-york* (utc-ms 1800 1 1 0 0 0)))
              :to-be -17762))

    (it "shifts a nonexistent spring wall time forward through the gap"
      (expect (zone-local-to-epoch-milliseconds
               *new-york* (utc-ms 2026 3 8 2 30 0))
              :to-be (utc-ms 2026 3 8 7 30 0)))

    (it "resolves a repeated fall wall time to its earlier instant"
      (expect (zone-local-to-epoch-milliseconds
               *new-york* (utc-ms 2026 11 1 1 30 0))
              :to-be (utc-ms 2026 11 1 5 30 0)))

    (it "converts an ordinary wall time in Tokyo"
      (expect (zone-local-to-epoch-milliseconds
               *tokyo* (utc-ms 2026 3 8 15 30 0))
              :to-be (utc-ms 2026 3 8 6 30 0)))

    (it-each ((0) (-64800) (19800) (64800))
        "resolves a fixed zone with offset ~D seconds"
        (offset-seconds)
      (let ((fixed (make-fixed-zone "fixed" offset-seconds)))
        (expect (offset-at fixed 0)
                :to-equal (list offset-seconds nil "fixed"))
        (expect (zone-local-to-epoch-milliseconds fixed 0)
                :to-be (- (* offset-seconds 1000)))))

    (it "preserves TZif error classification and parser reason"
      (expect (handler-case
                  (progn
                    (parse-tzif "invalid" (octets-from-hex "00010203"))
                    :no-error)
                (tzif-format-error (condition)
                  (tzif-format-error-reason condition)))
              :to-equal "missing TZif magic number")
      (let ((octets (octets-from-hex *tokyo-tzif-hex*)))
        (signals tzif-format-error
          (parse-tzif "truncated" (subseq octets 0 60)))))))
