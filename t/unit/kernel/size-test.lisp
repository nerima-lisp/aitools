;;;; t/unit/kernel/size-test.lisp
(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain size"
  ;; A bare number is bytes; a fractional size floors to whole bytes.
  (it-each (("512" 512)
            ("10KiB" 10240)
            ("1MiB" 1048576)
            ("1GiB" 1073741824)
            ("1.5KiB" 1536)
            ("0.5" 0)
            ("0KiB" 0))
      "parses ~A to a byte count"
      (input expected)
    (expect (size-bytes (parse-size input)) :to-be expected))

  (it-each (("-1KiB")   ; a negative value
            ("10TiB")   ; an unknown unit
            ("")        ; an empty string
            ("KiB")     ; a unit with no number
            ("1.KiB")   ; a dot with no fraction
            ("٣KiB")    ; a non-ASCII digit
            ("٣"))      ; a bare non-ASCII digit
      "rejects ~S"
      (input)
    (signals invalid-size-error (parse-size input))))
