;;;; t/unit/kernel/duration-test.lisp
(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain duration"
  ;; The 10ms row also guards against reading `ms` as `m` plus a dangling `s`.
  (it-each (("150ms" 150)
            ("2s" 2000)
            ("3m" 180000)
            ("1h" 3600000)
            ("1d" 86400000)
            ("1.5s" 1500)
            ("10ms" 10)
            (".5s" 500)        ; no integer part
            ("0.0014s" 1))     ; rounds to the nearest millisecond
      "parses ~A to its millisecond total"
      (input expected)
    (expect (duration-milliseconds (parse-duration input)) :to-be expected))

  (it-each (("10")     ; a bare number with no unit
            ("-10s")   ; a negative value
            ("")       ; an empty string
            ("ms")     ; a unit with no number
            ("1.s")    ; a dot with no fraction
            (".s")     ; a dot alone
            ("1.2.3s") ; two dots
            ("a.5s")   ; a non-digit integer part
            ("1,5s")   ; a comma decimal separator
            (" 1s")    ; leading whitespace
            ("٣s"))    ; a non-ASCII digit
      "rejects ~S"
      (input)
    (signals invalid-duration-error (parse-duration input))))
