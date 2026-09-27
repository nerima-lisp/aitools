;;;; t/unit/util/calc-test.lisp
(in-package #:aitools.util.test)

(describe "aitools.util.domain evaluate-expression/k"
  (it-each (("1 + 2 * 3" 7) ("(1 + 2) * 3" 9) ("2 ** 3 ** 2" 512) ("-2 ** 2" -4) ("(-2) ** 2" 4)
            ("10 - 4 - 3" 3) ("2 * 3 % 4" 2) ("2 ** -1" 1/2) ("- - 3" 3) ("8 / 4 / 2" 1))
      "evaluates ~A with standard precedence and associativity"
      (text expected)
    (multiple-value-bind (kind value) (calc text)
      (expect kind :to-be :value)
      (expect value :to-equal expected)))

  (it "keeps integers exact beyond 64 bits"
    (expect (nth-value 1 (calc "2**200 + 1")) :to-equal (1+ (expt 2 200)))
    (expect (nth-value 1 (calc "123456789012345678901234567890 * 987654321098765432109876543210"))
            :to-equal (* 123456789012345678901234567890 987654321098765432109876543210)))

  (it "computes division and decimal literals as exact rationals"
    (expect (nth-value 1 (calc "1/3 + 1/6")) :to-equal 1/2)
    (expect (nth-value 1 (calc "0.1 + 0.2")) :to-equal 3/10))

  (it-each (("min(3, 1.5, 2)" 3/2) ("max(1, 7/2)" 7/2) ("abs(0 - 4)" 4) ("floor(0 - 1.5)" -2) ("ceil(1.2)" 2)
            ("round(2.5)" 3) ("round(0 - 2.5)" -3) ("round(1.49)" 1) ("MIN(2, 1)" 1) ("7 % (0-3)" -2))
      "evaluates the fixed function or operator in ~A"
      (text expected)
    (expect (nth-value 1 (calc text)) :to-equal expected))

  (it-each (("1/0") ("5 % 0") ("0 ** (0-1)") ("1/(2-2)"))
      "rejects ~A as division by zero"
      (text)
    (multiple-value-bind (kind reason) (calc text)
      (expect kind :to-be :error)
      (expect reason :to-contain "division by zero")))

  (it-each (("x") ("x = 1") ("a + 1") ("defun f (x) x") ("f(1)") ("lambda(x)") ("sqrt(4)") ("pi")
            ("#.(run-program)") ("(+ 1 2)") ("1 2") ("") ("1 +") ("(1") ("1)") ("1.") (".5") ("2 ** 0.5")
            ("min()") ("abs(1, 2)") ("1e5") ("１"))
      "rejects ~S"
      (text)
    (expect (calc text) :to-be :error))

  (it "rejects identifiers as unknown names rather than evaluating them"
    (expect (nth-value 1 (calc "x")) :to-contain "no variables"))

  (it "rejects an exponent whose result exceeds the bit budget before computing it"
    (multiple-value-bind (kind reason) (calc "9 ** 9 ** 9")
      (expect kind :to-be :error)
      (expect reason :to-contain "bits")))

  (it "rejects a huge exponent of a negative base before computing it"
    (expect (calc "(0-2) ** 1000000000") :to-be :error)
    (expect (calc "(0-1/2) ** 1000000000") :to-be :error))

  (it "accepts a value exactly at the bit budget and rejects one bit more"
    (expect (nth-value 1 (calc "2**65535")) :to-equal (expt 2 65535))
    (expect (calc "2**65536") :to-be :error))

  (it "rejects a product that exceeds the bit budget"
    (expect (calc "2**60000 * 2**60000") :to-be :error))

  (it "allows huge exponents of 0, 1, and -1"
    (expect (nth-value 1 (calc "1 ** 100000000")) :to-equal 1)
    (expect (nth-value 1 (calc "(0-1) ** 100000001")) :to-equal -1))

  (it "rejects nesting beyond the depth budget"
    (let ((deep (concatenate 'string (make-string 100 :initial-element #\() "1" (make-string 100 :initial-element #\)))))
      (expect (calc deep) :to-be :error)
      (expect (calc (concatenate 'string (make-string 60 :initial-element #\() "1" (make-string 60 :initial-element #\))))
              :to-be :value)
      (expect (calc (concatenate 'string (make-string 100 :initial-element #\-) "1")) :to-be :error)))

  (it "rejects input longer than the length budget"
    (expect (calc (make-string 5000 :initial-element #\1)) :to-be :error))

  (it "reports the character offset of a syntax error"
    (expect (evaluate-expression/k "1 + $" :on-value #'identity :on-error (lambda (offset reason)
                                                                         (declare (ignore reason))
                                                                         offset))
            :to-be 4)))

(describe "aitools.util.domain format-decimal and format-exact"
  (it-each ((1/3 10 "0.3333333333") (2/3 2 "0.67") (1/2 10 "0.5") (-1/2 0 "-1") (5 3 "5")
            (-1/3 4 "-0.3333") (1/1000 2 "0") (-1/1000 2 "0") (1/8 2 "0.13"))
      "renders ~A at ~A decimals"
      (value decimals expected)
    (expect (format-decimal value decimals) :to-equal expected))

  (it "renders non-integers exactly and integers as NIL"
    (expect (format-exact 1/3) :to-equal "1/3")
    (expect (format-exact -7/2) :to-equal "-7/2")
    (expect (format-exact 4) :to-be nil)))

(defun calc-failure (text)
  "(OFFSET REASON) of TEXT's evaluation error, or :VALUE when it evaluates."
  (evaluate-expression/k text :on-value (constantly :value) :on-error #'list))

(describe "aitools.util.domain evaluate-expression/k error reports"
  (it-each (("1 = 2" 2 "assignment is not supported")
            (".5" 0 "a decimal literal must start with a digit")
            ("1." 1 "a decimal point must be followed by digits")
            ("1 $" 2 "unexpected character")
            ("   " 0 "empty expression")
            ("1 2" 2 "unexpected trailing input")
            ("1 +" 3 "unexpected end of expression")
            ("1 + )" 4 "expected a number, '(' or a function")
            ("abs 1" 4 "a function name must be followed by '('")
            ("abs(1" 5 "expected ')' after function arguments")
            ("abs(1, 2)" 0 "abs takes exactly 1 argument")
            ("(1" 2 "expected ')'")
            ("x" 0 "unknown name; only min, max, abs, floor, ceil, and round are available (no variables)")
            ("2 ** 0.5" 2 "the exponent of ** must be an integer")
            ("1/0" 1 "division by zero")
            ("2**65536" 1 "result exceeds 65536 bits"))
      "reports ~S at offset ~D: ~A"
      (text offset reason)
    (expect (calc-failure text) :to-equal (list offset reason)))

  (it "reports the length budget at its limit"
    (expect (calc-failure (make-string 4097 :initial-element #\1))
            :to-equal '(4096 "expression exceeds 4096 characters")))

  (it "stops a chain of ** and of nested calls at the nesting budget"
    (let ((powers (with-output-to-string (out) (write-string "1" out) (loop repeat 70 do (write-string "**1" out))))
          (calls (concatenate 'string (apply #'concatenate 'string (make-list 70 :initial-element "abs("))
                              "1" (make-string 70 :initial-element #\)))))
      (expect (calc-failure powers) :to-equal '(193 "nesting exceeds 64 levels"))
      (expect (calc-failure calls) :to-equal '(256 "nesting exceeds 64 levels"))
      (expect (calc-failure (subseq powers 0 (+ 1 (* 3 64)))) :to-be :value))))
