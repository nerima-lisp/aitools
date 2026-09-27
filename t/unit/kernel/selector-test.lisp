;;;; t/unit/kernel/selector-test.lisp
(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain selector"
  (it "parses a S:E range"
    (multiple-value-bind (start end) (parse-range-spec "5:10")
      (expect start :to-be 5)
      (expect end :to-be 10)))

  (it "parses a S: range as open-ended"
    (multiple-value-bind (start end) (parse-range-spec "5:")
      (expect start :to-be 5)
      (expect end :to-be-falsy)))

  (it "parses a bare N as N:N"
    (multiple-value-bind (start end) (parse-range-spec "7")
      (expect start :to-be 7)
      (expect end :to-be 7)))

  (it "rejects a range whose end precedes its start"
    (signals error (parse-range-spec "10:5")))

  (it "rejects a line number below 1"
    (signals error (parse-range-spec "0")))

  (it "rejects an empty --old selector"
    (signals error (make-old-selector "")))

  (it "reports --old as content-basis and ambiguous on multiple matches"
    (let ((selector (make-old-selector "x")))
      (expect (selector-basis selector) :to-be :content)
      (expect (selector-uniqueness selector) :to-be :ambiguous)))

  (it "reports --range as position-basis and never ambiguous"
    (let ((selector (make-range-selector "1:2")))
      (expect (selector-basis selector) :to-be :position)
      (expect (selector-uniqueness selector) :to-be :fixed)))

  (it "reports --match as content-basis and selects all matches"
    (let ((selector (make-match-selector "re")))
      (expect (selector-uniqueness selector) :to-be :select-all)))

  (it "lets edit accept every selector kind"
    (dolist (kind '(:old :range :symbol :between :match))
      (expect (selector-accepts-command-p :edit kind) :to-be-truthy)))

  (it "does not let read accept --old"
    (expect (selector-accepts-command-p :read :old) :to-be-falsy))

  (it "lets read accept --range"
    (expect (selector-accepts-command-p :read :range) :to-be-truthy)))

(describe "aitools.kernel.domain parse-range-spec edges"
  (it "accepts an S:E range whose end equals its start"
    (multiple-value-bind (start end) (parse-range-spec "4:4")
      (expect start :to-be 4)
      (expect end :to-be 4)))

  ;; Every malformed spec is a SIMPLE-ERROR, the condition the docstring
  ;; promises, not whatever the underlying integer reader happens to signal.
  (it-each ((":5")    ; no start line
            ("0:5")   ; a zero start in the S:E form
            ("-1")    ; a sign
            ("+3")    ; a sign
            (" 3")    ; whitespace
            ("3: 5")  ; whitespace after the colon
            ("abc")   ; not a number
            ("3:x")   ; a non-numeric end
            ("")      ; nothing at all
            ("1:2:3") ; a second colon
            ("٣"))    ; a non-ASCII digit
      "rejects ~S as a simple error"
      (spec)
    (signals simple-error (parse-range-spec spec))))

(describe "aitools.kernel.domain selector catalog"
  (it-each ((:old :content :ambiguous)
            (:range :position :fixed)
            (:symbol :position :ambiguous)
            (:between :content :ambiguous)
            (:match :content :select-all))
      "reports ~S as ~S-basis with ~S uniqueness"
      (kind basis uniqueness)
    (expect (selector-basis kind) :to-be basis)
    (expect (selector-uniqueness kind) :to-be uniqueness))

  (it "reports a built selector the same as its kind"
    (expect (selector-basis (make-symbol-selector "f" :kind "defun")) :to-be :position)
    (expect (selector-uniqueness (make-between-selector "a" "b")) :to-be :ambiguous))

  (it "signals for a kind outside the catalog"
    (signals simple-error (selector-basis :regex))
    (signals simple-error (selector-uniqueness :regex)))

  (it "accepts no selector for a command absent from the acceptance table"
    (dolist (kind '(:old :range :symbol :between :match))
      (expect (selector-accepts-command-p :write kind) :to-be-falsy))))
