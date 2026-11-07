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

  (it "reports --range as position-basis and never ambiguous"
    (let ((selector (make-range-selector "1:2")))
      (expect (selector-basis selector) :to-be :position)
      (expect (aitools.kernel.domain::selector-uniqueness selector) :to-be :fixed)))

  (it "reports --match as content-basis and selects all matches"
    (let ((selector (make-match-selector "re")))
      (expect (aitools.kernel.domain::selector-uniqueness selector) :to-be :select-all)))

  (it "lets edit accept every selector kind"
    (dolist (kind '(:old :range :symbol :between :match))
      (expect (selector-accepts-command-p :edit kind) :to-be-truthy)))

  (it "does not let read accept --old"
    (expect (selector-accepts-command-p :read :old) :to-be-falsy))

  (it "lets read accept --range"
    (expect (selector-accepts-command-p :read :range) :to-be-truthy))

  (it-each ((:range "2:4" nil nil 2 4 nil nil nil nil nil nil nil)
            (:symbol "main" "defun" nil nil nil "main" "defun" nil nil nil nil nil)
            (:between "start" "end" t nil nil nil nil "start" "end" t nil nil)
            (:match "needle" nil t nil nil nil nil nil nil nil "needle" t))
      "constructs a ~S selector with its supplied fields"
      (kind input-a input-b option range-start range-end symbol-name symbol-kind
       between-start between-end exclusive match-pattern invert)
    (let ((built-selector
            (case kind
              (:range (make-range-selector input-a))
              (:symbol (make-symbol-selector input-a :kind input-b))
              (:between (make-between-selector input-a input-b :exclusive option))
              (:match (make-match-selector input-a :invert option)))))
      (expect (selector-kind built-selector) :to-be kind)
      (expect (selector-range-start built-selector) :to-be range-start)
      (expect (selector-range-end built-selector) :to-be range-end)
      (expect (selector-symbol-name built-selector) :to-be symbol-name)
      (expect (selector-symbol-kind built-selector) :to-be symbol-kind)
      (expect (selector-between-start built-selector) :to-be between-start)
      (expect (selector-between-end built-selector) :to-be between-end)
      (expect (selector-exclusive built-selector) :to-be exclusive)
      (expect (selector-match-pattern built-selector) :to-be match-pattern)
      (expect (selector-invert built-selector) :to-be invert))))

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
    (expect (aitools.kernel.domain::selector-uniqueness kind) :to-be uniqueness))

  (it "reports a built selector the same as its kind"
    (expect (selector-basis (make-symbol-selector "f" :kind "defun")) :to-be :position)
    (expect (aitools.kernel.domain::selector-uniqueness
             (make-between-selector "a" "b"))
            :to-be :ambiguous))

  (it "signals for a kind outside the catalog"
    (signals simple-error (selector-basis :regex))
    (signals simple-error (aitools.kernel.domain::selector-uniqueness :regex)))

  (it "accepts no selector for a command absent from the acceptance table"
    (dolist (kind '(:old :range :symbol :between :match))
      (expect (selector-accepts-command-p :write kind) :to-be-falsy)))

  (it-each ((:edit :old t)
            (:edit :range t)
            (:edit :symbol t)
            (:edit :between t)
            (:edit :match t)
            (:read :old nil)
            (:read :range t)
            (:read :symbol t)
            (:read :between t)
            (:read :match t)
            (:transform :old nil)
            (:transform :range t)
            (:transform :symbol t)
            (:transform :between t)
            (:transform :match t)
            (:move-lines :old nil)
            (:move-lines :range t)
            (:move-lines :symbol t)
            (:move-lines :between t)
            (:move-lines :match t)
            (:archive-read :old nil)
            (:archive-read :range t)
            (:archive-read :symbol t)
            (:archive-read :between t)
            (:archive-read :match t)
            (:insert :old nil)
            (:insert :range t)
            (:insert :symbol t)
            (:insert :between t)
            (:insert :match t)
            (:replace :old nil)
            (:replace :range t)
            (:replace :symbol t)
            (:replace :between t)
            (:replace :match t)
            (:write :old nil)
            (:write :range nil)
            (:write :symbol nil)
            (:write :between nil)
            (:write :match nil))
      "command ~S accepts selector ~S: ~S"
      (command kind accepted)
    (if accepted
        (expect (selector-accepts-command-p command kind) :to-be-truthy)
        (expect (selector-accepts-command-p command kind) :to-be-falsy)))
  (it-each ((:range-spec)
              (:symbol-name)
              (:symbol-kind)
              (:between-start)
              (:between-end)
              (:match-pattern))
  "rejects non-string selector inputs and invalid selector storage"
  (field)
  (let ((value (read-from-string "1"))
        (invalid-zero (read-from-string "0")))
    (signals type-error
      (ecase field
        (:range-spec (make-range-selector value))
        (:symbol-name (make-symbol-selector value))
        (:symbol-kind (make-symbol-selector "name" :kind value))
        (:between-start (make-between-selector value "end"))
        (:between-end (make-between-selector "start" value))
        (:match-pattern (make-match-selector value))))
    (signals type-error (aitools.kernel.domain::%make-selector :old :range-start invalid-zero))
    (signals type-error (aitools.kernel.domain::%make-selector :old :range-end invalid-zero))
    (signals type-error (aitools.kernel.domain::%make-selector :old :exclusive value))
    (signals type-error (aitools.kernel.domain::%make-selector :old :invert value))
    (signals type-error (aitools.kernel.domain::%make-selector value))
    (signals type-error
      (aitools.kernel.domain::make-selector-catalog-entry
       :kind :old :basis value :uniqueness :ambiguous)))))
