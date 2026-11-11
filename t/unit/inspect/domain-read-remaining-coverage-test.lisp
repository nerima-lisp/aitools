(in-package #:aitools.inspect.test)

(defun %remaining-coverage-member (name text)
  (aitools.text.domain:make-archive-member
   :name name
   :kind :file
   :mode #o644
   :mtime 1700000000
   :data (%as-octets text)))

(defun %remaining-coverage-open-archive (octets path)
  (aitools.inspect.domain:open-archive/k
   octets path
   :on-archive #'identity
   :on-unsupported (constantly nil)
   :on-malformed (constantly nil)
   :on-too-large (constantly nil)))

(defun %remaining-coverage-item-content (archive item)
  (aitools.inspect.domain:archive-item-content/k
   archive item
   :on-content #'identity
   :on-too-large (constantly nil)
   :on-malformed (constantly nil)))

(describe "archive domain continuations"
  (it "opens a ZIP with its default options and reads an indexed item"
    (let* ((zip (aitools.text.domain:write-zip
                 (list (%remaining-coverage-member "entry.txt" "body"))))
           (archive (%remaining-coverage-open-archive zip "a.zip"))
           (item (aitools.inspect.domain:find-archive-item archive "entry.txt")))
      (expect (aitools.inspect.domain:archive-format archive) :to-be :zip)
      (expect (aitools.inspect.domain:archive-item-kind item) :to-be :file)
      (expect (%remaining-coverage-item-content archive item)
              :to-equalp (%as-octets "body"))
      (signals type-error
        (aitools.inspect.domain::make-archive-item
         "bad.txt" :file (parse-integer "-1") nil 0 nil))))

  (it "reads the decompressed data for an item without an archive entry"
    (let* ((gzip (aitools.text.domain:gzip-compress
                  (%as-octets "body") :name "body.txt"))
           (archive (%remaining-coverage-open-archive gzip "body.txt.gz"))
           (item (aitools.inspect.domain:find-archive-item archive "body.txt")))
      (expect (aitools.inspect.domain:archive-format archive) :to-be :gz)
      (expect (aitools.inspect.domain:archive-item-kind item) :to-be :file)
      (expect (%remaining-coverage-item-content archive item)
              :to-equalp (%as-octets "body")))))

(describe "line comparison defaults"
  (it "compares unchanged and changed lines with default keyword options"
    (expect (aitools.inspect.domain:compare-lines #("same") #("same"))
            :to-be nil)
    (expect (length (aitools.inspect.domain:compare-lines #("old") #("new")))
            :to-be 1))

  (it "retexts a whitespace-only difference when normalization is requested"
    (expect (aitools.inspect.domain:compare-lines #("a b") #("ab")
                                                   :ignore-whitespace t)
            :to-be nil)))

(describe "Lisp scanner final states and character forms"
  (it "returns the location of an unterminated string and reports unmatched delimiters"
    (expect (aitools.inspect.domain:scan-lisp-delimiters
             #("\"open") :common-lisp (lambda (kind char line column)
                                          (declare (ignore kind char line column))
                                          nil))
            :to-equal '(:string 0 0))
    (expect (aitools.inspect.domain:lisp-balance-diagnostics
             (lines-of ")") :common-lisp)
            :to-equal '((1 1 "unexpected ')' with no open delimiter"))))

  (it "skips plain and escaped Emacs Lisp character literals"
    (expect (aitools.inspect.domain:lisp-balance-diagnostics
             (lines-of "(list ?x)") :emacs-lisp)
            :to-be nil)
    (expect (aitools.inspect.domain:lisp-balance-diagnostics
             (lines-of (format nil "(list ?~C))" #\\)) :emacs-lisp)
            :to-be nil)))

(describe "definition constructor bounds"
  (it "accepts positive lines and rejects invalid start or end lines"
    (let ((definition
            (aitools.inspect.domain::make-definition 1 2 "function" "run")))
      (expect (aitools.inspect.domain:definition-line definition) :to-be 1)
      (expect (aitools.inspect.domain:definition-end-line definition) :to-be 2))
    (signals type-error
      (aitools.inspect.domain::make-definition (parse-integer "0") 1 "function" "run"))
    (signals type-error
      (aitools.inspect.domain::make-definition 1 (parse-integer "0") "function" "run"))))

(describe "read rendering empty and split runs"
  (it "returns no rows for an empty hexadecimal range"
    (expect (aitools.inspect.domain:hex-rows (%as-octets "abc") 2 2)
            :to-equal nil)
    (expect (json-object-get
             (first (aitools.inspect.domain:hex-rows (%as-octets "abc") 0 3 :base 7))
             "hex")
            :to-equal "61 62 63"))

  (it "bounds a redaction span with shared prefixes and suffixes"
    (expect (multiple-value-list
             (aitools.inspect.domain::%redacted-span "secret" "[REDACTED]"))
            :to-equal (list 0 6))
    (expect (multiple-value-list
             (aitools.inspect.domain::%redacted-span
              "left secret" "left [REDACTED]"))
            :to-equal (list 5 11))
    (expect (multiple-value-list
             (aitools.inspect.domain::%redacted-span
              "secret-tail" "[REDACTED]-tail"))
            :to-equal (list 0 6)))

  (it "extracts separated printable runs and emits nothing for controls"
    (let ((runs nil))
      (expect (aitools.inspect.domain::extract-strings
               (concatenate (quote (simple-array (unsigned-byte 8) (*)))
                            (%as-octets "abc") #(0) (%as-octets "def"))
               :min-length 3
               :emit (lambda (offset text)
                       (push (list offset text) runs)))
              :to-be nil)
      (expect (nreverse runs) :to-equal (list (list 0 "abc") (list 4 "def"))))
    (let ((runs nil))
      (aitools.inspect.domain::extract-strings
       (%as-octets (string (code-char 0)))
       :min-length 2
       :emit (lambda (offset text) (push (list offset text) runs)))
      (expect runs :to-be nil))))

(describe "similarity rank order"
  (it "returns nearest matches in input order when distances tie"
    (expect (aitools.inspect.domain:rank-similar
             "alpha" '("alpga" "alphi" "omega")
             :count 2 :max-distance 1)
            :to-equal '("alpga" "alphi"))))

(describe "snapshot defaults"
  (it "leaves omitted scan options nil and validates snapshot file sizes"
    (let ((snapshot (aitools.inspect.domain:make-snapshot "id" "created" nil)))
      (expect (list (aitools.inspect.domain:snapshot-glob snapshot)
                    (aitools.inspect.domain:snapshot-lang snapshot)
                    (aitools.inspect.domain:snapshot-no-ignore snapshot)
                    (aitools.inspect.domain:snapshot-skip-larger-than snapshot)
                    (aitools.inspect.domain:snapshot-newer snapshot))
              :to-equal '(nil nil nil nil nil)))
    (expect (aitools.inspect.domain::make-snapshot-file "entry" 0 0 "hash")
            :to-be-truthy)
    (signals type-error
      (aitools.inspect.domain::make-snapshot-file "entry" (parse-integer "-1") 0 "hash"))))
(describe "table number normalization"
  (it "returns valid signed integers and decimal values"
    (expect (aitools.inspect.domain::%number-value "12") :to-be 12)
    (expect (aitools.inspect.domain::%number-value "-12.5")
            :to-be-close-to -12.5d0)
    (expect (aitools.inspect.domain::%number-value "-3.5e2")
            :to-be-close-to -350d0)))
