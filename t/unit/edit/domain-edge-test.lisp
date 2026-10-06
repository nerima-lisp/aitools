;;;; t/unit/edit/domain-edge-test.lisp
;;;;
;;;; Edge cases of the text rules: layout, --old matching, templates, regex
;;;; use, transform ops and split.
(in-package #:aitools.edit.test)

(describe "aitools.edit.application pipeline helpers"
  (it "inspects static write options without reading workspace state"
    (let ((inspection
            (aitools.edit.application::%inspect-write-plan
             (aitools.edit.application:make-write-plan
              :command "edit"
              :targets (list (aitools.edit.application:make-write-target "a.txt"))
              :expect-hashes '("a.txt=00")
              :expect-count "2"
              :plan (lambda (context commit reject)
                      (declare (ignore context commit reject))))
             nil nil '("edit" "a.txt") "a.txt")))
      (expect (first inspection) :to-be :ok)
      (expect (numberp (second inspection)) :to-be t)
      (expect (length (third inspection)) :to-be 1)
      (expect (fourth inspection) :to-be 2)))

  (it "returns the redacted-input refusal from static inspection"
    (expect
     (aitools.edit.application::%inspect-write-plan
      (aitools.edit.application:make-write-plan
       :command "edit"
       :inputs (list aitools.edit.application::+redaction-placeholder+)
       :plan (lambda (context commit reject)
               (declare (ignore context commit reject))))
      nil nil '("edit") nil)
     :to-equal
     (list :error "refusal.redacted-input"
           (format nil "the input contains ~A, an output mask rather than real content"
                   aitools.edit.application::+redaction-placeholder+)
           nil)))

  (it "resolves bare, target-named and additional hash paths by their area"
    (let* ((targets (list (aitools.edit.application:make-write-target "a.txt")))
           (paths '("a.txt"))
           (calls 0)
           (resolve (lambda (path)
                      (incf calls)
                      (values (concatenate 'string "resolved/" path) :temporary))))
      (multiple-value-bind (path area)
          (aitools.edit.application::%resolve-hash-path
           (aitools.kernel.domain:parse-expect-hash-argument "00") targets paths :inside resolve)
        (expect path :to-equal "a.txt")
        (expect area :to-be :inside))
      (multiple-value-bind (path area)
          (aitools.edit.application::%resolve-hash-path
           (aitools.kernel.domain:parse-expect-hash-argument "a.txt=00") targets paths :inside resolve)
        (expect path :to-equal "a.txt")
        (expect area :to-be :inside))
      (multiple-value-bind (path area)
          (aitools.edit.application::%resolve-hash-path
           (aitools.kernel.domain:parse-expect-hash-argument "other=00") targets paths :inside resolve)
        (expect path :to-equal "resolved/other")
        (expect area :to-be :temporary))
      (expect calls :to-be 1))))

(describe "aitools.edit.domain text document edge cases"
  (it "treats an empty document as having no final newline and leaves it alone"
    (let ((empty (doc "")))
      (expect (document-final-newline-p empty) :to-be nil)
      (expect (doc-string (document-with-final-newline empty t)) :to-equal "")))

  (it "drops the BOM and gives a line that gained text the file's line ending"
    (expect (doc-string (document-without-bom (doc (format nil "~Ca~%" (code-char #xFEFF))))) :to-equal (format nil "a~%"))
    (expect (doc-string (document-replace-lines (doc (format nil "a~%b")) 1 2 '("x" "y")))
            :to-equal (format nil "a~%x~%y")))

  (it "keeps the old terminator of the unchanged trailing lines when the logical text changes"
    (let* ((document (doc (format nil "a~C~%b~C~%c~%" #\Return #\Return)))
           (edited (document-with-logical-text document (format nil "A~%b~%c~%") 0 2)))
      (expect (doc-string edited) :to-equal (format nil "A~C~%b~C~%c~%" #\Return #\Return)))))

(describe "aitools.edit.domain --old matching edge cases"
  (flet ((edit (text old new)
           (apply-old-edit (doc text) old new
                           :on-edited (lambda (document strategy line) (list (doc-string document) strategy line))
                           :on-ambiguous (lambda (matches) (list :ambiguous (mapcar #'car matches)))
                           :on-no-match (lambda (candidates) (list :no-match (mapcar #'car candidates))))))
    (it "reports every line of an ambiguous whitespace-insensitive match"
      (expect (edit (format nil "  x~%y~%    x~%") " x " "z") :to-equal '(:ambiguous (1 3))))

    (it "keeps a blank line of --new blank when re-indenting"
      (expect (edit (format nil "f~%    a~%    b~%") (format nil "a~%b") (format nil "c~%~%  d"))
              :to-equal (list (format nil "f~%    c~%~%      d~%") :whitespace 2)))

    (it "offers no candidates in an empty file"
      (expect (edit "" "x" "y") :to-equal '(:no-match ()))))

  (it "ranks similar places on bounded prefixes of long lines"
    (let ((long (make-string 1000 :initial-element #\a)))
      (expect (mapcar #'car (similar-windows (vector "b" long "c") (list (concatenate 'string long "z"))))
              :to-equal '(2 1 3)))))

(describe "aitools.edit.domain replacement templates: literal dollars and filters"
  (flet ((expand (template &rest groups)
           (expand-template (parse-replacement-template template)
                            (lambda (designator) (and (integerp designator) (nth designator groups))))))
    (it-each (("a trailing $" "a$" "a$")
              ("an unclosed ${" "${1" "${1")
              ("$ before a non-name character" "$-x" "$-x")
              ("an empty ${}" "a${}b" "ab")
              ("a group that did not participate" "[$3]" "[]"))
        "keeps ~A literal"
        (name template expected)
      (declare (ignore name))
      (expect (expand template "whole" "one") :to-equal expected))

    (it-each (("dec below zero" "dec" "0" "-1")
              ("dec of a negative number keeps its width" "dec" "-09" "-10")
              ("inc of a negative number" "inc" "-1" "0")
              ("pad leaves a longer value" "pad2" "12345" "12345")
              ("camel of nothing" "camel" "" "")
              ("capitalize of nothing" "capitalize" "" "")
              ("snake splits a digit before a capital" "snake" "v2Beta" "v2_beta")
              ("kebab splits an acronym before a word" "kebab" "HTTPServer" "http-server"))
        "applies ~A"
        (name filter value expected)
      (declare (ignore name))
      (expect (apply-filter filter value) :to-equal expected))

    (it "refuses inc on an empty value and an unknown filter name"
      (expect (refusal-code (lambda () (apply-filter "inc" ""))) :to-equal "argument.invalid")
      (expect (refusal-code (lambda () (apply-filter "dec" "-"))) :to-equal "argument.invalid")
      (expect (refusal-code (lambda () (apply-filter "shout" "x"))) :to-equal "argument.invalid")))

  (it "expands a named group the pattern lacks as empty through the regex replacement"
    (let* ((regex (compile-search-pattern/k "(a)" :on-regex #'identity :on-invalid #'error))
           (function (template-regex-replacement (parse-replacement-template "[${1}|${missing}]"))))
      (expect (cl-regex-kit:replace-all regex "xay" function) :to-equal "x[a|]y"))))

(describe "aitools.edit.domain regex use"
  (it "reports a pattern that does not compile and keeps --fixed text literal"
    (expect (compile-search-pattern/k "(" :on-regex (constantly :compiled) :on-invalid (lambda (message) (search "bad regular expression" message)))
            :to-be 0)
    (let ((regex (compile-search-pattern/k "a.b" :fixed t :on-regex #'identity :on-invalid #'error)))
      (expect (cl-regex-kit:scan regex "axb") :to-be nil)
      (expect (cl-regex-kit:scan regex "a.b") :to-be-truthy)))

  (it "turns a regex failure while matching into input.syntax-error"
    (expect (refusal-code (lambda () (call-with-regex-refusals
                                      (lambda () (error 'cl-regex-kit:cl-regex-kit-error)))))
            :to-equal "input.syntax-error")
    (expect (call-with-regex-refusals (constantly 7)) :to-be 7))

  (it-each (("an empty needle" "" "abc" 0)
            ("a needle longer than the haystack" "abcd" "abc" nil)
            ("a match after a false start" "ab" "aab" 1)
            ("no match" "zz" "abc" nil))
      "finds bytes: ~A"
      (name needle haystack expected)
    (declare (ignore name))
    (expect (octets-search (bytes needle) (bytes haystack)) :to-equal expected))

  (it "offers no byte prefilter for a literal holding a line end"
    (flet ((literal (pattern)
             (replacer-required-literal
              (make-replacer (compile-search-pattern/k pattern :on-regex #'identity :on-invalid #'error)
                             (literal-replacement "x")))))
      (expect (literal "needle") :to-equalp (bytes "needle"))
      (expect (literal (format nil "a~%b")) :to-be nil)))

  (it "replaces across lines with --multiline and refuses a line break without it"
    (flet ((replacer (pattern replacement)
             (make-replacer (compile-search-pattern/k pattern :on-regex #'identity :on-invalid #'error)
                            (literal-replacement replacement))))
      (multiple-value-bind (document count)
          (replace-document (doc (format nil "a~%b~%c~%")) (replacer (format nil "a~%b") "ab") nil t)
        (expect (doc-string document) :to-equal (format nil "ab~%c~%"))
        (expect count :to-be 1))
      (multiple-value-bind (document count) (replace-document (doc "") (replacer "a" "b") nil nil)
        (expect (doc-string document) :to-equal "")
        (expect count :to-be 0))
      (expect (refusal-code (lambda () (replace-document (doc (format nil "a~%")) (replacer "a" (format nil "x~%y")) nil nil)))
              :to-equal "argument.invalid")))

  (it-each (("digits" "86400" 86400)
            ("@digits" "@7" 7)
            ("a date" "1970-01-02" 86400)
            ("a UTC time with a fraction" "1970-01-01T00:00:01.5Z" 1)
            ("a positive zone" "1970-01-01T09:00+09:00" 0)
            ("a negative zone without a colon" "1969-12-31T23:30-0030" 0)
            ("a leap day" "2024-02-29" 1709164800)
            ("an impossible date" "2024-02-30" nil)
            ("an hour past the day" "1970-01-01T24:00" nil)
            ("text" "yesterday" nil)
            ("a lone @" "@" nil))
      "parses --mtime ~A"
      (name text expected)
    (declare (ignore name))
    (expect (parse-mtime text) :to-equal expected))

  (it "prints Unix seconds as ISO 8601 UTC"
    (expect (iso-utc 86401) :to-equal "1970-01-02T00:00:01Z")))

(describe "aitools.edit.domain transform ops: remaining cases"
  (flet ((op (name lines &rest keys) (apply #'transform-lines name lines keys)))
    (it "sorts numbers with signs and fractions, and lines without a number as 0"
      (expect (op "sort-numeric" '("2.5 b" "x" "-0.5 a" "+3 c" ".25 d")) :to-equal '("-0.5 a" "x" ".25 d" "2.5 b" "+3 c")))

    (it "sorts versions with non-numeric parts, a number before a word"
      (expect (op "sort-version" '("v1.b" "v1.10" "v1.a" "v1" "v1.9")) :to-equal '("v1" "v1.9" "v1.10" "v1.a" "v1.b")))

    (it "reverses, lowercases and composes to NFC"
      (expect (op "reverse" '("a" "b")) :to-equal '("b" "a"))
      (expect (op "lower" '("AÉ")) :to-equal '("aé"))
      (expect (op "nfc" (list (coerce (list #\e (code-char #x301)) 'string))) :to-equal (list (string (code-char #xE9)))))

    (it "dedents by tab stops and leaves short lines unwrapped"
      (expect (op "dedent" (list (format nil "~Ca" #\Tab) "        b" "")) :to-equal '("a" "b" ""))
      (expect (op "dedent" '("    a" "  b") :width 2) :to-equal '("  a" "b"))
      (expect (op "wrap" '("short") :columns 10) :to-equal '("short")))

    (it "comments and uncomments with a block marker when the language has no line comment"
      (let ((markdown (aitools.text.domain:language-for-path "x.md")))
        (expect (op "comment" '("  a" "" "  b") :language markdown) :to-equal '("  <!-- a -->" "" "  <!-- b -->"))
        (expect (op "uncomment" '("  <!-- a -->" "<!--b-->" "<!-- open" "plain") :language markdown)
                :to-equal '("  a" "b" "<!-- open" "plain"))))

    (it "signals for an op outside +line-transform-ops+"
      (expect (every (lambda (name) (listp (op name '("1") :seed 1 :language (aitools.text.domain:language-for-path "x.lisp"))))
                     +line-transform-ops+)
              :to-be t)
      (signals error (op "no-such-op" '("a"))))))

(describe "aitools.edit.domain split edge cases"
  (it "cuts an empty file into no pieces"
    (expect (split-by-bytes (bytes "") 4) :to-equal '())
    (expect (split-by-lines (bytes "") 2) :to-equal '())))

(describe "aitools.edit.domain remaining branches of the text rules"
  (it "keeps a final line's terminator when lines are replaced at the end"
    (expect (doc-string (document-replace-lines (doc (format nil "a~%b~%")) 1 2 '("x"))) :to-equal (format nil "a~%x~%"))
    (expect (doc-string (document-with-logical-text (doc (format nil "a~%b")) (format nil "A~%b~%") 0 1))
            :to-equal (format nil "A~%b~%")))

  (it "reads a leading number with a sign inside as 0, and orders a number before a word part"
    (expect (transform-lines "sort-numeric" '("2 b" "1-2 x" "-1 a")) :to-equal '("-1 a" "1-2 x" "2 b"))
    (expect (version< "1" "v1") :to-be t)
    (expect (version< "v1" "1") :to-be nil))

  (it "uncomments a marker with no space after it and leaves a lone block opener"
    (let ((lisp (aitools.text.domain:language-for-path "x.lisp"))
          (markdown (aitools.text.domain:language-for-path "x.md")))
      (expect (transform-lines "uncomment" '(";a" "  ;; b" ";") :language lisp) :to-equal '("a" "  ; b" ""))
      (expect (transform-lines "uncomment" '("<!--") :language markdown) :to-equal '("<!--"))))

  (it "splits an all-capital word as one"
    (expect (apply-filter "kebab" "HTTP") :to-equal "http"))

  (it "parses an empty --mtime as malformed"
    (expect (parse-mtime "") :to-be nil))

  (it "leaves a document alone when --multiline finds nothing"
    (let ((replacer (make-replacer (compile-search-pattern/k "zz" :on-regex #'identity :on-invalid #'error)
                                   (literal-replacement "y"))))
      (multiple-value-bind (document count) (replace-document (doc (format nil "a~%")) replacer nil t)
        (expect (doc-string document) :to-equal (format nil "a~%"))
        (expect count :to-be 0)))))
