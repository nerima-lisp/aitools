;;;; t/unit/edit/domain-test.lisp
;;;;
;;;; The edit context's pure rules: layout preservation, `--old`
;;;; matching, replacement templates and filters, transform ops, and `split`.
;;;; Their edge cases are in domain-edge-test.lisp.
(in-package #:aitools.edit.test)

(describe "aitools.edit.domain text documents"
  (it "round-trips CRLF, a missing final newline, and a BOM byte for byte"
    (dolist (text (list (format nil "a~C~%b~C~%" #\Return #\Return)
                        (format nil "a~%b")
                        (format nil "~Ca~%b~%" (code-char #xFEFF))
                        ""))
      (expect (doc-string (doc text)) :to-equal text)))

  (it "keeps the BOM out of line 1"
    (let ((document (doc (format nil "~Chello~%" (code-char #xFEFF)))))
      (expect (document-line document 0) :to-equal "hello")
      (expect (text-document-bom-p document) :to-be t)))

  (it "refuses binary and invalid UTF-8 input"
    (expect (decode-text-document/k (octet-vector 97 0 98) :on-decoded #'identity
                                                           :on-binary (lambda () :binary)
                                                           :on-invalid (lambda (offset) offset))
            :to-be :binary)
    (expect (decode-text-document/k (octet-vector 97 #xFF 98) :on-decoded #'identity
                                                              :on-binary (lambda () :binary)
                                                              :on-invalid (lambda (offset) offset))
            :to-be 1))

  (it "gives replaced lines the file's line ending and keeps a missing final newline"
    (let ((crlf (doc (format nil "a~C~%b~C~%" #\Return #\Return)))
          (open-end (doc (format nil "a~%b"))))
      (expect (doc-string (document-replace-lines crlf 1 2 '("x" "y")))
              :to-equal (format nil "a~C~%x~C~%y~C~%" #\Return #\Return #\Return))
      (expect (doc-string (document-replace-lines open-end 1 2 '("x"))) :to-equal (format nil "a~%x"))
      (expect (doc-string (document-replace-lines open-end 1 2 '())) :to-equal "a")))

  (it "keeps each untouched line's own terminator in a mixed file"
    (let ((mixed (doc (format nil "a~C~%b~%c~C~%" #\Return #\Return))))
      (expect (doc-string (document-with-lines mixed (vector "a" "B" "c")))
              :to-equal (format nil "a~C~%B~%c~C~%" #\Return #\Return)))))

(describe "aitools.edit.domain --old matching"
  (flet ((edit (text old new)
           (apply-old-edit (doc text) old new
                           :on-edited (lambda (document strategy line) (list (doc-string document) strategy line))
                           :on-ambiguous (lambda (matches) (list :ambiguous (mapcar #'car matches)))
                           :on-no-match (lambda (candidates) (list :no-match (mapcar #'car candidates))))))
    (it "replaces a unique exact match"
      (expect (edit (format nil "one~%two~%") "two" "2") :to-equal (list (format nil "one~%2~%") :exact 2)))

    (it "reports every line of an ambiguous match"
      (expect (edit (format nil "x~%y~%x~%") "x" "z") :to-equal '(:ambiguous (1 3))))

    (it "matches ignoring surrounding whitespace and re-indents the replacement"
      (expect (edit (format nil "(defun f ()~%    (old-call)~%    (more))~%")
                    (format nil "(old-call)~%(more))") (format nil "(new-call)~%  (nested))"))
              :to-equal (list (format nil "(defun f ()~%    (new-call)~%      (nested))~%") :whitespace 2)))

    (it "offers the most similar places when nothing matches"
      (expect (edit (format nil "alpha~%beta~%gamma~%") "betta" "x") :to-equal '(:no-match (2 1 3))))

    (it "matches --old written with LF in a CRLF file and keeps CRLF"
      (expect (first (edit (format nil "a~C~%b~C~%" #\Return #\Return) (format nil "a~%b") (format nil "A~%B")))
              :to-equal (format nil "A~C~%B~C~%" #\Return #\Return)))))

(describe "aitools.edit.domain replacement templates"
  (flet ((expand (template &rest groups)
           (expand-template (parse-replacement-template template)
                            (lambda (designator)
                              (if (integerp designator) (nth designator groups) (getf groups (intern (string-upcase designator) :keyword)))))))
    (it "expands $0, $1, ${1} and $$"
      (expect (expand "[$0|$1|${1}x|$$]" "whole" "one") :to-equal "[whole|one|onex|$]"))

    (it-each (("upper" "${1:upper}" "Mixed Case" "MIXED CASE")
         ("lower" "${1:lower}" "Mixed Case" "mixed case")
         ("capitalize" "${1:capitalize}" "hELLO" "Hello")
         ("snake" "${1:snake}" "parseHTTPRequest" "parse_http_request")
         ("camel" "${1:camel}" "parse-http request" "parseHttpRequest")
         ("kebab" "${1:kebab}" "ParseHttp_Request" "parse-http-request")
         ("trim" "${1:trim}" "  x  " "x")
         ("inc keeps zero padding" "${1:inc}" "007" "008")
         ("inc widens past the padding" "${1:inc}" "99" "100")
         ("dec keeps zero padding" "${1:dec}" "010" "009")
         ("padN" "${1:pad4}" "7" "0007")
         ("chained filters apply left to right" "${1:snake:upper}" "fooBar" "FOO_BAR"))
        "applies filter ~A"
        (name template value expected)
      (declare (ignore name))
      (expect (expand template "whole" value) :to-equal expected))

    (it "rejects an unknown filter and inc on a non-integer"
      (expect (refusal-code (lambda () (parse-replacement-template "${1:shout}"))) :to-equal "argument.invalid")
      (expect (refusal-code (lambda () (expand "${1:inc}" "whole" "v1"))) :to-equal "argument.invalid"))

    (it "finds perl \\N references only for groups the pattern has"
      (expect (perl-backreferences "\\1 and \\2 and \\9" 2) :to-equal '(1 2))
      (expect (perl-backreferences "\\1" 0) :to-equal '())
      (expect (rewrite-perl-backreferences "a\\1b") :to-equal "a${1}b"))))

(describe "aitools.edit.domain transform ops"
  (flet ((op (name lines &rest keys) (apply #'transform-lines name lines keys)))
    (it "sorts plainly, numerically and by version"
      (expect (op "sort" '("b" "a" "c")) :to-equal '("a" "b" "c"))
      (expect (op "sort-numeric" '("10 x" "9 y" "-1 z")) :to-equal '("-1 z" "9 y" "10 x"))
      (expect (op "sort-version" '("1.10" "1.9" "1.2")) :to-equal '("1.2" "1.9" "1.10")))

    (it "sorts by --key and --delimiter"
      (let ((comma (compile-search-pattern/k "," :on-regex #'identity :on-invalid #'error)))
        (expect (op "sort" '("x,3" "y,1" "z,2") :key 2 :delimiter comma) :to-equal '("y,1" "z,2" "x,3"))))

    (it "keeps the first occurrence for unique and squeezes blank runs"
      (expect (op "unique" '("b" "a" "b" "c" "a")) :to-equal '("b" "a" "c"))
      (expect (op "squeeze-blank" '("a" "" "" "b" "")) :to-equal '("a" "" "b" ""))
      (expect (op "delete-blank" '("a" " " "b")) :to-equal '("a" "b")))

    (it "shuffles the same way for the same seed"
      (let ((lines (loop for i below 20 collect (format nil "~D" i))))
        (expect (op "shuffle" lines :seed 42) :to-equal (op "shuffle" lines :seed 42))
        (expect-not (op "shuffle" lines :seed 42) :to-equal lines)
        (expect (sort (copy-list (op "shuffle" lines :seed 7)) #'string<) :to-equal (sort (copy-list lines) #'string<))))

    (it "handles whitespace ops"
      (expect (op "strip-trailing" '("a  " "b	")) :to-equal '("a" "b"))
      (expect (op "indent" '("a" "" "b") :width 4) :to-equal '("    a" "" "    b"))
      (expect (op "dedent" '("    a" "      b")) :to-equal '("a" "  b"))
      (expect (op "tabs-to-spaces" (list (format nil "~Ca" #\Tab)) :width 4) :to-equal '("    a"))
      (expect (op "spaces-to-tabs" '("        a") :width 4) :to-equal (list (format nil "~C~Ca" #\Tab #\Tab))))

    (it "changes case and normalizes Unicode"
      (expect (op "upper" '("aé")) :to-equal '("AÉ"))
      (expect (op "nfkc" (list (coerce (list (code-char #xFF21) (code-char #x30AB) (code-char #x3099)) 'string)))
              :to-equal (list (coerce (list #\A (code-char #x30AC)) 'string))))

    (it "wraps and reflows prose but leaves code fences alone"
      (expect (op "wrap" '("one two three four") :columns 9) :to-equal '("one two" "three" "four"))
      (expect (op "reflow" '("a b" "c d" "" "```" "x y z long code line" "```") :columns 5)
              :to-equal '("a b c" "d" "" "```" "x y z long code line" "```"))
      (expect (op "wrap" '("```" "a very long line inside a fence" "```") :columns 5)
              :to-equal '("```" "a very long line inside a fence" "```")))

    (it "comments and uncomments with the language's marker"
      (let ((lisp (aitools.text.domain:language-for-path "x.lisp")))
        (expect (op "comment" '("  (a)" "" "  (b)") :language lisp) :to-equal '("  ; (a)" "" "  ; (b)"))
        (expect (op "uncomment" '("  ; (a)" "  (b)") :language lisp) :to-equal '("  (a)" "  (b)"))))))

(describe "aitools.edit.domain split"
  (it "cuts by lines, by bytes and before matching lines"
    (let* ((octets (bytes (format nil "h~%a~%b~%h~%c~%")))
           (document (doc (format nil "h~%a~%b~%h~%c~%"))))
      (expect (mapcar #'split-piece-start-line (split-by-lines octets 2)) :to-equal '(1 3 5))
      (expect (mapcar (lambda (piece) (- (split-piece-end piece) (split-piece-start piece))) (split-by-bytes octets 4))
              :to-equal '(4 4 2))
      (let ((regex (compile-search-pattern/k "^h" :on-regex #'identity :on-invalid #'error)))
        (expect (mapcar (lambda (piece) (list (split-piece-start-line piece) (split-piece-lines piece)))
                        (split-at-matches octets (text-document-lines document) regex))
                :to-equal '((1 3) (4 2)))))))
