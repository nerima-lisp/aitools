(in-package #:aitools.search.test)

(describe "aitools.search.domain edge contracts"
  (it "folds ASCII bytes and finds LF, CR LF, and final line ends"
    (let ((octets (%bytes (format nil "lf~%crlf~C~%tail" #\Return))))
      (expect (funcall (symbol-function 'aitools.search.domain::%ascii-fold) 64) :to-be 64)
      (expect (funcall (symbol-function 'aitools.search.domain::%ascii-fold) 65) :to-be 97)
      (expect (funcall (symbol-function 'aitools.search.domain::%ascii-fold) 90) :to-be 122)
      (expect (funcall (symbol-function 'aitools.search.domain::%ascii-fold) 91) :to-be 91)
      (expect (funcall (symbol-function 'aitools.search.domain::%ascii-fold) 97) :to-be 97)
      (expect (octets-find (%bytes "abc") (%bytes "aBC") 0 :fold t) :to-be 0)
      (expect (funcall (symbol-function 'aitools.search.domain::line-content-end) octets 0) :to-be 2)
      (expect (funcall (symbol-function 'aitools.search.domain::line-content-end) octets 3) :to-be 7)
      (expect (funcall (symbol-function 'aitools.search.domain::line-content-end) octets 9) :to-be 13)
      (expect (funcall (symbol-function 'aitools.search.domain::line-content-end) octets 8) :to-be 8)))

  (it "builds a language index and reports an invalid definition pattern"
    (let* ((language (aitools.text.domain:find-language "common-lisp"))
           (index (aitools.search.domain::%build-language-index language))
           (kinds (aitools.search.domain::language-index-kinds index))
           (names (aitools.search.domain::language-index-name-groups index))
           (levels (aitools.search.domain::language-index-level-groups index)))
      (expect (eq (aitools.search.domain::language-index-language index) language)
              :to-be t)
      (expect (plusp (length kinds)) :to-be t)
      (expect (length names) :to-be (length kinds))
      (expect (length levels) :to-be (length kinds))
      (expect (every #'integerp (coerce names 'list)) :to-be t)
      (expect (every (lambda (group) (or (null group) (integerp group)))
                     (coerce levels 'list))
              :to-be t))
    (let* ((language
             (aitools.text.domain::make-language
              :name "broken"
              :identifier "[a-z]+"
              :definitions '(("function" "["))))
           (message
             (handler-case
                 (progn (aitools.search.domain::%build-language-index language) nil)
               (error (condition) (princ-to-string condition)))))
      (expect (not (null (and message (search "definition pattern 0" message))))
              :to-be t)
      (expect (not (null (and message (search "does not compile" message))))
              :to-be t)))

  (it "returns the requested start when an S-expression opener is absent"
    (expect (aitools.search.domain::%sexp-end (%bytes "text") 2 nil)
            :to-be 2))

  (it-each (("r\"plain\"")
            ("br#\"byte\"#")
            ("r###\"many\"###")
            ("r##\"short\"# then \"full\"##")
            ("r##\"open"))
      "finds a Rust raw-string delimiter or returns the final byte for ~S"
      (literal)
    (let ((octets (%bytes literal)))
      (expect (aitools.search.domain::%rust-raw-string-end octets 0)
              :to-be (1- (length octets)))))
  (it "rejects incomplete Rust prefixes and a near-EOF delimiter with too few hashes"
    (dolist (literal '("b" "bx" "r" "r#" "r##x"))
      (expect (aitools.search.domain::%rust-raw-string-end (%bytes literal) 0)
              :to-be nil))
    (let ((octets (%bytes "r##\"x\"#")))
      (expect (aitools.search.domain::%rust-raw-string-end octets 0)
              :to-be (1- (length octets)))))
  (it "skips Rust raw and byte strings with matching delimiters"
    (expect (outline-of "/w/a.rs"
                        (lines "fn close_raw() {"
                               "  let close = r#\"}\"#;"
                               "  let plain = r\"}\";"
                               "  let bytes = b\"}\";"
                               "  let byte_raw = br##\"}\"##;"
                               "  let shorter = r##\" } \"# } still inside \"##;"
                               "  use_value(close);"
                               "}"
                               "fn later() {}"))
            :to-equal '((1 8 "function" "close_raw") (9 9 "function" "later")))
    (expect (outline-of "/w/a.rs"
                        (lines "fn incomplete_raw() {"
                               "  let value = r##\" } \"#"
                               "  } inside"))
            :to-equal '((1 3 "function" "incomplete_raw"))))

  (it "marks blank lines and both markers inside a Markdown fence"
    (let ((fences
            (aitools.search.domain::%fence-lines
             (%bytes
              (format nil "~C~C~Clisp~%~%# hidden~%~C~C~C~%# visible~%"
                      (code-char 96) (code-char 96) (code-char 96)
                      (code-char 96) (code-char 96) (code-char 96))))))
      (dolist (line '(1 2 3 4))
        (expect (gethash line fences) :to-be t))
      (expect (gethash 5 fences) :to-be nil)))

  (it "returns literal, no-literal, and multiline hits with exact bounds"
    (let* ((octets (%bytes (format nil "x ab~%ab")))
           (program (svref (matcher-programs (matcher '("ab"))) 0))
           (first (aitools.search.domain::%next-match program octets 0 nil))
           (second (aitools.search.domain::%next-match
                    program octets
                    (funcall (symbol-function 'aitools.search.domain::hit-end) first)
                    nil)))
      (expect (funcall (symbol-function 'aitools.search.domain::hit-start) first) :to-be 2)
      (expect (funcall (symbol-function 'aitools.search.domain::hit-end) first) :to-be 4)
      (expect (funcall (symbol-function 'aitools.search.domain::hit-start) second) :to-be 5)
      (expect (funcall (symbol-function 'aitools.search.domain::hit-end) second) :to-be 7)
      (expect (aitools.search.domain::%next-match program octets 7 nil) :to-be nil)
      (expect (aitools.search.domain::%next-match program octets 8 nil) :to-be nil))
    (let* ((octets (%bytes "x"))
           (program (svref (matcher-programs (matcher '("."))) 0))
           (hit (aitools.search.domain::%next-match program octets 0 nil)))
      (expect (funcall (symbol-function 'aitools.search.domain::hit-start) hit) :to-be 0)
      (expect (funcall (symbol-function 'aitools.search.domain::hit-end) hit) :to-be 1))
    (let* ((octets (%bytes "x"))
           (program (svref (matcher-programs (matcher '(".") :multiline t)) 0))
           (hit (aitools.search.domain::%next-match program octets 0 t)))
      (expect (funcall (symbol-function 'aitools.search.domain::hit-start) hit) :to-be 0)
      (expect (funcall (symbol-function 'aitools.search.domain::hit-end) hit) :to-be 1))
    (let* ((octets (%bytes (format nil "a~%b")))
           (program (svref (matcher-programs (matcher '("a\\nb") :multiline t)) 0))
           (hit (aitools.search.domain::%next-match program octets 0 t)))
      (expect (funcall (symbol-function 'aitools.search.domain::hit-start) hit) :to-be 0)
      (expect (funcall (symbol-function 'aitools.search.domain::hit-end) hit) :to-be 3)))

  (it "covers direct, folded, absent, and captured program metadata"
    (let* ((direct (svref (matcher-programs (matcher '("needle"))) 0))
           (folded (svref (matcher-programs (matcher '("SKIP_THIS") :ignore-case t)) 0))
           (absent (svref (matcher-programs (matcher '("a*"))) 0))
           (folded-absent (svref (matcher-programs (matcher '("a*") :ignore-case t)) 0))
           (named (svref (matcher-programs (matcher '("(?<name>needle)"))) 0)))
      (expect (not (null (aitools.search.domain::program-regex direct))) :to-be t)
      (expect (equalp (aitools.search.domain::program-literal direct) (%bytes "needle")) :to-be t)
      (expect (aitools.search.domain::program-fold direct) :to-be nil)
      (expect (aitools.search.domain::program-group-count direct) :to-be 0)
      (expect (equalp (aitools.search.domain::program-literal folded) (%bytes "ip_thi")) :to-be t)
      (expect (aitools.search.domain::program-fold folded) :to-be t)
      (expect (aitools.search.domain::program-literal absent) :to-be nil)
      (expect (aitools.search.domain::program-fold absent) :to-be nil)
      (expect (aitools.search.domain::program-literal folded-absent) :to-be nil)
      (expect (aitools.search.domain::program-fold folded-absent) :to-be nil)
      (expect (aitools.search.domain::program-group-count named) :to-be 1)
      (expect (aref (aitools.search.domain::program-names named) 1) :to-equal "name")
      (multiple-value-bind (literal fold)
          (funcall (symbol-function 'aitools.search.domain::%program-literal)
                   (aitools.search.domain::program-regex direct) "needle" nil nil
                   #'aitools.search.domain::%compile)
        (expect (equalp literal (%bytes "needle")) :to-be t)
        (expect fold :to-be nil))
      (multiple-value-bind (literal fold)
          (funcall (symbol-function 'aitools.search.domain::%program-literal)
                   (aitools.search.domain::program-regex folded) "SKIP_THIS" t nil
                   #'aitools.search.domain::%compile)
        (expect (equalp literal (%bytes "ip_thi")) :to-be t)
        (expect fold :to-be t)))
    (let ((aitools.search.domain:*regex-run-deadline* nil))
      (expect (funcall (symbol-function 'aitools.search.domain::%regex-deadline-passed-p))
              :to-be nil))
    (let ((aitools.search.domain:*regex-run-deadline* -1))
      (expect (funcall (symbol-function 'aitools.search.domain::%regex-deadline-passed-p))
              :to-be t)))

  (it "exercises file outcome variants and found-entry boundary values"
    (let* ((search-matcher (matcher '("needle")))
           (input (%bytes (format nil "needle~%")))
           (empty (search-file search-matcher (%bytes "quiet") :matches 1))
           (matched (search-file search-matcher input :matches 1))
           (unkept (search-file search-matcher input :matches 0))
           (selected (search-file search-matcher input :blocks 1))
           (counted (search-file search-matcher input :count 1)))
      (expect (file-outcome-octets empty) :to-be nil)
      (expect (file-outcome-selected empty) :to-equal '())
      (expect (file-outcome-selected-count empty) :to-be 0)
      (expect (file-outcome-match-count empty) :to-be 0)
      (expect (file-outcome-matches empty) :to-equal '())
      (expect (equalp (file-outcome-octets matched) input) :to-be t)
      (expect (file-outcome-match-count matched) :to-be 1)
      (expect (length (file-outcome-matches matched)) :to-be 1)
      (expect (file-outcome-selected matched) :to-equal '())
      (expect (file-outcome-octets unkept) :to-be nil)
      (expect (file-outcome-match-count unkept) :to-be 1)
      (expect (file-outcome-matches unkept) :to-equal '())
      (expect (equalp (file-outcome-octets selected) input) :to-be t)
      (expect (file-outcome-selected-count selected) :to-be 1)
      (expect (length (file-outcome-selected selected)) :to-be 1)
      (expect (file-outcome-match-count selected) :to-be 0)
      (expect (file-outcome-matches selected) :to-equal '())
      (expect (file-outcome-octets counted) :to-be nil)
      (expect (file-outcome-selected-count counted) :to-be 1)
      (expect (file-outcome-match-count counted) :to-be 0)
      (expect (file-outcome-matches counted) :to-equal '()))
    (let ((directory (make-found-entry "src" :directory nil 0 100))
          (empty-file (make-found-entry "src/empty" :file 0 0 0))
          (file (make-found-entry "src/main.lisp" :file 42 #o644 200)))
      (expect (found-entry-size directory) :to-be nil)
      (expect (found-entry-size empty-file) :to-be 0)
      (expect (found-entry-size file) :to-be 42)
      (expect (found-entry-mode directory) :to-be 0)
      (expect (found-entry-mode empty-file) :to-be 0)
      (expect (found-entry-path directory) :to-equal "src")
      (expect (found-entry-path file) :to-equal "src/main.lisp")
      (expect (jfield (found-entry-json directory) "size") :to-be json-kit:+json-null+)
      (expect (jfield (found-entry-json empty-file) "size") :to-be 0)
      (expect (jfield (found-entry-json file) "size") :to-be 42)))
  (it "rejects invalid domain constructor slot values"
    (signals type-error
      (make-found-entry "src/file" :file 1 (read-from-string "-1") 0))
    (signals type-error
      (aitools.search.domain::%make-program nil (read-from-string "\"literal\"") nil #() 0))
    (signals type-error
      (aitools.search.domain::%make-file-outcome (read-from-string "\"octets\"") '() 0 0 '())))
  (it "returns no prefilter when case-sensitive recompilation fails"
    (let* ((program (svref (matcher-programs (matcher '("a*") :ignore-case t)) 0))
           (regex (aitools.search.domain::program-regex program)))
      (multiple-value-bind (literal fold)
          (funcall (symbol-function 'aitools.search.domain::%program-literal)
                   regex "a*" t nil
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error 'cl-regex-kit:cl-regex-kit-error)))
        (expect literal :to-be nil)
        (expect fold :to-be nil)))))

