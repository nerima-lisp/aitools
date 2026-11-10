(in-package #:aitools.search.test)

(describe "aitools.search.domain edge contracts"
  (it "folds ASCII uppercase bytes and finds LF, CR LF, and final line ends"
    (let ((octets (%bytes (format nil "lf~%crlf~C~%tail" #\Return))))
      (expect (aitools.search.domain::%ascii-fold 64) :to-be 64)
      (expect (aitools.search.domain::%ascii-fold 65) :to-be 97)
      (expect (aitools.search.domain::%ascii-fold 90) :to-be 122)
      (expect (aitools.search.domain::%ascii-fold 91) :to-be 91)
      (expect (aitools.search.domain::%ascii-fold 97) :to-be 97)
      (expect (aitools.search.domain::line-content-end octets 0) :to-be 2)
      (expect (aitools.search.domain::line-content-end octets 3) :to-be 7)
      (expect (aitools.search.domain::line-content-end octets 9) :to-be 13)))

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
  (it "rejects text that does not start a Rust raw string"
    (expect (aitools.search.domain::%rust-raw-string-end (%bytes "r##x") 0)
            :to-be nil)
    (expect (aitools.search.domain::%rust-raw-string-end (%bytes "x\"no\"") 0)
            :to-be nil))

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

  (it "returns bounded literal hits and multiline hits from the requested position"
    (let* ((octets (%bytes (format nil "x ab~%ab")))
           (program (svref (matcher-programs (matcher '("ab"))) 0))
           (first (aitools.search.domain::%next-match program octets 0 nil))
           (second (aitools.search.domain::%next-match
                    program octets (aitools.search.domain::hit-end first) nil)))
      (expect (aitools.search.domain::hit-start first) :to-be 2)
      (expect (aitools.search.domain::hit-end first) :to-be 4)
      (expect (aitools.search.domain::hit-start second) :to-be 5)
      (expect (aitools.search.domain::hit-end second) :to-be 7)
      (expect (aitools.search.domain::%next-match program octets 7 nil) :to-be nil))
    (let* ((octets (%bytes (format nil "a~%b")))
           (program (svref (matcher-programs (matcher '("a\\nb") :multiline t)) 0))
           (hit (aitools.search.domain::%next-match program octets 0 t)))
      (expect (aitools.search.domain::hit-start hit) :to-be 0)
      (expect (aitools.search.domain::hit-end hit) :to-be 3)))

  (it "constructs programs with direct, folded, and absent literals"
    (let* ((direct (svref (matcher-programs (matcher '("needle"))) 0))
           (folded (svref (matcher-programs (matcher '("SKIP_THIS") :ignore-case t)) 0))
           (absent (svref (matcher-programs (matcher '("a*"))) 0)))
      (expect (not (null (aitools.search.domain::program-regex direct))) :to-be t)
      (expect (equalp (aitools.search.domain::program-literal direct) (%bytes "needle")) :to-be t)
      (expect (aitools.search.domain::program-fold direct) :to-be nil)
      (expect (aitools.search.domain::program-group-count direct) :to-be 0)
      (expect (equalp (aitools.search.domain::program-literal folded) (%bytes "ip_thi")) :to-be t)
      (expect (aitools.search.domain::program-fold folded) :to-be t)
      (expect (aitools.search.domain::program-literal absent) :to-be nil)
      (expect (aitools.search.domain::program-fold absent) :to-be nil))
    (let ((aitools.search.domain:*regex-run-deadline* nil))
      (expect (aitools.search.domain::%regex-deadline-passed-p) :to-be nil))
    (let ((aitools.search.domain:*regex-run-deadline* -1))
      (expect (aitools.search.domain::%regex-deadline-passed-p) :to-be t)))

  (it "builds empty, matched, and selected file outcomes plus both entry sizes"
    (let* ((search-matcher (matcher '("needle")))
           (empty (search-file search-matcher (%bytes "quiet") :matches 1))
           (matched (search-file search-matcher (%bytes (format nil "needle~%")) :matches 1))
           (selected (search-file search-matcher (%bytes (format nil "needle~%")) :blocks 1)))
      (expect (file-outcome-octets empty) :to-be nil)
      (expect (file-outcome-match-count empty) :to-be 0)
      (expect (file-outcome-matches empty) :to-equal '())
      (expect (equalp (file-outcome-octets matched) (%bytes (format nil "needle~%"))) :to-be t)
      (expect (file-outcome-match-count matched) :to-be 1)
      (expect (length (file-outcome-matches matched)) :to-be 1)
      (expect (file-outcome-selected-count selected) :to-be 1)
      (expect (length (file-outcome-selected selected)) :to-be 1)
      (expect (file-outcome-match-count selected) :to-be 0))
    (let ((directory (make-found-entry "src" :directory nil #o755 100))
          (file (make-found-entry "src/main.lisp" :file 42 #o644 200)))
      (expect (found-entry-size directory) :to-be nil)
      (expect (found-entry-size file) :to-be 42)
      (expect (found-entry-path directory) :to-equal "src")
      (expect (found-entry-path file) :to-equal "src/main.lisp"))))

