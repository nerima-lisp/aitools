;;;; t/unit/search/matcher-test.lisp
;;;;
;;;; The byte-level matcher (regex semantics, literal prefilter): which
;;;; lines are selected, and where matches start, for single files.
(in-package #:aitools.search.test)

(defun matcher (patterns &rest options)
  (apply #'build-matcher/k patterns
         :on-built #'identity
         :on-syntax-error (lambda (index pattern message)
                            (fail (format nil "pattern ~D ~S rejected: ~A" index pattern message)))
         options))

(defun selected-lines (patterns text &rest options)
  "Line numbers the matcher selects in TEXT."
  (let ((outcome (search-file (apply #'matcher patterns options) (%bytes text) :blocks 1000)))
    (mapcar #'aitools.search.domain::selected-line-number (file-outcome-selected outcome))))

(defun match-spans (patterns text &rest options)
  "(pattern-index line start end) for every match in TEXT."
  (let ((outcome (search-file (apply #'matcher patterns options) (%bytes text) :matches 1000)))
    (mapcar (lambda (match)
              (list (aitools.search.domain::found-match-pattern-index match)
                    (aitools.search.domain::found-match-line match)
                    (aitools.search.domain::found-match-start match)
                    (aitools.search.domain::found-match-end match)))
            (file-outcome-matches outcome))))

(describe "aitools.search.domain matcher: regex semantics"
  (it "matches `.` against one non-ASCII character, not one byte"
    (expect (selected-lines '("^a.b$") (format nil "a~Cb~%a~C~Cb~%" (code-char #x3042) #\x #\y)) :to-equal '(1)))

  (it "keeps a UTF-8 BOM out of `^` on the first line"
    (let ((text (concatenate '(vector (unsigned-byte 8)) #(#xEF #xBB #xBF) (string-bytes (format nil "abc~%abc~%")))))
      (expect (selected-lines '("^abc$") text) :to-equal '(1 2))))

  (it "matches `$` before CR LF and never returns the CR"
    (let ((spans (match-spans '("b[^x]*") (format nil "ab~C~%ab~%" #\Return))))
      (expect (mapcar #'fourth spans) :to-equal '(2 6))))

  (it "does not let a match cross a line end without --multiline"
    (expect (selected-lines '("a\\sb") (format nil "a~%b~%a b~%")) :to-equal '(3)))

  (it "lets --multiline matches span lines and selects every spanned line"
    (expect (selected-lines '("a\\nb") (format nil "x~%a~%b~%y~%") :multiline t) :to-equal '(2 3)))

  (it "selects the complement with --invert"
    (expect (selected-lines '("x") (format nil "x~%y~%x~%z") :invert t) :to-equal '(2 4)))

  (it "restricts --word to whole words and --line-regexp to whole lines"
    (expect (selected-lines '("cat") (format nil "cat~%concat~%cat food~%") :word t) :to-equal '(1 3))
    (expect (selected-lines '("cat") (format nil "cat~%concat~%cat food~%") :line-regexp t) :to-equal '(1)))

  (it "treats --fixed patterns literally"
    (expect (selected-lines '("a.b") (format nil "axb~%a.b~%") :fixed t) :to-equal '(2)))

  (it "selects nothing past the final newline, even for an empty pattern"
    (expect (selected-lines '("") (format nil "a~%b~%")) :to-equal '(1 2))
    (expect (selected-lines '("") "") :to-equal '()))

  (it "merges several patterns leftmost-first and tags each match with its pattern"
    (expect (match-spans '("b" "a") "ab ba") :to-equal '((1 1 0 1) (0 1 1 2) (0 1 3 4) (1 1 4 5)))))

(describe "aitools.search.domain matcher: literal prefilter"
  (it "derives the required literal from the pattern through cl-regex-kit"
    (let ((program (svref (matcher-programs (matcher '("foo[0-9]+bar"))) 0)))
      (expect (map 'string #'code-char (aitools.search.domain::program-literal program)) :to-equal "foo")))

  (it "searches a case-folded literal under --ignore-case, avoiding k and s"
    (let ((program (svref (matcher-programs (matcher '("SKIP_THIS") :ignore-case t)) 0)))
      (expect (map 'string #'code-char (aitools.search.domain::program-literal program)) :to-equal "ip_thi")
      (expect (aitools.search.domain::program-fold program) :to-be t))
    (expect (selected-lines '("needle") (format nil "a NEEDLE~%no~%NeEdLe~%") :ignore-case t) :to-equal '(1 3)))

  (it "still finds matches the literal search would miss under Unicode folding"
    ;; U+212A KELVIN SIGN folds to k; k is excluded from the folded literal.
    (expect (selected-lines '("kit") (format nil "~Cit~%" (code-char #x212A)) :ignore-case t) :to-equal '(1)))

  (it "finds matches that start before the literal on the same line"
    (expect (selected-lines '("[0-9]+needle") (format nil "x~%12needle~%needle~%")) :to-equal '(2))))

(defun %within-seconds (seconds thunk)
  "THUNK's value, or :TIMEOUT when it has not returned after SECONDS, so a
walk that never ends fails the spec instead of hanging the suite."
  (let ((thread (sb-thread:make-thread thunk :name "matcher-test watchdog")))
    (let ((value (sb-thread:join-thread thread :timeout seconds :default :timeout)))
      (when (eq value :timeout) (sb-thread:terminate-thread thread))
      value)))

(describe "aitools.search.domain matcher: empty matches and line ends"
  ;; A pattern that can match the empty string finds a match at the buffer
  ;; end of a file whose last line has no LF; the walk must end there.
  (it-each (("x*" "ab" ((0 1 0 0) (0 1 1 1) (0 1 2 2)))
            ("a*" "baaa" ((0 1 0 0) (0 1 1 4)))
            ("x*" "aé" ((0 1 0 0) (0 1 1 1) (0 1 3 3)))
            ("x*" "" ()))
      "reports each empty match of ~S in ~S once and ends at the buffer end"
      (pattern text expected)
    (expect (%within-seconds 5 (lambda () (match-spans (list pattern) text))) :to-equal expected))

  (it-each (("" "ab" (1)) ("b?" "ab" (1)) ("" "a
b" (1 2)))
      "selects each line once for ~S in ~S"
      (pattern text expected)
    (expect (%within-seconds 5 (lambda () (selected-lines (list pattern) text))) :to-equal expected))

  (it "never reports the CR of a CR LF as a match of a pattern without a required literal"
    (expect (match-spans '("\\r|\\t") (format nil "a~C~%b~Cc" #\Return #\Tab)) :to-equal '((0 2 4 5)))))

(describe "aitools.search.domain matcher: folded literal choice"
  (it-each (("abcKdefgh" "defgh") ("abcdeKfg" "abcde") ("abé1234" "1234"))
      "under --ignore-case, prefilters ~S by its longest fold-safe run ~S"
      (pattern literal)
    (let ((program (svref (matcher-programs (matcher (list pattern) :ignore-case t)) 0)))
      (expect (map 'string #'code-char (aitools.search.domain::program-literal program)) :to-equal literal)
      (expect (aitools.search.domain::program-fold program) :to-be t))))

(describe "aitools.search.domain octets-find"
  (it-each (("" "abc" 1 1) ("" "abc" 3 3) ("" "abc" 4 nil) ("bc" "abc" 2 nil) ("c" "abc" 0 2))
      "finds ~S in ~S from ~D at ~S"
      (needle haystack start expected)
    (expect (octets-find (%bytes needle) (%bytes haystack) start) :to-be expected))

  (it "folds ASCII case when the needle starts with a non-letter"
    (expect (octets-find (%bytes "_x") (%bytes "A_X") 0 :fold t) :to-be 1)
    (expect (octets-find (%bytes "_x") (%bytes "A_Y") 0 :fold t) :to-be nil)))
