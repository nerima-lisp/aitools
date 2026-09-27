;;;; t/unit/search/search-flow-test.lisp
;;;;
;;;; `search` through the flow, over the in-memory filesystem, with the
;;;; ignore-rule and regex cases. Standard
;;;; input, patterns, next commands and scan options are in
;;;; search-flow-options-test.lisp, which uses the helpers defined here.
(in-package #:aitools.search.test)

(defun numbered-lines (count &key (match-every 0) (prefix "line"))
  "COUNT lines `<prefix> N`; every MATCH-EVERY-th also holds `hit`."
  (with-output-to-string (out)
    (loop for n from 1 to count
          do (format out "~A ~D~:[~; hit~]~%" prefix n (and (plusp match-every) (zerop (mod n match-every)))))))

(defun search-in (files &rest arguments)
  (apply #'run-flow #'search/k (make-fake-ports :files files) arguments))

(describe "aitools.search.application search/k blocks"
  (it "merges matches whose context touches into one block and keeps separate ones apart"
    (multiple-value-bind (kind fields)
        (search-in (list (list "/w/a.txt" (numbered-lines 20 :match-every 0)))
                   :patterns '("line (3|5|15)$") :context 1)
      (expect kind :to-be :ok)
      (let ((blocks (field fields "blocks")))
        (expect (length blocks) :to-be 2)
        (expect (jfield (first blocks) "start_line") :to-be 2)
        (expect (jfield (first blocks) "lines") :to-equal '("line 2" "line 3" "line 4" "line 5" "line 6"))
        (expect (jfield (first blocks) "match_lines") :to-equal '(3 5))
        (expect (jfield (second blocks) "match_lines") :to-equal '(15)))
      (expect (field fields "total_matches") :to-be 3)
      (expect (field fields "files_scanned") :to-be 1)
      (expect (field fields "mode") :to-equal "blocks")))

  (it "lets --before and --after override --context"
    (multiple-value-bind (kind fields)
        (search-in (list (list "/w/a.txt" (numbered-lines 10))) :patterns '("line 5$") :context 3 :before 0 :after 1)
      (expect kind :to-be :ok)
      (let ((block (first (field fields "blocks"))))
        (expect (jfield block "start_line") :to-be 5)
        (expect (jfield block "lines") :to-equal '("line 5" "line 6")))))

  (it "is partial past --limit with an exact total_matches and a next command"
    (multiple-value-bind (kind fields)
        (search-in (list (list "/w/a.txt" (numbered-lines 100 :match-every 2))
                         (list "/w/b.txt" (numbered-lines 10 :match-every 1)))
                   :patterns '("hit") :limit 3 :context 0)
      (expect kind :to-be :partial)
      (expect (field fields "total_matches") :to-be 60)
      (expect (loop for block in (field fields "blocks") sum (length (jfield block "match_lines"))) :to-be 3)
      (expect (false-p (field fields "truncated")) :to-be nil)
      (expect (first (field fields "next_commands")) :to-contain "--limit 60")))

  (it "reports files in path order whatever order the filesystem lists them in"
    (multiple-value-bind (kind fields)
        (search-in (list (list "/w/z.txt" "hit") (list "/w/a/b.txt" "hit") (list "/w/a.txt" "hit") (list "/w/m.txt" "hit"))
                   :patterns '("hit"))
      (expect kind :to-be :ok)
      (expect (mapcar (lambda (block) (jfield block "path")) (field fields "blocks"))
              :to-equal '("a.txt" "a/b.txt" "m.txt" "z.txt"))))

  (it "produces byte-identical output for the same input"
    (let ((files (list (list "/w/a.txt" (numbered-lines 30 :match-every 3)) (list "/w/b.txt" (numbered-lines 5 :match-every 2)))))
      (expect (multiple-value-call #'rendered (search-in files :patterns '("hit")))
              :to-equal (multiple-value-call #'rendered (search-in files :patterns '("hit")))))))

(describe "aitools.search.application search/k output modes"
  (let ((files (list (list "/w/a.txt" (format nil "id=12 name=ab~%none~%id=7~%"))
                     (list "/w/b.txt" (format nil "nothing~%")))))
    (it "matches: returns each match with col, groups, named, and null for an unmatched group"
      (multiple-value-bind (kind fields)
          (search-in (list (list "/w/a.txt" (format nil "~Cx id=12~%id=7 q~%" (code-char #x3042))))
                     :patterns '("id=(?<num>[0-9]+)( q)?") :output :matches)
        (expect kind :to-be :ok)
        (let ((matches (field fields "matches")))
          (expect (length matches) :to-be 2)
          (expect (jfield (first matches) "col") :to-be 4)
          (expect (jfield (first matches) "text") :to-equal "id=12")
          (expect (first (jfield (first matches) "groups")) :to-equal "12")
          (expect (second (jfield (first matches) "groups")) :to-be json-kit:+json-null+)
          (expect (json-alist-value (jfield (first matches) "named") "num") :to-equal "12")
          (expect (jfield (second matches) "groups") :to-equal '("7" " q"))
          (expect (jfield (first matches) "pattern_index") :to-be nil))))

    (it "matches: --limit bounds matches, not lines"
      (multiple-value-bind (kind fields)
          (search-in (list (list "/w/a.txt" "a a a a")) :patterns '("a") :output :matches :limit 2)
        (expect kind :to-be :partial)
        (expect (length (field fields "matches")) :to-be 2)
        (expect (field fields "total_matches") :to-be 4)))

    (it "count: per-file selected lines"
      (multiple-value-bind (kind fields) (search-in files :patterns '("id=") :output :count)
        (expect kind :to-be :ok)
        (expect (mapcar (lambda (entry) (list (jfield entry "path") (jfield entry "count"))) (field fields "counts"))
                :to-equal '(("a.txt" 2)))
        (expect (field fields "total") :to-be 1)))

    (it "files and files-without-match: paths with and without a selected line"
      (expect (field (nth-value 1 (search-in files :patterns '("id=") :output :files)) "paths") :to-equal '("a.txt"))
      (expect (field (nth-value 1 (search-in files :patterns '("id=") :output :files-without-match)) "paths")
              :to-equal '("b.txt")))

    (it "--invert selects the lines without a match"
      (multiple-value-bind (kind fields) (search-in files :patterns '("id=") :invert t :context 0)
        (expect kind :to-be :ok)
        (expect (mapcar (lambda (block) (jfield block "lines")) (field fields "blocks"))
                :to-equal '(("none") ("nothing")))))

    (it "rejects --output matches with --invert"
      (expect (getf (nth-value 1 (search-in files :patterns '("id=") :invert t :output :matches)) :code)
              :to-equal "argument.invalid"))

    (it "--multiline matches across lines"
      (multiple-value-bind (kind fields) (search-in files :patterns '("none\\nid") :multiline t :context 0)
        (expect kind :to-be :ok)
        (expect (jfield (first (field fields "blocks")) "match_lines") :to-equal '(2 3))))

    (it "--line-regexp matches whole lines only"
      (expect (field (nth-value 1 (search-in files :patterns '("id=7") :line-regexp t)) "total_matches") :to-be 1)
      (expect (field (nth-value 1 (search-in files :patterns '("id=1") :line-regexp t)) "total_matches") :to-be 0))

    (it "repeated --pattern selects lines matching any, with pattern_index on matches"
      (multiple-value-bind (kind fields) (search-in files :patterns '("name" "none") :output :matches)
        (expect kind :to-be :ok)
        (expect (mapcar (lambda (match) (list (jfield match "line") (jfield match "pattern_index"))) (field fields "matches"))
                :to-equal '((1 0) (2 1)))))))

(describe "aitools.search.application search/k errors, input, and scanning"
  (it "returns input.syntax-error (exit 1) with a --fixed repair for a bad regex"
    (multiple-value-bind (kind fields) (search-in (list (list "/w/a.txt" "x")) :patterns '("a("))
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "input.syntax-error")
      (expect (aitools.protocol.domain:error-code-exit-code (getf fields :code)) :to-be 1)
      (expect (getf (first (getf fields :repairs)) :command) :to-contain "--fixed")))

  (it "reads the pattern from standard input only with --stdin, dropping one trailing newline"
    (multiple-value-bind (kind fields)
        (run-flow #'search/k (make-fake-ports :files (list (list "/w/a.txt" (format nil "x $1 \\ y~%")))
                                              :stdin (format nil "$1 \\~%"))
                  :stdin t :fixed t)
      (expect kind :to-be :ok)
      (expect (field fields "total_matches") :to-be 1)))

  (it "lists binary files in skipped instead of searching them"
    (multiple-value-bind (kind fields)
        (search-in (list (list "/w/bin.dat" (coerce #(104 105 116 0 1) '(vector (unsigned-byte 8)))) (list "/w/t.txt" "hit"))
                   :patterns '("hit"))
      (expect kind :to-be :ok)
      (expect (json-alist-value (first (field fields "skipped")) "reason") :to-equal "binary")
      (expect (field fields "files_scanned") :to-be 1)))

  (it "applies .gitignore in a repository, reports ignore_source, and --no-ignore lifts it"
    (let ((files (list (list "/w/.git/HEAD" (format nil "ref: refs/heads/main~%"))
                       (list "/w/.gitignore" (format nil "out/~%"))
                       (list "/w/out/gen.txt" "hit")
                       (list "/w/src.txt" "hit"))))
      (multiple-value-bind (kind fields) (search-in files :patterns '("hit"))
        (expect kind :to-be :ok)
        (expect (field fields "ignore_source") :to-equal "gitignore")
        (expect (mapcar (lambda (block) (jfield block "path")) (field fields "blocks")) :to-equal '("src.txt")))
      (multiple-value-bind (kind fields) (search-in files :patterns '("hit") :no-ignore t)
        (expect kind :to-be :ok)
        (expect (field fields "ignore_source") :to-equal "none")
        (expect (mapcar (lambda (block) (jfield block "path")) (field fields "blocks"))
                :to-equal '("out/gen.txt" "src.txt")))))

  (it "honours --glob, --skip-larger-than, and --newer"
    (let ((files (list (list "/w/a.lisp" "hit" :mtime 5000) (list "/w/b.txt" "hit" :mtime 100)
                       (list "/w/big.lisp" (format nil "hit~A" (make-string 2000 :initial-element #\x))))))
      (expect (field (nth-value 1 (search-in files :patterns '("hit") :glob '("*.lisp") :skip-larger-than "1KiB"))
                     "total_matches")
              :to-be 1)
      (expect (json-alist-value (first (field (nth-value 1 (search-in files :patterns '("hit") :skip-larger-than "1KiB"))
                                              "skipped"))
                                "reason")
              :to-equal "too-large")
      (expect (field (nth-value 1 (search-in files :patterns '("hit") :newer "/w/b.txt")) "total_matches") :to-be 2)))

  (it "skips a file on which the bounded advanced executor runs out of steps, and searches the rest"
    (multiple-value-bind (kind fields)
        (search-in (list (list "/w/a.txt" (make-string 40 :initial-element #\a)) (list "/w/b.txt" "ab"))
                   :patterns '("(a+)+(?=[bc])"))
      (expect kind :to-be :ok)
      (expect (mapcar (lambda (entry) (list (json-alist-value entry "path") (json-alist-value entry "reason")))
                      (field fields "skipped"))
              :to-equal '(("a.txt" "regex-limit")))
      (expect (field fields "total_matches") :to-be 1)))

  (it "reports a start path that does not exist as input.not-found"
    (expect (getf (nth-value 1 (search-in (list (list "/w/a.txt" "x")) :patterns '("x") :paths '("nope"))) :code)
            :to-equal "input.not-found"))

  (it "masks secrets in the returned lines of the written envelope"
    (let ((text (multiple-value-call #'rendered
                  (search-in (list (list "/w/a.txt" (format nil "token ghp_abcdefghijklmnopqrstuvwxyz0123456789~%")))
                             :patterns '("token")))))
      (expect text :to-contain "token [REDACTED_SECRET]")
      (expect text :to-contain "\"redactions\":1")
      (expect (search "ghp_" text) :to-be nil)))

  (it "skips a file as regex-limit once the run's cumulative regex budget is spent"
    ;; The per-call step budget resets per line; a zero run budget stands in
    ;; for a cumulative overrun and must stop the file, not the whole command.
    (let ((aitools.search.application::*search-regex-budget-seconds* 0))
      (multiple-value-bind (kind fields)
          (search-in (list (list "/w/a.txt" "hit here")) :patterns '("hit"))
        (expect kind :to-be :ok)
        (expect (mapcar (lambda (entry) (list (json-alist-value entry "path") (json-alist-value entry "reason")))
                        (field fields "skipped"))
                :to-equal '(("a.txt" "regex-limit")))
        (expect (field fields "total_matches") :to-be 0)))))
