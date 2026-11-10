(in-package #:aitools.search.test)

(describe "aitools.search.application overview/k scanning"
  (it "is partial past --limit with a next command carrying the path, and skips a file that fails to read"
    (multiple-value-bind (kind fields)
        (run-flow #'overview/k (make-fake-search-boundaries :files '(("/w/src/a.lisp" "x") ("/w/src/b.py" "y") ("/w/src/c.py" "z"))
                                                :failing '("/w/src/c.py"))
                  :path "src" :limit 1)
      (expect kind :to-be :partial)
      (expect (mapcar (lambda (row) (list (jfield row "lang") (jfield row "files"))) (field fields "languages"))
              :to-equal '(("common-lisp" 1)))
      (expect (field fields "languages_total") :to-be 2)
      (expect (length (field fields "entries")) :to-be 3)
      (expect (field fields "next_commands") :to-equal '("aitools overview src --limit 2"))))

  (it "reports a path that does not exist as input.not-found"
    (expect (getf (nth-value 1 (run-flow #'overview/k (make-fake-search-boundaries :directories '("/w")) :path "nope")) :code) :to-equal "input.not-found")))

(describe "aitools.search.domain overview parts"
  (it "orders tally rows by lines, then bytes, then name"
    (let ((tally (make-language-tally)))
      (tally-file tally "b" 10 5)
      (tally-file tally "a" 10 5)
      (tally-file tally "c" 10 9)
      (tally-file tally "d" 20 1)
      (expect (mapcar #'first (aitools.search.domain::language-tally-rows tally)) :to-equal '("d" "c" "a" "b")))))
