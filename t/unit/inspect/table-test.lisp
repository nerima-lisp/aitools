;;;; t/unit/inspect/table-test.lisp
;;;;
;;;; `table read` and `table agg` (each format, column
;;;; type inference, each --where operator; grouping, each aggregate,
;;;; --min-count, rejection of non-numeric values).
;;;; Option validation, detection, typing, and row-shape cases are in
;;;; table-edge-test.lisp.
(in-package #:aitools.inspect.test)

(defun table-read (files path &rest options)
  (apply #'run-flow #'table-read-flow (make-test-ports :files files) path options))

(defun table-agg (files path &rest options)
  (apply #'run-flow #'table-agg-flow (make-test-ports :files files) path options))

(defun column-summary (fields)
  (mapcar (lambda (column) (list (json-object-get column "name") (json-object-get column "type")))
          (field fields "columns")))

(defparameter *people-csv* "name,age,city,active
\"Smith, J\",42,Tokyo,true
Lee,7,Osaka,false
\"say \"\"hi\"\"\",,Tokyo,true
")

(describe "table read formats"
  (it "reads csv with quoting, a detected header, and typed columns"
    (multiple-value-bind (kind fields) (table-read `(("/work/p.csv" . ,*people-csv*)) "p.csv")
      (expect kind :to-be :ok)
      (expect (field fields "format") :to-equal "csv")
      (expect (column-summary fields) :to-equal '(("name" "string") ("age" "integer") ("city" "string") ("active" "boolean")))
      (expect (first (first (field fields "rows"))) :to-equal "Smith, J")
      (expect (first (third (field fields "rows"))) :to-equal "say \"hi\"")
      (expect (second (first (field fields "rows"))) :to-be 42)
      (expect (json-null-value-p (second (third (field fields "rows")))) :to-be t)
      (expect (field fields "total_rows") :to-be 3)))

  (it "reads tsv, jsonl, and json under a pointer"
    (expect (column-summary (nth-value 1 (table-read '(("/work/t.tsv" . "k	v
a	1.5
b	2
")) "t.tsv")))
            :to-equal '(("k" "string") ("v" "number")))
    (let ((fields (nth-value 1 (table-read '(("/work/r.jsonl" . "{\"a\": 1, \"b\": \"x\"}
{\"a\": 2, \"c\": true}
")) "r.jsonl"))))
      (expect (column-summary fields) :to-equal '(("a" "integer") ("b" "string") ("c" "boolean")))
      (expect (json-null-value-p (second (second (field fields "rows")))) :to-be t))
    (let ((fields (nth-value 1 (table-read '(("/work/d.json" . "{\"rows\": [{\"n\": 1}, {\"n\": 2}]}")) "d.json"
                                           :pointer "/rows"))))
      (expect (field fields "rows") :to-equal '((1) (2)))))

  (it "reads ws with awk semantics; --ws-columns keeps the rest; sep with a delimiter"
    ;; awk semantics: each line splits on whitespace runs independently, not
    ;; folding extra fields into the last column of a fixed width.
    (expect (field (nth-value 1 (table-read '(("/work/ps.txt" . "  1 root  sleep
  22 user vim
")) "ps.txt" :format "ws" :no-header t)) "rows")
            :to-equal '((1 "root" "sleep") (22 "user" "vim")))
    (expect (field (nth-value 1 (table-read '(("/work/one.txt" . "1 a b c
")) "one.txt" :format "ws" :no-header t)) "rows")
            :to-equal '((1 "a" "b" "c")))
    ;; --ws-columns N: at most N fields, the last keeping the rest of the line.
    (expect (field (nth-value 1 (table-read '(("/work/ps.txt" . "  1 root  sleep
  22 user vim a  b
")) "ps.txt" :format "ws" :no-header t :ws-columns 3)) "rows")
            :to-equal '((1 "root" "sleep") (22 "user" "vim a  b")))
    ;; --ws-columns applies to --format ws only.
    (expect (error-code (nth-value 1 (table-read '(("/work/x.csv" . "a,b
")) "x.csv" :ws-columns 2)))
            :to-equal "argument.invalid")
    (let ((fields (nth-value 1 (table-read '(("/work/passwd" . "root:x:0:0
daemon:x:1:1
")) "passwd" :format "sep" :delimiter ":" :no-header t :columns '("1")))))
      (expect (field fields "rows") :to-equal '(("root") ("daemon")))))

  (it "reads lines as one line column and names headerless columns 1, 2, ..."
    (expect (column-summary (nth-value 1 (table-read '(("/work/l.txt" . "a
b
")) "l.txt" :format "lines")))
            :to-equal '(("line" "string")))
    (expect (mapcar #'first (column-summary (nth-value 1 (table-read '(("/work/n.csv" . "1,2
3,4
")) "n.csv"))))
            :to-equal '("1" "2")))

  (it "fails on unknown formats and bad csv"
    (expect (error-code (nth-value 1 (table-read '(("/work/x.dat" . "plain words")) "x.dat"))) :to-equal "input.unsupported-format")
    (expect (error-code (nth-value 1 (table-read '(("/work/q.csv" . "a,b
\"open,1
")) "q.csv")))
            :to-equal "input.syntax-error")
    (expect (error-code (nth-value 1 (table-read '(("/work/s.txt" . "a:b")) "s.txt" :format "sep"))) :to-equal "argument.invalid")))

(describe "table read selection"
  (flet ((names (fields) (mapcar #'first (field fields "rows"))))
    (it-each ((("city=Tokyo") ("Smith, J" "say \"hi\""))
              (("city!=Tokyo") ("Lee"))
              (("age<10") ("Lee"))
              (("age<=42") ("Smith, J" "Lee"))
              (("age>7") ("Smith, J"))
              (("age>=7") ("Smith, J" "Lee"))
              (("name~^S") ("Smith, J"))
              (("active=true" "city=Tokyo") ("Smith, J" "say \"hi\"")))
        "filters with --where ~A"
        (where expected)
      (expect (names (nth-value 1 (table-read `(("/work/p.csv" . ,*people-csv*)) "p.csv" :where where)))
              :to-equal expected))

    (it "chooses columns, windows rows with --range and --limit, and names the next range"
      (let ((files `(("/work/n.csv" . ,(format nil "n~%~{~D~%~}" (loop for i from 1 to 9 collect i))))))
        (multiple-value-bind (kind fields) (table-read files "n.csv" :range "2:7" :limit 3)
          (expect kind :to-be :partial)
          (expect (field fields "start_row") :to-be 2)
          (expect (field fields "rows") :to-equal '((2) (3) (4)))
          (expect (field fields "next_commands") :to-equal '("aitools table read n.csv --range 5:7 --limit 3")))
        (expect (error-code (nth-value 1 (table-read files "n.csv" :columns '("nope")))) :to-equal "argument.invalid")))))

(defparameter *sales-csv* "region,item,amount
east,apple,10
west,apple,5
east,pear,7
east,apple,3
")

(describe "table agg"
  (flet ((group-list (fields &rest names)
           (mapcar (lambda (group) (mapcar (lambda (name) (json-object-get group name)) names))
                   (field fields "groups"))))
    (it "groups with count, sum, avg, min, max, and distinct"
      (let ((fields (nth-value 1 (table-agg `(("/work/s.csv" . ,*sales-csv*)) "s.csv"
                                            :group-by '("region") :count t :sum "amount" :avg "amount"
                                            :min "amount" :max "amount" :distinct "item"))))
        (expect (mapcar (lambda (group) (json-object-get (json-object-get group "key") "region")) (field fields "groups"))
                :to-equal '("east" "west"))
        (expect (group-list fields "count" "sum" "min" "max" "distinct") :to-equal '((3 20 3 10 2) (1 5 5 5 1)))
        (expect (json-object-get (first (field fields "groups")) "avg") :to-be-close-to (/ 20d0 3))
        (expect (field fields "total_groups") :to-be 2)))

    (it "counts all rows as one group without --group-by, and applies --min-count and --sort --desc"
      (expect (group-list (nth-value 1 (table-agg `(("/work/s.csv" . ,*sales-csv*)) "s.csv" :sum "amount")) "sum")
              :to-equal '((25)))
      (let ((fields (nth-value 1 (table-agg '(("/work/w.txt" . "b
a
b
c
b
a
")) "w.txt" :format "lines" :group-by '("line") :count t :sort "count" :desc t :min-count 2))))
        (expect (mapcar (lambda (group) (json-object-get (json-object-get group "key") "line")) (field fields "groups"))
                :to-equal '("b" "a"))
        (expect (group-list fields "count") :to-equal '((3) (2)))))

    (it "rejects a numeric aggregate over non-numeric values with the rows as diagnostics"
      (multiple-value-bind (kind fields) (table-agg `(("/work/s.csv" . ,*sales-csv*)) "s.csv" :sum "item")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "argument.invalid")
        (expect (json-object-get (first (getf fields :diagnostics)) "row") :to-be 1)
        (expect (json-object-get (first (getf fields :diagnostics)) "value") :to-equal "apple")))

    (it "reports only the non-numeric rows of a mixed column and a concrete --where repair"
      (multiple-value-bind (kind fields)
          (table-agg '(("/work/m.csv" . "n
10
oops
20
")) "m.csv" :sum "n")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "argument.invalid")
        ;; Only row 2 ("oops") offends; rows 1 and 3 read as numbers.
        (expect (mapcar (lambda (d) (json-object-get d "row")) (getf fields :diagnostics)) :to-equal '(2))
        (expect (json-object-get (first (getf fields :diagnostics)) "value") :to-equal "oops")
        (expect (and (find-if (lambda (r) (search "n!=oops" (getf r :command))) (getf fields :repairs)) t)
                :to-be t)))))
