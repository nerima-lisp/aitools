(in-package #:aitools.inspect.test)

(describe "table numeric text edge behavior"
  (it "normalizes signed integers and decimals adjacent to exponents"
    (let* ((fields
             (nth-value 1
               (table-read
                '(("/work/n.csv" . "v
+000
-0
.5e+1
5.e-1"))
                "n.csv")))
           (rows (field fields "rows")))
      (expect (column-summary fields) :to-equal '(("v" "number")))
      (expect (mapcar (lambda (row) (float (first row) 1d0)) rows)
              :to-equal '(0d0 0d0 5d0 0.5d0))))

  (it "keeps an exponent without digits as text"
    (let ((fields (nth-value 1 (table-read '(("/work/n.csv" . "v
1
1e+")) "n.csv"))))
      (expect (column-summary fields) :to-equal '(("v" "string")))
      (expect (field fields "rows") :to-equal '(("1") ("1e+"))))))

(describe "CSV final record and empty-cell behavior"
  (it "keeps a final record without a newline and types empty cells by column"
    (let* ((fields
             (nth-value 1
               (table-read
                '(("/work/end.csv" . "label,n
alpha,1
,2
omega,"))
                "end.csv")))
           (rows (field fields "rows")))
      (expect (first rows) :to-equal '("alpha" 1))
      (expect (first (second rows)) :to-equal "")
      (expect (second (second rows)) :to-be 2)
      (expect (first (third rows)) :to-equal "omega")
      (expect (json-null-value-p (second (third rows))) :to-be t)
      (expect (field fields "total_rows") :to-be 3))))

(describe "table where column comparisons"
  (it "accepts a 1-based column number and reports an invalid match pattern"
    (let ((rows
            (field
             (nth-value 1
               (table-read '(("/work/n.csv" . "name,n
alpha,8
beta,10"))
                          "n.csv" :where '("2>=9")))
             "rows")))
      (expect rows :to-equal '(("beta" 10))))
    (multiple-value-bind (kind fields)
        (table-read '(("/work/n.csv" . "name,n
alpha,8")) "n.csv" :where '("n~["))
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (getf fields :message) :to-contain "invalid regular expression"))))

(describe "table row-width validation across text formats"
  (it "rejects more than 8192 fields for TSV, separator, and whitespace rows"
    (flet ((wide-row (delimiter)
             (with-output-to-string (stream)
               (dotimes (index 8193)
                 (when (plusp index) (write-string delimiter stream))
                 (write-char #\x stream)))))
      (dolist (case
               (list (list "tsv" (wide-row (string #\Tab)))
                     (list "sep" (wide-row ":") :delimiter ":")
                     (list "ws" (wide-row " "))))
        (let ((format (first case))
              (content (second case)))
          (multiple-value-bind (kind fields)
              (apply #'table-read
                     (list (cons "/work/wide.dat" content))
                     "wide.dat"
                     (append (list :format format :no-header t) (cddr case)))
            (expect kind :to-be :error)
            (expect (error-code fields) :to-equal "input.syntax-error")
            (expect (getf fields :message)
                    :to-contain "a row has 8193 columns")))))))

(describe "table aggregate numeric strings and null groups"
  (it "aggregates decimal strings left after filtering a mixed column"
    (multiple-value-bind (kind fields)
        (table-agg '(("/work/m.csv" . "n
1.5
bad
.5e+1
5.e-1")) "m.csv"
                   :where '("n!=bad") :sum "n" :avg "n" :min "n" :max "n" :count t)
      (expect kind :to-be :ok)
      (let ((group (first (field fields "groups"))))
        (expect (json-object-get group "count") :to-be 3)
        (expect (json-object-get group "sum") :to-be-close-to 7d0)
        (expect (json-object-get group "avg") :to-be-close-to (/ 7d0 3))
        (expect (json-object-get group "min") :to-be-close-to 0.5d0)
        (expect (json-object-get group "max") :to-be-close-to 5d0))))

  (it "keeps groups whose numeric cells and distinct values are all null"
    (let* ((fields
             (nth-value 1
               (table-agg
                '(("/work/g.jsonl" . "{\"g\":\"empty\",\"n\":null,\"tag\":null}
{\"g\":\"full\",\"n\":4,\"tag\":\"x\"}
{\"g\":\"empty\",\"tag\":null}"))
                "g.jsonl" :group-by '("g") :sum "n" :avg "n" :min "n" :max "n"
                :distinct "tag" :count t)))
           (empty (first (field fields "groups"))))
      (expect (json-object-get (json-object-get empty "key") "g") :to-equal "empty")
      (expect (json-object-get empty "count") :to-be 2)
      (expect (json-object-get empty "sum") :to-be 0)
      (expect (json-null-value-p (json-object-get empty "avg")) :to-be t)
      (expect (json-null-value-p (json-object-get empty "min")) :to-be t)
      (expect (json-null-value-p (json-object-get empty "max")) :to-be t)
      (expect (json-object-get empty "distinct") :to-be 0)))

  (it "excludes blank, missing, and numeric cells from mixed-column diagnostics"
    (multiple-value-bind (kind fields)
        (table-agg
         '(("/work/m.jsonl" . "{\"n\":10}
{\"n\":\"\"}
{}
{\"n\":\"2.5\"}
{\"n\":\"bad\"}
{\"n\":3}"))
         "m.jsonl" :sum "n")
      (expect kind :to-be :error)
      (expect (mapcar (lambda (item) (json-object-get item "row"))
                      (getf fields :diagnostics))
              :to-equal '(5))
      (expect (json-object-get (first (getf fields :diagnostics)) "value")
              :to-equal "bad")))

  (it "limits non-numeric diagnostics to the first twenty rows"
    (let ((content
            (with-output-to-string (stream)
              (write-line "n" stream)
              (loop repeat 21 do (write-line "bad" stream)))))
      (multiple-value-bind (kind fields)
          (table-agg (list (cons "/work/m.csv" content)) "m.csv" :sum "n")
        (let ((rows (mapcar (lambda (item) (json-object-get item "row"))
                            (getf fields :diagnostics))))
          (expect kind :to-be :error)
          (expect (length rows) :to-be 20)
          (expect rows :to-equal (loop for row from 1 to 20 collect row)))))))
