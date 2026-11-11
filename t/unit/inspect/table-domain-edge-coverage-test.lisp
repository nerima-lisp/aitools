(in-package #:aitools.inspect.test)

(describe "JSON Lines table record boundaries"
  (it "skips blank lines and stops at the first malformed record"
    (multiple-value-bind (kind fields)
        (table-read
         '(("/work/r.jsonl" . "{\"n\":1}

not-json
{\"n\":2}
also-not-json"))
         "r.jsonl")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.syntax-error")
      (expect (json-object-get (first (getf fields :diagnostics)) "line")
              :to-be 3))))

(describe "empty CSV tables"
  (it "returns an empty typed table for an empty CSV document"
    (multiple-value-bind (kind fields)
        (table-read '(("/work/empty.csv" . "")) "empty.csv")
      (expect kind :to-be :ok)
      (expect (field fields "format") :to-equal "csv")
      (expect (field fields "rows") :to-be nil)
      (expect (field fields "total_rows") :to-be 0))))

(describe "table aggregation with no selected rows"
  (it "returns no groups when a mixed numeric column is filtered to nothing"
    (multiple-value-bind (kind fields)
        (table-agg '(("/work/m.csv" . "n
1
bad")) "m.csv"
                   :where '("n=missing") :sum "n" :avg "n")
      (expect kind :to-be :ok)
      (expect (field fields "groups") :to-be nil)
      (expect (field fields "total_groups") :to-be 0))))

(describe "container-valued table group keys"
  (it "keeps incomparable object keys as separate groups in encounter order"
    (let* ((fields
             (nth-value 1
               (table-agg
                '(("/work/g.jsonl" . "{\"group\":{\"a\":1},\"n\":1}
{\"group\":{\"a\":2},\"n\":2}"))
                "g.jsonl" :group-by '("group") :count t)))
           (groups (field fields "groups")))
      (expect (length groups) :to-be 2)
      (expect (mapcar (lambda (group)
                        (json-object-get
                         (json-object-get (json-object-get group "key") "group")
                         "a"))
                      groups)
              :to-equal '(1 2))
      (expect (mapcar (lambda (group) (json-object-get group "count")) groups)
              :to-equal '(1 1)))))

(describe "incomplete table number exponents"
  (it-each (("an exponent marker without digits" "1e")
            ("a signed empty exponent" "1e-"))
      "keeps ~A as text"
      (label text)
    (declare (ignore label))
    (let ((fields
            (nth-value 1
              (table-read
               (list (cons "/work/n.csv" (format nil "v~%1~%~A" text)))
               "n.csv"))))
      (expect (column-summary fields) :to-equal '(("v" "string")))
      (expect (second (field fields "rows")) :to-equal (list text)))))

(describe "direct table text helper boundaries"
  (it "parses raw CRLF records when called before line normalization"
    (aitools.kernel.domain:flow-call
        (aitools.inspect.domain::%csv-records/k
         (format nil "name,n~C~Citem,2~C~C"
                 #\Return #\Newline #\Return #\Newline)
         #\,)
      (:on-records (records)
        (expect records :to-equal '(("name" "n") ("item" "2"))))
      (:on-error (message line)
        (fail (format nil "CSV rejected raw CRLF at line ~D: ~A" line message)))))

  (it "splits an empty whitespace record into no fields"
    (expect (aitools.inspect.domain::%split-whitespace "" nil)
            :to-equal nil))

  (it "returns nil when the number helper's JSON parser rejects a token"
    (expect (aitools.inspect.domain::%number-value "x") :to-be nil)))
