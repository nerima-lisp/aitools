;;;; t/unit/inspect/table-edge-test.lisp
;;;;
;;;; `table read` and `table agg` beyond the basic cases: option validation,
;;;; decoding and row windows, format detection without a known extension,
;;;; JSON documents, number typing, agg options and output, and row shapes.
;;;; Helpers and fixtures come from table-test.lisp.
(in-package #:aitools.inspect.test)

(describe "table read option validation"
  (it-each (("an unknown --format" (:format "xml") "argument.invalid" "--format \"xml\" is not one of csv, tsv")
            ("--delimiter without --format sep" (:format "csv" :delimiter ";") "argument.invalid"
             "--delimiter applies to --format sep only")
            ("--pointer without --format json" (:format "csv" :pointer "/rows") "argument.invalid"
             "--pointer applies to --format json only")
            ("--format sep without a delimiter" (:format "sep" :delimiter "") "argument.invalid"
             "--format sep needs a non-empty --delimiter")
            ("--ws-columns without --format ws" (:format "csv" :ws-columns 2) "argument.invalid"
             "--ws-columns applies to --format ws only")
            ("an unknown --encoding" (:encoding "ebcdic") "input.unsupported-format"
             "unknown encoding \"ebcdic\"; supported: utf-8, shift_jis")
            ("a malformed --range" (:range "x:y") "argument.invalid" "--range \"x:y\" is not S:E, S:, or N")
            ("a --where naming no column" (:where ("nope=1")) "argument.invalid" "--where names no column \"nope\"")
            ("a --where with no operator" (:where ("age")) "argument.invalid" "--where \"age\" has no operator")
            ("a --columns number past the last column" (:columns ("9")) "argument.invalid" "no column \"9\""))
      "rejects ~A"
      (label options code message)
    (declare (ignore label))
    (multiple-value-bind (kind fields) (apply #'table-read `(("/work/p.csv" . ,*people-csv*)) "p.csv" options)
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal code)
      (expect (getf fields :message) :to-contain message)
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools schema table read"))))

(describe "table read decoding and windows"
  (it "decodes --encoding before parsing and carries it into the next command"
    (let ((octets (concatenate '(vector (unsigned-byte 8))
                               (string-bytes "name,n") #(10) #(#x63 #x61 #x66 #xE9) (string-bytes ",1") #(10)
                               #(#xE9 #x74 #xE9) (string-bytes ",2") #(10))))
      (multiple-value-bind (kind fields) (table-read `(("/work/l.csv" . ,octets)) "l.csv" :encoding "iso-8859-1" :limit 1)
        (expect kind :to-be :partial)
        (expect (field fields "rows") :to-equal (list (list (format nil "caf~C" (code-char #xE9)) 1)))
        (expect (field fields "next_commands")
                :to-equal '("aitools table read l.csv --encoding iso-8859-1 --range 2:2 --limit 1")))))

  (it "reads --encoding utf-8 as plain UTF-8"
    (expect (field (nth-value 1 (table-read `(("/work/p.csv" . ,*people-csv*)) "p.csv" :encoding "utf-8" :columns '("city")))
                   "rows")
            :to-equal '(("Tokyo") ("Osaka") ("Tokyo"))))

  (it "returns an empty window for a --range that starts past the last row"
    (multiple-value-bind (kind fields) (table-read `(("/work/p.csv" . ,*people-csv*)) "p.csv" :range "7:")
      (expect kind :to-be :ok)
      (expect (field fields "start_row") :to-be 7)
      (expect (field fields "rows") :to-be nil)
      (expect (field fields "total_rows") :to-be 3))))

(describe "table format detection without a known extension"
  (it-each (("a JSON array" "json" "  [{\"a\": 1}]" (1))
            ("one JSON object" "json" "{\"rows\": [{\"a\": 2}]}" (2))
            ("several JSON object lines" "jsonl" "{\"a\": 3}
{\"a\": 4}" (3 4))
            ("a tab-separated line" "tsv" "a	b
5	6" (5))
            ("a comma-separated line" "csv" "a,b
7,8" (7)))
      "detects ~A as ~A"
      (label format content firsts)
    (declare (ignore label))
    (multiple-value-bind (kind fields)
        (apply #'table-read `(("/work/x.dat" . ,content)) "x.dat" (and (search "rows" content) '(:pointer "/rows")))
      (expect kind :to-be :ok)
      (expect (field fields "format") :to-equal format)
      (expect (mapcar #'first (field fields "rows")) :to-equal firsts)))

  (it "names --format ws and --format lines as repairs when nothing decides"
    (multiple-value-bind (kind fields) (table-read '(("/work/words.dat" . "  plain words")) "words.dat")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.unsupported-format")
      (expect (mapcar (lambda (repair) (getf repair :command)) (getf fields :repairs))
              :to-equal '("aitools table read words.dat --format ws" "aitools table read words.dat --format lines")))))

(describe "table read of JSON documents"
  (it-each (("a malformed --pointer" "[1]" "rows" "\"rows\" is not a JSON pointer")
            ("a --pointer that names nothing" "{\"a\": []}" "/nope" "pointer /nope does not exist")
            ("a document that is an object" "{\"a\": []}" nil "the document is not an array; give --pointer to one")
            ("a --pointer to an object" "{\"a\": {}}" "/a" "/a is not an array")
            ("malformed JSON" "[1," nil "invalid JSON"))
      "reports ~A as input.syntax-error"
      (label content pointer message)
    (declare (ignore label))
    (multiple-value-bind (kind fields) (table-read `(("/work/d.json" . ,content)) "d.json" :pointer pointer)
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.syntax-error")
      (expect (getf fields :message) :to-contain message)))

  (it "reports the line of a malformed JSON Lines record"
    (multiple-value-bind (kind fields) (table-read '(("/work/r.jsonl" . "{\"a\": 1}
{\"a\":
")) "r.jsonl")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.syntax-error")
      (expect (json-object-get (first (getf fields :diagnostics)) "line") :to-be 2)))

  (it "puts scalar elements in a value column beside object members"
    (let ((fields (nth-value 1 (table-read '(("/work/v.json" . "[{\"a\": 1}, 5, \"x\"]")) "v.json"))))
      (expect (column-summary fields) :to-equal '(("a" "integer") ("value" "mixed")))
      (expect (json-null-value-p (second (first (field fields "rows")))) :to-be t)
      (expect (mapcar #'second (rest (field fields "rows"))) :to-equal '(5 "x"))))

  (it "types a column with only nulls as null"
    (expect (column-summary (nth-value 1 (table-read '(("/work/n.json" . "[{\"z\": null}]")) "n.json")))
            :to-equal '(("z" "null")))))

(describe "table read number typing"
  (it "reads decimal spellings as numbers by value"
    (let ((fields (nth-value 1 (table-read '(("/work/n.csv" . "v
1.5
.5
5.
-2.25
+3
1e3
25E-2
007.5
")) "n.csv"))))
      (expect (column-summary fields) :to-equal '(("v" "number")))
      (expect (mapcar (lambda (row) (float (first row) 1d0)) (field fields "rows"))
              :to-equal '(1.5d0 0.5d0 5d0 -2.25d0 3d0 1000d0 0.25d0 7.5d0))))

  (it-each (("an exponent past four digits" "1e99999")
            ("a bare dot" ".")
            ("a sign alone" "-"))
      "keeps ~A as a string"
      (label text)
    (declare (ignore label))
    (let ((fields (nth-value 1 (table-read `(("/work/s.csv" . ,(format nil "v~%1~%~A~%" text))) "s.csv"))))
      (expect (column-summary fields) :to-equal '(("v" "string")))
      (expect (second (field fields "rows")) :to-equal (list text))))

  (it "types an all-empty column as null"
    (expect (column-summary (nth-value 1 (table-read '(("/work/e.csv" . "a,b
1,
2,
")) "e.csv")))
            :to-equal '(("a" "integer") ("b" "null")))))

(describe "table agg options and output"
  (flet ((agg (&rest options)
           (apply #'table-agg `(("/work/s.csv" . ,*sales-csv*)) "s.csv" options))
         (regions (fields)
           (mapcar (lambda (group) (json-object-get (json-object-get group "key") "region")) (field fields "groups"))))
    (it "rejects a --sort that names no output column"
      (multiple-value-bind (kind fields) (agg :group-by '("region") :sort "amount")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "argument.invalid")
        (expect (getf fields :message) :to-contain "--sort \"amount\" names no output column")
        (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools schema table agg")))

    (it "rejects an aggregate over an unknown column with near column names"
      (multiple-value-bind (kind fields) (agg :sum "amont")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "argument.invalid")
        (expect (json-object-get (first (getf fields :candidates)) "column") :to-equal "amount")))

    (it "sorts ascending by an aggregate and by a --group-by column"
      (expect (regions (nth-value 1 (agg :group-by '("region") :sum "amount" :sort "sum"))) :to-equal '("west" "east"))
      (expect (regions (nth-value 1 (agg :group-by '("region") :sort "region" :desc t))) :to-equal '("west" "east")))

    (it "reverses the group order with --desc alone"
      (expect (regions (nth-value 1 (agg :group-by '("region") :desc t))) :to-equal '("west" "east")))

    (it "is partial past --limit and names the command for every group"
      (multiple-value-bind (kind fields) (agg :group-by '("item") :limit 1)
        (expect kind :to-be :partial)
        (expect (length (field fields "groups")) :to-be 1)
        (expect (field fields "total_groups") :to-be 2)
        (expect (field fields "next_commands") :to-equal '("aitools table agg s.csv --limit 2"))))

    (it "reports only --distinct, without an implied count"
      (let ((group (first (field (nth-value 1 (agg :distinct "item")) "groups"))))
        (expect (json-object-get group "distinct") :to-be 2)
        (expect (nth-value 1 (json-object-get group "count")) :to-be nil)))

    (it "names the column's type when a numeric aggregate meets text"
      (expect (getf (nth-value 1 (agg :avg "item")) :message)
              :to-equal "--avg needs a numeric column; item is string"))

    (it "renders a non-string offending value as JSON in the --where repair"
      (multiple-value-bind (kind fields)
          (table-agg '(("/work/b.jsonl" . "{\"ok\": true}
{\"ok\": false}
")) "b.jsonl" :max "ok")
        (expect kind :to-be :error)
        (expect (json-object-get (first (getf fields :diagnostics)) "value") :to-be t)
        (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools table agg b.jsonl --where 'ok!=true'")))))

(describe "table read row shapes"
  (it "keeps a newline inside a quoted CSV field and reads CRLF records"
    (let ((fields (nth-value 1 (table-read `(("/work/q.csv" . ,(format nil "a,b~C~C\"x~%y\",2~C~C" #\Return #\Newline #\Return #\Newline)))
                                           "q.csv"))))
      (expect (field fields "rows") :to-equal (list (list (format nil "x~%y") 2)))))

  (it "fills the missing cells of a short row with null"
    (let ((rows (field (nth-value 1 (table-read '(("/work/r.csv" . "a,b
1,2
3
")) "r.csv")) "rows")))
      (expect (first (second rows)) :to-be 3)
      (expect (json-null-value-p (second (second rows))) :to-be t)))

  (it "refuses a row past the column limit as input.syntax-error"
    (multiple-value-bind (kind fields)
        (table-read `(("/work/w.csv" . ,(format nil "~{~A~^,~}~%" (loop for i below 8193 collect i)))) "w.csv" :no-header t)
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.syntax-error")
      (expect (getf fields :message) :to-contain "a row has 8193 columns, past the 8192-column limit")))

  (it "reads a JSON column of integers and fractions as number"
    (expect (column-summary (nth-value 1 (table-read '(("/work/n.json" . "[{\"v\": 1}, {\"v\": 1.5}]")) "n.json")))
            :to-equal '(("v" "number"))))

  (it-each (("-1") ("+1") ("0"))
      "does not take ~S as a column number"
      (spec)
    (expect (error-code (nth-value 1 (table-read `(("/work/p.csv" . ,*people-csv*)) "p.csv" :columns (list spec))))
            :to-equal "argument.invalid")))
