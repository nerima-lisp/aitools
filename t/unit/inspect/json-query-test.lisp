;;;; t/unit/inspect/json-query-test.lisp
;;;;
;;;; `json get`, `json select`, `json diff` (pointers with
;;;; ~0/~1, --keys, --raw, length, each --where operator, several
;;;; conditions, --pick, --sort-by, --output count, key order not a
;;;; difference).
(in-package #:aitools.inspect.test)

(defparameter *doc* "{\"a\": {\"b\": [1, 2, 3], \"s\": \"x\\ty\"},
 \"a/b\": 7, \"m~n\": 8,
 \"items\": [{\"id\": 1, \"name\": \"alpha\", \"score\": 30, \"tag\": \"red\"},
            {\"id\": 2, \"name\": \"beta\", \"score\": 10},
            {\"id\": 3, \"name\": \"gamma\", \"score\": 20, \"tag\": \"blue\"}]}
")

(defun json-get (pointer &rest options)
  (apply #'run-flow #'json-get-flow (make-test-ports :files `(("/work/d.json" . ,*doc*))) "d.json" pointer options))

(defun json-select (pointer &rest options)
  (apply #'run-flow #'json-select-flow (make-test-ports :files `(("/work/d.json" . ,*doc*))) "d.json" pointer options))

(defun selected-ids (fields)
  (mapcar (lambda (item) (json-object-get (json-object-get item "value") "id")) (field fields "items")))

(describe "json pointers"
  (it "parses RFC 6901 escapes and rejects malformed pointers"
    (expect (parse-json-pointer "") :to-equal '())
    (expect (parse-json-pointer "/a~1b/m~0n") :to-equal '("a/b" "m~n"))
    (expect (parse-json-pointer "/~01") :to-equal '("~1"))
    (expect (parse-json-pointer "a") :to-be :invalid)
    (expect (parse-json-pointer "/x~2") :to-be :invalid)
    (expect (format-json-pointer '("a/b" "m~n")) :to-equal "/a~1b/m~0n")))

(describe "json get"
  (it "returns value, type, and length"
    (multiple-value-bind (kind fields) (json-get "/a/b")
      (expect kind :to-be :ok)
      (expect (coerce (field fields "value") 'list) :to-equal '(1 2 3))
      (expect (field fields "type") :to-equal "array")
      (expect (field fields "length") :to-be 3)))

  (it "follows ~1 and ~0 escapes"
    (expect (field (nth-value 1 (json-get "/a~1b")) "value") :to-be 7)
    (expect (field (nth-value 1 (json-get "/m~0n")) "value") :to-be 8))

  (it "lists keys in document order with --keys"
    (expect (field (nth-value 1 (json-get "" :keys t)) "keys") :to-equal '("a" "a/b" "m~n" "items"))
    (expect (field (nth-value 1 (json-get "/a/b" :keys t)) "keys") :to-equal '("0" "1" "2")))

  (it "unescapes a string with --raw and counts its characters"
    (multiple-value-bind (kind fields) (json-get "/a/s" :raw t)
      (expect kind :to-be :ok)
      (expect (field fields "text") :to-equal (format nil "x~Cy" #\Tab))
      (expect (field fields "length") :to-be 3)))

  (it "fails with input.not-found and near keys for a missing pointer"
    (multiple-value-bind (kind fields) (json-get "/itemz/0")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (json-object-get (first (getf fields :candidates)) "pointer") :to-equal "/items")))

  (it "rejects a non-JSON file as input.unsupported-format"
    (expect (error-code (nth-value 1 (run-flow #'json-get-flow (make-test-ports :files '(("/work/x.json" . "{nope")))
                                               "x.json" "")))
            :to-equal "input.unsupported-format"))

  (it "returns a preview, partial, beyond --max-bytes"
    (multiple-value-bind (kind fields) (json-get "/items" :max-bytes "20")
      (expect kind :to-be :partial)
      (expect (length (field fields "value_preview")) :to-be 20)
      (expect (field fields "value") :to-be nil)
      (expect (field fields "truncated") :to-be t))))

(describe "json select"
  (it "filters with each operator"
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/score=10")))) :to-equal '(2))
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/score!=10")))) :to-equal '(1 3))
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/score<20")))) :to-equal '(2))
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/score<=20")))) :to-equal '(2 3))
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/score>20")))) :to-equal '(1))
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/score>=20")))) :to-equal '(1 3))
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/name~^[ab]")))) :to-equal '(1 2))
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/name=\"gamma\"")))) :to-equal '(3))
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/name=gamma")))) :to-equal '(3)))

  (it "treats a missing member as satisfying only !="
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/tag=red")))) :to-equal '(1))
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/tag!=red")))) :to-equal '(2 3)))

  (it "ANDs several conditions"
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/score>=10" "/name~a$")))) :to-equal '(1 2 3))
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/score>=20" "/id!=1")))) :to-equal '(3)))

  (it "picks members, sorts, and counts"
    (let ((fields (nth-value 1 (json-select "/items" :pick '("/name") :sort-by "/score" :desc t))))
      (expect (mapcar (lambda (item) (json-object-get (json-object-get item "value") "/name")) (field fields "items"))
              :to-equal '("alpha" "gamma" "beta"))
      (expect (json-object-get (first (field fields "items")) "pointer") :to-equal "/items/0"))
    (let ((fields (nth-value 1 (json-select "/items" :where '("/score>10") :output "count"))))
      (expect (field fields "mode") :to-equal "count")
      (expect (field fields "count") :to-be 2)))

  (it "is partial past --limit"
    (multiple-value-bind (kind fields) (json-select "/items" :limit 2)
      (expect kind :to-be :partial)
      (expect (length (field fields "items")) :to-be 2)
      (expect (field fields "total") :to-be 3)))

  (it "rejects a non-array as argument.invalid and a bad regex as input.syntax-error"
    (expect (error-code (nth-value 1 (json-select "/a"))) :to-equal "argument.invalid")
    (expect (error-code (nth-value 1 (json-select "/items" :where '("/name~(")))) :to-equal "input.syntax-error")
    (expect (error-code (nth-value 1 (json-select "/items" :where '("score")))) :to-equal "argument.invalid")))

(describe "json diff"
  (flet ((diff (a b &rest options)
           (apply #'run-flow #'json-diff-flow (make-test-ports :files `(("/work/a.json" . ,a) ("/work/b.json" . ,b)))
                  "a.json" "b.json" options)))
    (it "does not count key order or number spelling as a difference"
      (multiple-value-bind (kind fields) (diff "{\"x\": 1, \"y\": [1, 2]}" "{ \"y\": [1, 2.0], \"x\": 1 }")
        (expect kind :to-be :ok)
        (expect (field fields "identical") :to-be t)
        (expect (field fields "ops") :to-be nil)))

    (it "reports add, remove, and replace with pointers"
      (let* ((fields (nth-value 1 (diff "{\"x\": 1, \"gone\": true, \"l\": [1, 2]}" "{\"x\": 2, \"new\": null, \"l\": [1]}")))
             (ops (mapcar (lambda (op) (list (json-object-get op "op") (json-object-get op "pointer")))
                          (field fields "ops"))))
        (expect (json-false-value-p (field fields "identical")) :to-be t)
        (expect ops :to-equal '(("replace" "/x") ("remove" "/gone") ("remove" "/l/1") ("add" "/new")))))))

(describe "json select --where ~ that exhausts the regex step budget"
  (it "answers input.syntax-error rather than internal.unexpected"
    (let* ((doc (format nil "[{\"s\": \"~A\"}]" (make-string 40 :initial-element #\a)))
           (ports (make-test-ports :files `(("/work/j.json" . ,doc)))))
      (expect (error-code (nth-value 1 (run-flow #'json-select-flow ports "j.json" ""
                                                 :where '("/s~(a+)+(?=[bc])"))))
              :to-equal "input.syntax-error"))))

(defparameter *scalars* "{\"t\": true, \"f\": false, \"n\": null, \"s\": \"x\", \"i\": 1, \"r\": 1.5, \"o\": {}, \"a\": []}")

(describe "json get arguments"
  (it-each (("a pointer without a leading slash" "a" nil "\"a\" is not a JSON pointer")
            ("a --max-bytes that is not a size" "" "lots" "--max-bytes \"lots\" is not a size")
            ("a zero --max-bytes" "" "0" "--max-bytes \"0\" is not a size"))
      "rejects ~A as argument.invalid"
      (label pointer max-bytes message)
    (declare (ignore label))
    (multiple-value-bind (kind fields) (apply #'json-get pointer (and max-bytes (list :max-bytes max-bytes)))
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (getf fields :message) :to-contain message)))

  (it "rejects --keys on a scalar and names the plain get as the repair"
    (multiple-value-bind (kind fields) (json-get "/a~1b" :keys t)
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (getf fields :message) :to-equal "--keys needs an object or array; /a~1b is a number")
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools json get d.json '/a~1b'"))))

(describe "json get values"
  (it-each (("/t" "boolean") ("/f" "boolean") ("/n" "null") ("/s" "string")
            ("/i" "number") ("/r" "number") ("/o" "object") ("/a" "array"))
      "reports ~A as type ~A"
      (pointer type)
    (expect (field (nth-value 1 (run-flow #'json-get-flow (make-test-ports :files `(("/work/s.json" . ,*scalars*)))
                                          "s.json" pointer))
                   "type")
            :to-equal type))

  (it "rejects a non-JSON value in the domain"
    (signals error (json-type-name :not-json)))

  (it "renders a non-string value as JSON text with --raw"
    (expect (field (nth-value 1 (json-get "/a/b" :raw t)) "text") :to-equal "[1,2,3]"))

  (it "descends through array indexes and reports a missing index or a scalar step as not found"
    (expect (field (nth-value 1 (json-get "/items/1/name")) "value") :to-equal "beta")
    (multiple-value-bind (kind fields) (json-get "/items/9")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message) :to-contain "its deepest existing prefix is \"/items\""))
    (multiple-value-bind (kind fields) (json-get "/a/s/x")
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :candidates) :to-be nil)
      (expect kind :to-be :error)))

  (it "counts --max-bytes in UTF-8 bytes, not characters"
    (let* ((text (format nil "~C~C~C" (code-char #xE9) (code-char #x65E5) (code-char #x1F600)))
           (ports (make-test-ports :files `(("/work/u.json" . ,(format nil "{\"s\": \"~A\"}" text))))))
      ;; "\"é日😀\"" renders as 2 quote bytes + 2 + 3 + 4 = 11 bytes, 5 characters.
      (expect (run-flow #'json-get-flow ports "u.json" "/s" :max-bytes "11") :to-be :ok)
      (expect (run-flow #'json-get-flow ports "u.json" "/s" :max-bytes "10") :to-be :partial)))

  (it "names --keys and the first element as next commands of a truncated array"
    (expect (field (nth-value 1 (json-get "/items" :max-bytes "20")) "next_commands")
            :to-equal '("aitools json get d.json /items --keys" "aitools json get d.json /items/0"))))

(describe "json select arguments and ordering"
  (it "rejects a --pick that is not a relative pointer"
    (multiple-value-bind (kind fields) (json-select "/items" :pick '("name"))
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (getf fields :message) :to-equal "\"name\" is not a relative JSON pointer")))

  (it "rejects a malformed array pointer"
    (expect (error-code (nth-value 1 (json-select "items"))) :to-equal "argument.invalid"))

  (it "picks a missing member as null"
    (let ((values (mapcar (lambda (item) (json-object-get (json-object-get item "value") "/tag"))
                          (field (nth-value 1 (json-select "/items" :pick '("/tag"))) "items"))))
      (expect (first values) :to-equal "red")
      (expect (json-null-value-p (second values)) :to-be t)))

  (it "sorts ascending with --sort-by and reverses document order with --desc alone"
    (expect (selected-ids (nth-value 1 (json-select "/items" :sort-by "/score"))) :to-equal '(2 3 1))
    (expect (selected-ids (nth-value 1 (json-select "/items" :desc t))) :to-equal '(3 2 1)))

  (it "orders values of mixed types null, false, true, numbers, strings, then containers"
    (let* ((doc "[{\"v\": \"b\"}, {\"v\": 2}, {\"v\": [1]}, {\"v\": null}, {\"v\": true}, {\"v\": false}, {\"v\": \"a\"}, {}, {\"v\": 1}]")
           (fields (nth-value 1 (run-flow #'json-select-flow (make-test-ports :files `(("/work/m.json" . ,doc)))
                                          "m.json" "" :sort-by "/v"))))
      (expect (mapcar (lambda (item) (json-object-get item "pointer")) (field fields "items"))
              :to-equal '("/3" "/7" "/5" "/4" "/8" "/1" "/6" "/0" "/2"))))

  (it "compares strings by order and never matches across types"
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/name<\"b\"")))) :to-equal '(1))
    (expect (selected-ids (nth-value 1 (json-select "/items" :where '("/name>1")))) :to-be nil))

  (it "names --output count as the next command past --limit"
    (expect (field (nth-value 1 (json-select "/items" :limit 1)) "next_commands")
            :to-equal '("aitools json select d.json /items --output count"))))

(describe "json diff limits"
  (it "is partial past --limit with the total"
    (multiple-value-bind (kind fields)
        (run-flow #'json-diff-flow (make-test-ports :files '(("/work/a.json" . "[1, 2, 3]") ("/work/b.json" . "[]")))
                  "a.json" "b.json" :limit 1)
      (expect kind :to-be :partial)
      (expect (length (field fields "ops")) :to-be 1)
      (expect (field fields "total") :to-be 3)
      (expect (field fields "truncated") :to-be t))))

(describe "json-equal"
  (flet ((parsed (text) (parse-json-document/k text :on-value #'identity :on-error (lambda (&rest error) (fail (format nil "~S" error))))))
    (it-each (("{\"a\": 1, \"b\": [1, 2]}" "{\"b\": [1, 2.0], \"a\": 1}" t)
              ("[1, [2]]" "[1, [2]]" t)
              ("[1, 2]" "[2, 1]" nil)
              ("null" "null" t)
              ("null" "false" nil)
              ("true" "true" t)
              ("\"a\"" "\"b\"" nil)
              ("{\"a\": null}" "{\"a\": false}" nil))
        "compares ~A and ~A as ~A"
        (a b equal)
      (expect (and (json-equal (parsed a) (parsed b)) t) :to-be equal))))

(describe "json diff of arrays"
  (it "reports appended elements as adds at their indexes"
    (let ((fields (nth-value 1 (run-flow #'json-diff-flow
                                         (make-test-ports :files '(("/work/a.json" . "[1]") ("/work/b.json" . "[1, 2, 3]")))
                                         "a.json" "b.json"))))
      (expect (mapcar (lambda (op) (list (json-object-get op "op") (json-object-get op "pointer") (json-object-get op "new")))
                      (field fields "ops"))
              :to-equal '(("add" "/1" 2) ("add" "/2" 3))))))
