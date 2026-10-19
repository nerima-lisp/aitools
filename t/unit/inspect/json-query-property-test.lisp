(in-package #:aitools.inspect.test)

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

  (it-each (("without filters" nil 3 "aitools json select d.json /items --output count")
            ("with one filter" ("/score>=20") 2 "aitools json select d.json /items --where '/score>=20' --output count")
            ("with multiple filters" ("/score>=20" "/name!=\"beta\"") 2
             "aitools json select d.json /items --where '/score>=20' --where '/name!=\"beta\"' --output count"))
      "names the matching count as the next command past --limit ~A"
      (label where total command)
    (declare (ignore label))
    (multiple-value-bind (kind fields) (json-select "/items" :where where :limit 1)
      (expect kind :to-be :partial)
      (expect (field fields "total") :to-be total)
      (expect (field fields "next_commands") :to-equal (list command)))
    (expect (field (nth-value 1 (json-select "/items" :where where :output "count")) "count")
            :to-be total)))

(describe "JSON parser properties"
  (cl-weave:it-property
   "round-trips generated JSON string documents"
   ((value (cl-weave:gen-string
            :min-length 0
            :max-length 80
            :alphabet (format nil "aB09\"\\~%~C" #\Tab))))
   (let ((parsed
          (aitools.kernel.domain:flow-call
           (parse-json-document/k (render-json value))
           (:on-value (document)
             document)
           (:on-error (message line column)
             (fail (format nil "generated JSON failed at ~D:~D: ~A"
                           line
                           column
                           message))))))
     (expect parsed :to-equal value)))

  (cl-weave:it-property
   "splits generated --where text at its earliest longest operator"
   ((left (cl-weave:gen-string
           :min-length 1
           :max-length 20
           :alphabet "abcxyz012 _"))
    (middle (cl-weave:gen-string
             :min-length 0
             :max-length 20
             :alphabet "abcxyz012 _"))
    (right (cl-weave:gen-string
            :min-length 0
            :max-length 30
            :alphabet "abcxyz012 _")))
   (multiple-value-bind (actual-left operator actual-right)
       (split-comparison (format nil "~A>=~A=~A" left middle right))
     (expect (list actual-left operator actual-right)
             :to-equal (list left ">=" (format nil "~A=~A" middle right)))))

  (it "kills the comparison-operator mutation in --where parsing"
    (let* ((form
            aitools.inspect.domain::*comparison-position-before-form*)
           (baseline
            aitools.inspect.domain::*comparison-position-before-p*)
           (probes '((1 1) (2 1)))
           (results
            (cl-weave:run-mutations
             form
             (lambda (mutated-form mutation)
               (declare (ignore mutation))
               (let ((mutant (compile nil mutated-form)))
                 (equal
                  (mapcar (lambda (probe)
                            (funcall mutant (first probe) (second probe)))
                          probes)
                  (mapcar (lambda (probe)
                            (funcall baseline (first probe) (second probe)))
                          probes))))
             :operators
             '(:comparison-operator))))
      (expect (length results) :to-be 1)
      (cl-weave:assert-mutation-score results 1.0))))
