(in-package #:aitools.inspect.test)

(describe "format value mapping through parser helpers"
  (it "maps nested TOML arrays and inline tables"
    (let* ((value
             (parse-toml-document
              (format nil
                      "records = [{ name = \"one\", values = [1, false] }, { name = \"two\", values = [] }]~%")))
           (records (json-object-get value "records"))
           (first-record (aref records 0))
           (first-values (json-object-get first-record "values"))
           (second-record (aref records 1)))
      (expect (length records) :to-be 2)
      (expect (json-object-get first-record "name") :to-equal "one")
      (expect (aref first-values 0) :to-be 1)
      (expect (json-false-value-p (aref first-values 1)) :to-be t)
      (expect (json-object-get second-record "name") :to-equal "two")
      (expect (length (json-object-get second-record "values")) :to-be 0)))

  (it "keeps the nested TOML pointer when a parsed array contains infinity"
    (handler-case
        (progn
          (parse-toml-document "records = [{ score = inf }]")
          (fail "expected an unsupported TOML value"))
      (toml-unsupported-value (condition)
        (expect (toml-unsupported-reason condition) :to-equal "non-finite-float")
        (expect (toml-unsupported-pointer condition) :to-equal "/records/0/score"))))

  (it "keeps the nested TOML write pointer for negative infinity"
    (handler-case
        (progn
          (parse-toml-write-document "records = [{ values = [-inf] }]")
          (fail "expected an unsupported TOML write value"))
      (toml-unsupported-value (condition)
        (expect (toml-unsupported-reason condition) :to-equal "non-finite-float")
        (expect (toml-unsupported-pointer condition) :to-equal "/records/0/values/0"))))

  (it "maps nested YAML sequences, mappings, booleans, and null"
    (let* ((value
             (parse-yaml-document
              (format nil
                      "records:~%  - name: one~%    values: [1, false, null]~%  - name: two~%    values: []~%")))
           (records (json-object-get value "records"))
           (first-record (aref records 0))
           (values (json-object-get first-record "values"))
           (second-record (aref records 1)))
      (expect (length records) :to-be 2)
      (expect (json-object-get first-record "name") :to-equal "one")
      (expect (aref values 0) :to-be 1)
      (expect (json-false-value-p (aref values 1)) :to-be t)
      (expect (json-null-value-p (aref values 2)) :to-be t)
      (expect (json-object-get second-record "name") :to-equal "two")
      (expect (length (json-object-get second-record "values")) :to-be 0)))

  (it "keeps the nested YAML pointer when a parsed sequence contains infinity"
    (handler-case
        (progn
          (parse-yaml-document (format nil "records:~%  - score: .inf~%"))
          (fail "expected an unsupported YAML value"))
      (yaml-unsupported-value (condition)
        (expect (yaml-unsupported-reason condition) :to-equal "non-finite-float")
        (expect (yaml-unsupported-pointer condition) :to-equal "/records/0/score"))))

  (it "keeps the nested YAML write pointer for negative infinity"
    (handler-case
        (progn
          (parse-yaml-write-document (format nil "records:~%  - values: [-.inf]~%"))
          (fail "expected an unsupported YAML write value"))
      (yaml-unsupported-value (condition)
        (expect (yaml-unsupported-reason condition) :to-equal "non-finite-float")
        (expect (yaml-unsupported-pointer condition) :to-equal "/records/0/values/0"))))

  (it "reads an explicit Core tag but reports it in YAML write preflight"
    (let ((text "value: !!str true"))
      (expect (json-object-get (parse-yaml-document text) "value") :to-equal "true")
      (handler-case
          (progn
            (parse-yaml-write-document text)
            (fail "expected an unsupported YAML write value"))
        (yaml-unsupported-value (condition)
          (expect (yaml-unsupported-reason condition) :to-equal "explicit-tag")
          (expect (yaml-unsupported-pointer condition) :to-equal "/value")))))
  (it "rejects a non-string key in a TOML mapping helper value"
    (let ((value (make-hash-table :test #'equal)))
      (setf (gethash 7 value) "seven")
      (handler-case
          (progn
            (aitools.inspect.domain::%toml-to-json value nil)
            (fail "expected a non-string TOML mapping key"))
        (toml-unsupported-value (condition)
          (expect (toml-unsupported-reason condition) :to-equal "non-string-key")
          (expect (toml-unsupported-pointer condition) :to-equal "")))))
  (it "skips a visited YAML node before checking its explicit tag"
    (let* ((node (first (yaml-kit:compose-all "!!str text")))
           (visited (make-hash-table :test #'eq)))
      (setf (gethash node visited) t)
      (expect (aitools.inspect.domain::%yaml-write-check-node
               node nil visited 0 nil)
              :to-be nil)
      (expect (gethash node visited) :to-be t)))
)
