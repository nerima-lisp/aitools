(in-package #:aitools.inspect.test)

(describe "YAML read flows"
  (it "gets values converted through the YAML adapter"
    (multiple-value-bind (kind fields)
        (run-flow #'yaml-get-flow
                  (make-test-ports
                   :files (list (cons "/work/doc.yaml"
                                      (format nil "name: Ada~%enabled: true~%"))))
                  "doc.yaml" "")
      (expect kind :to-be :ok)
      (let ((value (field fields "value")))
        (expect (json-object-get value "name") :to-equal "Ada")
        (expect (json-object-get value "enabled") :to-be t))))

  (it "selects array values through the shared query flow"
    (let* ((text (format nil
                         "items:~%  - id: 1~%    score: 2~%  - id: 2~%    score: 3~%"))
           (ports (make-test-ports :files (list (cons "/work/items.yaml" text)))))
      (multiple-value-bind (kind fields)
          (run-flow #'yaml-select-flow ports "items.yaml" "/items"
                    :where '("/score>2") :pick '("/id"))
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 1)
        (expect (json-object-get (json-object-get (first (field fields "items")) "value")
                                "/id")
                :to-be 2))))

  (it "diffs YAML values through the shared diff flow"
    (let ((ports (make-test-ports
                  :files '(("/work/a.yaml" . "value: 1")
                           ("/work/b.yaml" . "value: 2")))))
      (multiple-value-bind (kind fields)
          (run-flow #'yaml-diff-flow ports "a.yaml" "b.yaml")
        (expect kind :to-be :ok)
        (expect (json-object-get (first (field fields "ops")) "pointer")
                :to-equal "/value"))))

  (it "maps unsupported values to the YAML read error and schema repair"
    (let ((ports (make-test-ports
                  :files '(("/work/bad.yaml" . "outer: {x: !custom value}")))))
      (multiple-value-bind (kind fields)
          (run-flow #'yaml-get-flow ports "bad.yaml" "")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.unsupported-value")
        (expect (getf fields :message) :to-contain "JSON Pointer /outer/x")
        (expect (getf (first (getf fields :repairs)) :command)
                :to-equal "aitools schema yaml get"))))

  (it "maps malformed YAML to the syntax error and schema repair"
    (let ((ports (make-test-ports
                  :files '(("/work/bad.yaml" . "value: [1,")))))
      (multiple-value-bind (kind fields)
          (run-flow #'yaml-get-flow ports "bad.yaml" "")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.syntax-error")
        (expect (getf fields :message) :to-equal "YAML syntax error.")
        (expect (getf (first (getf fields :repairs)) :command)
                :to-equal "aitools schema yaml get")))))
