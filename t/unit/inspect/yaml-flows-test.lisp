(in-package #:aitools.inspect.test)

(describe "YAML read flows"
  (it "gets values converted through the YAML adapter"
    (let ((ports (make-test-ports
                  :files (list (cons "/work/doc.yaml"
                                     (format nil "name: Ada~%enabled: true~%"))))))
      (multiple-value-bind (kind fields)
          (run-flow (function yaml-get-flow) ports "doc.yaml" "")
        (expect kind :to-be :ok)
        (let ((value (field fields "value")))
          (expect (json-object-get value "name") :to-equal "Ada")
          (expect (json-object-get value "enabled") :to-be t)))
      (expect (run-flow (function yaml-get-flow) ports "doc.yaml" "" :max-bytes "4")
              :to-be :partial)
      (multiple-value-bind (kind fields)
          (run-flow (function yaml-get-flow) (make-test-ports) "missing.yaml" "")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.not-found"))
      (let* ((present (make-fake-filesystem
                       :files (list (cons "/work/vanished.yaml" "name: Ada"))
                       :directories (list "/work")))
             (gone (make-fake-filesystem :directories (list "/work")))
             (vanished-ports
               (make-inspect-ports
                :workspace-host (make-fake-host present)
                :text-source (make-fake-source gone)
                :open-store (lambda (root) (fail (format nil "open-store ~A" root)))
                :state-directory-function (constantly nil))))
        (multiple-value-bind (kind fields)
            (run-flow (function yaml-get-flow) vanished-ports "vanished.yaml" "")
          (expect kind :to-be :error)
          (expect (error-code fields) :to-equal "input.not-found")))))

  (it "selects array values through the shared query flow"
    (let* ((text (format nil "items:~%  - id: 1~%    score: 2~%  - id: 2~%    score: 3~%"))
           (ports (make-test-ports :files (list (cons "/work/items.yaml" text))))
           (scalar-ports (make-test-ports :files (list (cons "/work/scalar.yaml" "name: Ada")))))
      (multiple-value-bind (kind fields)
          (run-flow (function yaml-select-flow) ports "items.yaml" "/items"
                    :where (list "/score>2") :pick (list "/id"))
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 1)
        (expect (json-object-get (json-object-get (first (field fields "items")) "value") "/id")
                :to-be 2))
      (multiple-value-bind (kind fields)
          (run-flow (function yaml-select-flow) ports "items.yaml" "/items" :limit 1)
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 2)
        (expect (length (field fields "items")) :to-be 1))
      (multiple-value-bind (kind fields)
          (run-flow (function yaml-select-flow) scalar-ports "scalar.yaml" "/name")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "argument.invalid"))))

  (it "diffs YAML values through the shared diff flow"
    (let ((ports (make-test-ports
                  :files (list (cons "/work/a.yaml" (format nil "first: 1~%second: 2~%"))
                               (cons "/work/b.yaml" (format nil "first: 3~%second: 4~%"))
                               (cons "/work/bad.yaml" "value: [1,")))))
      (multiple-value-bind (kind fields)
          (run-flow (function yaml-diff-flow) ports "a.yaml" "b.yaml" :limit 1)
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 2)
        (expect (length (field fields "ops")) :to-be 1))
      (multiple-value-bind (kind fields)
          (run-flow (function yaml-diff-flow) ports "a.yaml" "bad.yaml")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.syntax-error"))))

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
