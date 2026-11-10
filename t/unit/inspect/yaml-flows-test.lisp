(in-package #:aitools.inspect.test)

(describe "YAML read flows"
  (it "gets values converted through the YAML adapter"
    (let ((boundaries (make-test-boundaries
                  :files (list (cons "/work/doc.yaml"
                                     (format nil "name: Ada~%enabled: true~%"))))))
      (multiple-value-bind (kind fields)
          (run-flow (function json-get-flow) boundaries "doc.yaml" "" :format "yaml")
        (expect kind :to-be :ok)
        (let ((value (field fields "value")))
          (expect (json-object-get value "name") :to-equal "Ada")
          (expect (json-object-get value "enabled") :to-be t)))
      (expect (run-flow (function json-get-flow) boundaries "doc.yaml" "" :max-bytes "4" :format "yaml")
              :to-be :partial)
      (multiple-value-bind (kind fields)
          (run-flow (function json-get-flow) (make-test-boundaries) "missing.yaml" "" :format "yaml")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.not-found"))
      (let* ((present (make-fake-filesystem
                       :files (list (cons "/work/vanished.yaml" "name: Ada"))
                       :directories (list "/work")))
             (gone (make-fake-filesystem :directories (list "/work")))
             (vanished-boundaries
               (make-test-boundaries :workspace-host (make-fake-host present) :text-context (make-fake-text-context gone) :open-store (lambda (root) (fail (format nil "open-store ~A" root))) :state-directory nil)))
        (multiple-value-bind (kind fields)
            (run-flow (function json-get-flow) vanished-boundaries "vanished.yaml" "" :format "yaml")
          (expect kind :to-be :error)
          (expect (error-code fields) :to-equal "input.not-found")))))

  (it "selects array values through the shared query flow"
    (let* ((text (format nil "items:~%  - id: 1~%    score: 2~%  - id: 2~%    score: 3~%"))
           (boundaries (make-test-boundaries :files (list (cons "/work/items.yaml" text))))
           (scalar-boundaries (make-test-boundaries :files (list (cons "/work/scalar.yaml" "name: Ada")))))
      (multiple-value-bind (kind fields)
          (run-flow (function json-select-flow) boundaries "items.yaml" "/items"
                    :where (list "/score>2") :pick (list "/id") :format "yaml")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 1)
        (expect (json-object-get (json-object-get (first (field fields "items")) "value") "/id")
                :to-be 2))
      (multiple-value-bind (kind fields)
          (run-flow (function json-select-flow) boundaries "items.yaml" "/items" :limit 1 :format "yaml")
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 2)
        (expect (length (field fields "items")) :to-be 1))
      (multiple-value-bind (kind fields)
          (run-flow (function json-select-flow) scalar-boundaries "scalar.yaml" "/name" :format "yaml")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "argument.invalid"))))

  (it "diffs YAML values through the shared diff flow"
    (let ((boundaries (make-test-boundaries
                  :files (list (cons "/work/a.yaml" (format nil "first: 1~%second: 2~%"))
                               (cons "/work/b.yaml" (format nil "first: 3~%second: 4~%"))
                               (cons "/work/bad.yaml" "value: [1,")))))
      (multiple-value-bind (kind fields)
          (run-flow (function json-diff-flow) boundaries "a.yaml" "b.yaml" :limit 1 :format "yaml")
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 2)
        (expect (length (field fields "ops")) :to-be 1))
      (multiple-value-bind (kind fields)
          (run-flow (function json-diff-flow) boundaries "a.yaml" "bad.yaml" :format "yaml")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.syntax-error"))))

  (it "maps unsupported values to the YAML read error and schema repair"
    (let ((boundaries (make-test-boundaries
                  :files '(("/work/bad.yaml" . "outer: {x: !custom value}")))))
      (multiple-value-bind (kind fields)
          (run-flow #'json-get-flow boundaries "bad.yaml" "" :format "yaml")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.unsupported-value")
        (expect (getf fields :message) :to-contain "JSON Pointer /outer/x")
        (expect (getf (first (getf fields :repairs)) :command)
                :to-equal "aitools schema doc get"))))

  (it "maps malformed YAML to the syntax error and schema repair"
    (let ((boundaries (make-test-boundaries
                  :files '(("/work/bad.yaml" . "value: [1,")))))
      (multiple-value-bind (kind fields)
          (run-flow #'json-get-flow boundaries "bad.yaml" "" :format "yaml")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.syntax-error")
        (expect (getf fields :message) :to-equal "YAML syntax error.")
        (expect (getf (first (getf fields :repairs)) :command)
                :to-equal "aitools schema doc get")))))
