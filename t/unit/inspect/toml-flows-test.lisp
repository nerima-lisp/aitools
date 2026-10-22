(in-package #:aitools.inspect.test)

(describe "TOML read flows"
  (it "gets values converted through the TOML adapter"
    (let ((ports (make-test-ports
                  :files (list (cons "/work/doc.toml"
                                     (format nil "name = \"Ada\"~%enabled = true~%date = 1979-05-27~%"))))))
      (multiple-value-bind (kind fields)
          (run-flow (function toml-get-flow) ports "doc.toml" "")
        (expect kind :to-be :ok)
        (let ((value (field fields "value")))
          (expect (json-object-get value "name") :to-equal "Ada")
          (expect (json-object-get value "enabled") :to-be t)
          (expect (json-object-get value "date") :to-equal "1979-05-27")))
      (expect (run-flow (function toml-get-flow) ports "doc.toml" "" :max-bytes "4")
              :to-be :partial)
      (multiple-value-bind (kind fields)
          (run-flow (function toml-get-flow) (make-test-ports) "missing.toml" "")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.not-found"))
      (let* ((present (make-fake-filesystem
                       :files (list (cons "/work/vanished.toml" "name = \"Ada\""))
                       :directories (list "/work")))
             (gone (make-fake-filesystem :directories (list "/work")))
             (vanished-ports
               (make-inspect-ports
                :workspace-host (make-fake-host present)
                :text-source (make-fake-source gone)
                :open-store (lambda (root) (fail (format nil "open-store ~A" root)))
                :state-directory-function (constantly nil))))
        (multiple-value-bind (kind fields)
            (run-flow (function toml-get-flow) vanished-ports "vanished.toml" "")
          (expect kind :to-be :error)
          (expect (error-code fields) :to-equal "input.not-found")))))

  (it "selects array values through the shared query flow"
    (let* ((text (format nil "[[items]]~%id = 1~%score = 2~%~%[[items]]~%id = 2~%score = 3~%"))
           (ports (make-test-ports :files (list (cons "/work/items.toml" text))))
           (scalar-ports (make-test-ports :files (list (cons "/work/scalar.toml" "name = \"Ada\"")))))
      (multiple-value-bind (kind fields)
          (run-flow (function toml-select-flow) ports "items.toml" "/items"
                    :where (list "/score>2") :pick (list "/id"))
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 1)
        (expect (json-object-get (json-object-get (first (field fields "items")) "value") "/id")
                :to-be 2))
      (multiple-value-bind (kind fields)
          (run-flow (function toml-select-flow) ports "items.toml" "/items" :limit 1)
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 2)
        (expect (length (field fields "items")) :to-be 1))
      (multiple-value-bind (kind fields)
          (run-flow (function toml-select-flow) scalar-ports "scalar.toml" "/name")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "argument.invalid"))))

  (it "diffs TOML values through the shared diff flow"
    (let ((ports (make-test-ports
                  :files (list (cons "/work/a.toml" (format nil "first = 1~%second = 2~%"))
                               (cons "/work/b.toml" (format nil "first = 3~%second = 4~%"))
                               (cons "/work/bad.toml" "value = [")))))
      (multiple-value-bind (kind fields)
          (run-flow (function toml-diff-flow) ports "a.toml" "b.toml" :limit 1)
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 2)
        (expect (length (field fields "ops")) :to-be 1))
      (multiple-value-bind (kind fields)
          (run-flow (function toml-diff-flow) ports "a.toml" "bad.toml")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.syntax-error"))))

  (it "maps unsupported values to the TOML read error and schema repair"
    (let ((ports (make-test-ports
                  :files '(("/work/bad.toml" . "outer = { value = inf }")))))
      (multiple-value-bind (kind fields)
          (run-flow #'toml-get-flow ports "bad.toml" "")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.unsupported-value")
        (expect (getf fields :message) :to-contain "JSON Pointer /outer/value")
        (expect (getf (first (getf fields :repairs)) :command)
                :to-equal "aitools schema toml get"))))

  (it "maps malformed TOML to the syntax error and schema repair"
    (let ((ports (make-test-ports
                  :files '(("/work/bad.toml" . "value = [")))))
      (multiple-value-bind (kind fields)
          (run-flow #'toml-get-flow ports "bad.toml" "")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.syntax-error")
        (expect (getf fields :message) :to-equal "TOML syntax error.")
        (expect (getf (first (getf fields :repairs)) :command)
                :to-equal "aitools schema toml get")))))
