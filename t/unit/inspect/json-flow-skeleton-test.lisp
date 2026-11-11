(in-package #:aitools.inspect.test)

(describe "JSON document flow skeletons"
  (it "expands shared error forwarding around the flow body"
    (let ((expansion
            (macroexpand-1
             '(aitools.inspect.application::%with-json-flow-error-forwarder
               (marker)))))
      (expect (first expansion) :to-be 'labels)
      (expect (first (first (second expansion)))
              :to-be 'aitools.inspect.application::report-error)
      (expect (third expansion) :to-equal '(marker))))

  (it "expands document commands with their declared constructor inputs"
    (let* ((expansion
             (macroexpand-1
              '(aitools.inspect.presentation::%define-doc-command
                doc-command "get"
                :positionals positionals
                :options options
                :handler handler)))
           (constructor (fourth expansion))
           (description (getf (rest constructor) :description)))
      (expect (first expansion) :to-be 'defun)
      (expect (second expansion) :to-be 'doc-command)
      (expect (first constructor) :to-be 'cl-cli:make-command)
      (expect (getf (rest constructor) :name) :to-equal "get")
      (expect (second description) :to-equal "doc.get")
      (expect (getf (rest constructor) :positionals) :to-be 'positionals)
      (expect (getf (rest constructor) :options) :to-be 'options)
      (expect (getf (rest constructor) :handler) :to-be 'handler)))

  (it "expands the shared document reader with both result continuations"
    (let ((expansion
            (macroexpand-1
             '(aitools.inspect.application::%call-document-reader
               (read-document)))))
      (expect (first expansion) :to-be 'aitools.kernel.domain:flow-call)
      (expect (second expansion) :to-equal '(read-document))
      (expect (first (third expansion)) :to-be :on-document)
      (expect (first (fourth expansion)) :to-be :on-error)))

  (it "returns the entire JSON text when it fits within the byte limit"
    (expect (aitools.inspect.application::%utf8-prefix "short" 5)
            :to-equal "short")))
(describe "JSON flow defaults and pointer validation"
  (it "uses the default JSON format and byte limit for get"
    (let ((boundaries (make-test-boundaries
                       :files '(("/work/doc.json" . "{\"name\": \"Ada\"}")))))
      (multiple-value-bind (kind fields)
          (run-flow #'json-get-flow boundaries "doc.json" "/name")
        (expect kind :to-be :ok)
        (expect (field fields "value") :to-equal "Ada"))))
  (it "uses the default select output and limit"
    (let ((boundaries
            (make-test-boundaries
             :files '(("/work/items.json" . "{\"items\":[{\"id\":1}]}")))))
      (multiple-value-bind (kind fields)
          (run-flow #'json-select-flow boundaries "items.json" "/items")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 1)
        (expect (json-object-get (first (field fields "items")) "pointer")
                :to-equal "/items/0")
        (expect (json-object-get
                 (json-object-get (first (field fields "items")) "value")
                 "id")
                :to-be 1))))
  (it-each (("pick" :pick ("id"))
            ("sort-by" :sort-by "id"))
      "rejects a malformed relative --~A pointer"
      (label option value)
    (declare (ignore label))
    (multiple-value-bind (kind fields)
        (apply #'run-flow #'json-select-flow (make-test-boundaries)
               "missing.json" "" (list option value))
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (getf fields :message) :to-contain "is not a relative JSON pointer")
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools schema doc select")))
  (it "uses the default format and operation limit for diff"
    (let ((boundaries
            (make-test-boundaries
             :files '(("/work/a.json" . "{\"x\":1}")
                      ("/work/b.json" . "{\"x\":2}")))))
      (multiple-value-bind (kind fields)
          (run-flow #'json-diff-flow boundaries "a.json" "b.json")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 1)
        (expect (json-false-value-p (field fields "truncated")) :to-be t))))
  (it "expands shared document flow result forwarding"
    (let ((expansion
            (macroexpand-1
             '(aitools.inspect.application::%call-doc-inspect-flow
               (marker)))))
      (expect (first expansion) :to-be 'aitools.kernel.domain:flow-call)
      (expect (second expansion) :to-equal '(marker))
      (expect (first (third expansion)) :to-be :on-ok)
      (expect (first (fourth expansion)) :to-be :on-partial)
      (expect (first (fifth expansion)) :to-be :on-error))))
