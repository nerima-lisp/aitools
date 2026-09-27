;;;; t/unit/protocol/schema-flow-test.lisp
(in-package #:aitools.protocol.test)

(describe "aitools.protocol.application schema rendering"
  (it "renders only name and summary for the list view"
    (let* ((schema (make-command-schema "read" "Read a file."
                                        :description "Long description."
                                        :error-codes (list "input.not-found")))
           (summary (render-command-summary schema)))
      (expect (json-alist-value summary "name") :to-equal "read")
      (expect (json-alist-value summary "summary") :to-equal "Read a file.")
      (expect (assoc "description" (json-alist summary) :test #'string=) :to-be-falsy)))

  (it "renders full detail including args, output fields, and error codes"
    (let* ((schema (make-command-schema
                    "read" "Read a file."
                    :description "Long description."
                    :args (list (list :name "path" :type "string" :required t))
                    :output-fields (list (list :name "lines" :description "the lines read"))
                    :error-codes (list "input.not-found")))
           (detail (render-command-detail schema)))
      (expect (json-alist-value detail "description") :to-equal "Long description.")
      (expect (json-alist-value detail "error_codes") :to-equal (list "input.not-found"))
      (let ((arg (first (json-alist-value detail "args"))))
        (expect (json-alist-value arg "name") :to-equal "path")
        (expect (json-alist-value arg "type") :to-equal "string")
        (expect (json-alist-value arg "required") :to-be t))
      (let ((field (first (json-alist-value detail "output_fields"))))
        (expect (json-alist-value field "name") :to-equal "lines"))))

  (it "falls back to the summary when no long description was given"
    (let* ((schema (make-command-schema "read" "Read a file."))
           (detail (render-command-detail schema)))
      (expect (json-alist-value detail "description") :to-equal "Read a file."))))
