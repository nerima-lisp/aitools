;;;; YAML write document validation and conversion.
(in-package #:aitools.edit.test)

(defun yaml-write-refusal (text)
  (handler-case
      (progn (parse-yaml-write-document text) nil)
    (edit-refusal (condition) condition)))

(describe "aitools.edit.domain YAML write documents"
  (it "maps ordered values and counts only lexer-recognized comments"
    (multiple-value-bind (value comments)
        (parse-yaml-write-document
         (format nil "# head~%name: \"A\" # inline~%text: |~%  # scalar text~%# tail~%"))
      (expect (serialize-json value) :to-equal
              "{\"name\":\"A\",\"text\":\"# scalar text\n\"}")
      (expect comments :to-be 3)))
  (it "rejects nonzero mantissa underflow and accepts exponent-form zero"
    (let ((underflow (parse-json-text "{\"value\":1e-9999}"))
          (zero (parse-json-text "{\"value\":0e-9999}")))
      (dolist (serializer (list #'serialize-yaml-document
                                #'serialize-toml-document))
        (expect (refusal-code (lambda () (funcall serializer underflow)))
                :to-equal "argument.invalid")
        (expect (stringp (funcall serializer zero)) :to-be t))))

  (it "rejects custom, core, and invalid core tags before edits"
      (let ((tagged (yaml-write-refusal (format nil "keep: !custom text~%")))
            (core-tagged (yaml-write-refusal (format nil "keep: !!str text~%")))
            (invalid-core-tag
             (yaml-write-refusal (format nil "keep: !!int nope~%")))
            (anchored
             (yaml-write-refusal (format nil "a: &shared 1~%b: *shared~%"))))
        (expect (edit-refusal-code tagged) :to-equal "input.unsupported-value")
        (expect (edit-refusal-code core-tagged) :to-equal "input.unsupported-value")
        (expect (edit-refusal-code invalid-core-tag) :to-equal "input.unsupported-value")
        (expect (edit-refusal-detail tagged) :to-equal
                "YAML value at JSON Pointer /keep uses unsupported explicit-tag.")
        (expect (edit-refusal-detail core-tagged) :to-equal
                "YAML value at JSON Pointer /keep uses unsupported explicit-tag.")
        (expect (edit-refusal-detail invalid-core-tag) :to-equal
                "YAML value at JSON Pointer /keep uses unsupported explicit-tag.")
        (expect (edit-refusal-detail anchored) :to-equal
                "YAML value at JSON Pointer /a uses unsupported anchor-or-alias.")))

  (it "reports duplicate and non-string mapping keys at their containing map"
    (let ((duplicate (yaml-write-refusal "a: 1\na: 2\n"))
          (non-string (yaml-write-refusal "? [x, y]\n: 1\n")))
      (expect (edit-refusal-detail duplicate) :to-equal
              "YAML value at JSON Pointer  uses unsupported duplicate-key.")
      (expect (edit-refusal-detail non-string) :to-equal
              "YAML value at JSON Pointer  uses unsupported non-string-key.")))

  (it "serializes the JSON edit value model and rejects unrepresentable numbers"
    (let ((value (parse-json-text "{\"b\":[true,false],\"a\":null}"))
          (invalid (handler-case
                       (progn (serialize-yaml-document
                               (parse-json-text "{\"value\":1e9999}"))
                              nil)
                     (edit-refusal (condition) condition))))
      (expect (stringp (serialize-yaml-document value)) :to-be t)
      (expect (edit-refusal-code invalid) :to-equal "argument.invalid")))
  (it "rejects multiple documents and non-finite floats"
    (let ((multiple (yaml-write-refusal "---\na: 1\n---\nb: 2\n"))
          (non-finite (yaml-write-refusal "value: .inf\n")))
      (expect (edit-refusal-code multiple) :to-equal "input.unsupported-value")
      (expect (edit-refusal-detail multiple) :to-equal
              "YAML value at JSON Pointer  uses unsupported document-count.")
      (expect (edit-refusal-code non-finite) :to-equal "input.unsupported-value")
      (expect (edit-refusal-detail non-finite) :to-equal
              "YAML value at JSON Pointer /value uses unsupported non-finite-float."))))
