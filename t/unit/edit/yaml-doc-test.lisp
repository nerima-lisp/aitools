;;;; YAML write-value validation and number representability.
(in-package #:aitools.edit.test)

(defun yaml-write-value-refusal (value)
  (handler-case
      (progn (validate-yaml-write-value value) nil)
    (edit-refusal (condition) condition)))

(describe "aitools.edit.domain YAML write values"
  (it "accepts JSON values without changing them"
    (let ((value (parse-json-text "{\"b\":[true,false],\"a\":null}")))
      (expect (eq (validate-yaml-write-value value) value) :to-be t)))
  (it "rejects nonzero mantissa underflow and accepts exponent-form zero"
    (let ((underflow (yaml-write-value-refusal
                      (parse-json-text "{\"value\":1e-9999}")))
          (zero (parse-json-text "{\"value\":0e-9999}")))
      (expect (edit-refusal-code underflow) :to-equal "argument.invalid")
      (expect (eq (validate-yaml-write-value zero) zero) :to-be t)))
  (it "rejects unrepresentable JSON numbers"
    (let ((overflow (yaml-write-value-refusal
                     (make-json-num "1e9999"))))
      (expect (edit-refusal-code overflow) :to-equal "argument.invalid")))
  (it "rejects malformed JSON number tokens"
    (let ((refusal
            (yaml-write-value-refusal (make-json-num "1 trailing"))))
      (expect (edit-refusal-code refusal) :to-equal "argument.invalid"))))

(defun expect-yaml-comment-warning (fields count)
  (let ((warnings (field fields "warnings")))
    (expect (vectorp warnings) :to-be t)
    (expect (length warnings) :to-be (if (plusp count) 1 0))
    (when (plusp count)
      (let ((warning (aref warnings 0)))
        (expect (json-field warning "code") :to-equal "format.comments-dropped")
        (expect (json-field warning "message") :to-equal
                "Comments were dropped when the document was rewritten.")
        (expect (json-field warning "count") :to-be count)))))

(describe "aitools.edit YAML write flows"
  (it "applies all five JSON operations, supports stdin sets, and always returns warnings"
    (call-with-edit-workspace (lambda () (put "doc.yaml" (format nil "# input~%a: 1~%b: 2~%"))
      (multiple-value-bind (status fields)
          (run "doc.set" (list "doc.yaml" "/a" "3"))
        (expect status :to-be :ok)
        (expect-yaml-comment-warning fields 1))
      (multiple-value-bind (status fields)
          (run "doc.delete" (list "doc.yaml" "/b"))
        (expect status :to-be :ok)
        (expect-yaml-comment-warning fields 0))
      (multiple-value-bind (status fields)
          (run "doc.merge" (list "doc.yaml") :stdin-data "{\"c\":4}")
        (expect status :to-be :ok)
        (expect-yaml-comment-warning fields 0))
      (multiple-value-bind (status fields)
          (run "doc.patch" (list "doc.yaml")
               :stdin-data "[{\"op\":\"replace\",\"path\":\"/c\",\"value\":5}]")
        (expect status :to-be :ok)
        (expect-yaml-comment-warning fields 0))
      (put "fmt.yaml" (format nil "# keep count~%x: 1~%"))
      (multiple-value-bind (status fields)
          (run "doc.fmt" (list "fmt.yaml") :dry-run t)
        (expect status :to-be :ok)
        (expect-yaml-comment-warning fields 1)
        (expect (text "fmt.yaml") :to-equal (format nil "# keep count~%x: 1~%")))
      (multiple-value-bind (status fields)
          (run "doc.fmt" (list "fmt.yaml"))
        (expect status :to-be :ok)
        (expect-yaml-comment-warning fields 1))
      (multiple-value-bind (status fields)
          (run "doc.fmt" (list "fmt.yaml"))
        (expect status :to-be :ok)
        (expect-yaml-comment-warning fields 0)
        (expect (changes fields) :to-equal (list)))
      (put "nested.yaml" (format nil "key: old~%"))
      (multiple-value-bind (status fields)
          (run "doc.set" (list "nested.yaml")
               :stdin-data "{\"pointer\":\"/profile\",\"value\":{\"label\":\"text\",\"enabled\":false,\"missing\":null,\"items\":[1,true]}}"
               :expect-hash (list (hash "nested.yaml")))
        (expect status :to-be :ok)
        (expect-yaml-comment-warning fields 0))
      (let* ((value (aitools.inspect.domain:parse-yaml-document (text "nested.yaml")))
             (profile (aitools.inspect.domain:json-object-get value "profile"))
             (items (aitools.inspect.domain:json-object-get profile "items")))
        (expect (aitools.inspect.domain:json-object-get profile "label") :to-equal "text")
        (expect (aitools.inspect.domain:json-false-value-p
                 (aitools.inspect.domain:json-object-get profile "enabled"))
                :to-be t)
        (expect (aitools.inspect.domain:json-null-value-p
                 (aitools.inspect.domain:json-object-get profile "missing"))
                :to-be t)
        (expect (length items) :to-be 2)
        (expect (aref items 0) :to-be 1)
        (expect (aref items 1) :to-be t)))))
  (it "rejects nonzero mantissa underflow and accepts exponent-form zero"
    (call-with-edit-workspace (lambda () (put "doc.yaml" (format nil "a: 1~%"))
      (multiple-value-bind (status fields)
          (run "doc.set" '("doc.yaml" "/a" "0e-9999"))
        (expect status :to-be :ok)
        (expect-yaml-comment-warning fields 0))
      (multiple-value-bind (status code message keys)
          (run "doc.set" '("doc.yaml" "/a" "1e-9999"))
        (declare (ignore message keys))
        (expect status :to-be :error)
        (expect code :to-equal "argument.invalid")))))
  (it "adds a final LF when serialization omits it"
    (let ((document
            (aitools.edit.application::%format-document
             "yaml" nil nil 0
             (lambda (format value)
               (declare (ignore format value))
               "document"))))
      (expect (doc-string document) :to-equal (format nil "document~%"))))
  (it "rejects unsupported serializer formats"
    (signals error
      (aitools.edit.infrastructure::serialize-document "other" nil)))
  (it "rejects JSON null at the TOML serializer boundary"
    (signals error
      (aitools.edit.infrastructure::serialize-document
       "toml"
       (aitools.protocol.domain:json-null))))
  (it "uses the schema repair fallback for an unsupported YAML patch operation"
    (call-with-edit-workspace
     (lambda ()
       (put "patch.yaml" (format nil "a: 1~%"))
       (multiple-value-bind (status code message keys)
           (run "doc.patch" (list "patch.yaml")
                :stdin-data "[{\"op\":\"unknown\",\"path\":\"/a\"}]")
         (declare (ignore message))
         (expect status :to-be :error)
         (expect code :to-equal "argument.invalid")
         (expect (getf (first (getf keys :repairs)) :command)
                 :to-equal "aitools schema doc patch")))))

  (it "rejects invalid source documents and carries format repairs"
    (call-with-edit-workspace (lambda () (put "bad-tag.yaml" (format nil "keep: !custom value~%target: 2~%"))
      (multiple-value-bind (status code message keys)
          (run "doc.delete" (list "bad-tag.yaml" "/target"))
        (declare (ignore message))
        (expect status :to-be :error)
        (expect code :to-equal "input.unsupported-value")
        (expect (getf (first (getf keys :repairs)) :command)
                :to-equal "aitools schema doc delete"))
      (put "bad-format.yaml" (format nil "value: [~%"))
      (multiple-value-bind (status code message keys)
          (run "doc.fmt" (list "bad-format.yaml"))
        (declare (ignore message))
        (expect status :to-be :error)
        (expect code :to-equal "input.syntax-error")
        (expect (getf (first (getf keys :repairs)) :command)
                :to-equal "aitools schema doc fmt"))
      (put "missing-member.yaml" (format nil "present: true~%"))
      (multiple-value-bind (status code message keys)
          (run "doc.delete" (list "missing-member.yaml" "/missing"))
        (declare (ignore message))
        (expect status :to-be :error)
        (expect code :to-equal "input.not-found")
        (expect (plusp (length (getf keys :repairs))) :to-be t))
      (put "failed-patch.yaml" (format nil "value: 1~%"))
      (multiple-value-bind (status code message keys)
          (run "doc.patch" (list "failed-patch.yaml")
               :stdin-data "[{\"op\":\"test\",\"path\":\"/value\",\"value\":2}]")
        (declare (ignore message))
        (expect status :to-be :error)
        (expect code :to-equal "selection.no-match")
        (expect (plusp (length (getf keys :repairs))) :to-be t))
      (multiple-value-bind (status code message keys)
          (run "doc.fmt" (list "absent.yaml"))
        (declare (ignore message keys))
        (expect status :to-be :error)
        (expect code :to-equal "input.not-found"))))))
