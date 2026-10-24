;;;; t/unit/inspect/yaml-values-test.lisp
(in-package #:aitools.inspect.test)

(defun %assert-yaml-unsupported (text reason pointer)
  (handler-case
      (progn
        (parse-yaml-document text)
        (fail "expected an unsupported YAML value"))
    (yaml-unsupported-value (condition)
      (expect (yaml-unsupported-reason condition) :to-equal reason)
      (expect (yaml-unsupported-pointer condition) :to-equal pointer))))

(describe "YAML value mapping"
  (it "maps Core scalars and keeps mapping order"
    (let ((value (parse-yaml-document
                  (format nil "text: yes~%null_value: null~%true_value: true~%false_value: false~%"))))
      (expect (mapcar #'car (json-object-pairs value))
              :to-equal '("text" "null_value" "true_value" "false_value"))
      (expect (json-object-get value "text") :to-equal "yes")
      (expect (json-null-value-p (json-object-get value "null_value")) :to-be t)
      (expect (json-object-get value "true_value") :to-be t)
      (expect (json-false-value-p (json-object-get value "false_value")) :to-be t)))

  (it "resolves explicit Core tags and expands non-cyclic aliases"
    (let ((value (parse-yaml-document
                  (format nil
                          "text: !!str true~%number: !!int \"42\"~%copy: &source {value: 3}~%alias: *source~%fallback: ! text~%"))))
      (expect (json-object-get value "text") :to-equal "true")
      (expect (json-object-get value "number") :to-be 42)
      (expect (json-object-get (json-object-get value "alias") "value") :to-be 3)
      (expect (json-object-get value "fallback") :to-equal "text")))

  (it "accepts simple base strings"
    (let ((text (coerce "answer: 42" 'simple-base-string)))
      (expect (json-object-get (parse-yaml-document text) "answer") :to-be 42)))

  (it "rejects explicit tags, aliases with cycles, duplicate and non-string keys"
    (%assert-yaml-unsupported "x: !custom value" "explicit-tag" "/x")
    (%assert-yaml-unsupported "loop: &loop [*loop]" "anchor-or-alias" "/loop/0")
    (%assert-yaml-unsupported "outer: {x: 1, x: 2}" "duplicate-key" "/outer")
    (%assert-yaml-unsupported "? [a, b]: c" "non-string-key" ""))

  (it "rejects multiple documents and non-finite numbers"
    (%assert-yaml-unsupported (format nil "x: 1~%---~%x: 2~%") "document-count" "")
    (%assert-yaml-unsupported "x: .inf" "non-finite-float" "/x")
    (%assert-yaml-unsupported "x: -.inf" "non-finite-float" "/x")
    (%assert-yaml-unsupported "x: .nan" "non-finite-float" "/x"))

  (it "maps malformed YAML to a syntax condition"
    (signals yaml-syntax-error (parse-yaml-document "x: [1,")))

  (cl-weave:it-property
   "round-trips generated strings through the YAML Core parser"
   ((value (cl-weave:gen-string
            :min-length 0
            :max-length 80
            :alphabet (format nil "aB09\"\\~%~C" #\Tab))))
   (expect (parse-yaml-document (render-json value)) :to-equal value)))

(describe "YAML write validation"
  (it "validates JSON values and reports source comment spans"
    (multiple-value-bind (value comments)
        (parse-yaml-write-document (format nil "# note~%items:~%- 1~%- true~%"))
      (let ((items (json-object-get value "items"))) (expect (length items) :to-be 2) (expect (aref items 0) :to-be 1) (expect (aref items 1) :to-be t))
      (expect comments :to-be 1)))
  (it "rejects YAML constructs that cannot be represented as JSON"
    (labels ((assert-unsupported (text reason pointer)
               (handler-case
                   (progn
                     (parse-yaml-write-document text)
                     (fail "expected unsupported YAML write value"))
                 (yaml-unsupported-value (condition)
                   (expect (yaml-unsupported-reason condition) :to-equal reason)
                   (expect (yaml-unsupported-pointer condition) :to-equal pointer)))))
      (assert-unsupported "x: !custom value" "explicit-tag" "/x")
      (assert-unsupported "x: &a 1" "anchor-or-alias" "/x")
      (assert-unsupported (format nil "x: 1~%x: 2") "duplicate-key" "")
      (assert-unsupported "? [a, b]: c" "non-string-key" "")
      (assert-unsupported "x: .inf" "non-finite-float" "/x")
      (assert-unsupported (format nil "x: 1~%---~%x: 2~%") "document-count" "")))
  (it "maps malformed YAML to a syntax condition in write mode"
    (signals yaml-syntax-error (parse-yaml-write-document "x: ["))))
