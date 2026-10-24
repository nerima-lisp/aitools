;;;; t/integration/inspect-yaml-toml-read-test.lisp
;;;; YAML and TOML inspection commands through dispatch and their published schemas.
(in-package #:aitools.integration.inspect-cli-test)

(defun expect-format-read-error (workspace format path code reason &optional pointer)
  (multiple-value-bind (exit envelope stream)
      (run-aitools "--root" workspace format "get" path "")
    (expect (list exit stream) :to-equal '(1 :stderr))
    (expect envelope :to-match-envelope "status" "error"
            "command" (format nil "~A get" format))
    (expect (value-at envelope "schema_version") :to-be 2)
    (let ((actual-code (value-at envelope "error" "code")))
      (unless (equal actual-code code)
        (fail "~A read of ~A: expected error code ~A, got ~A"
              format path code actual-code)))
    (expect (value-at envelope "error" "code") :to-equal code)
    (expect (value-at envelope "error" "exit_code") :to-be 1)
    (expect (value-at envelope "error" "message")
            :to-equal (if (string= code "input.syntax-error")
                          reason
                          (format nil "~A value at JSON Pointer ~A uses unsupported ~A."
                                  (if (string= format "yaml") "YAML" "TOML") pointer reason)))
    (expect (value-at envelope "error" "repairs" 0 "command")
            :to-equal (format nil "aitools --root ~A schema ~A get" workspace format))
    (expect (value-at envelope "error" "repairs" 0 "action")
            :to-equal (if (string= code "input.syntax-error")
                          "fix-format-syntax"
                          "convert-to-supported-value"))
    (expect (value-at envelope "error" "repairs" 0 "detail")
            :to-equal (if (string= code "input.syntax-error")
                          "Fix the YAML/TOML syntax or TOML integer range, then retry."
                          "Convert the value to the supported subset, then retry."))))

(describe "YAML and TOML schemas"
  (it "lists read and write commands with errors and output fields"
  (dolist (format '("yaml" "toml"))
    (multiple-value-bind (code envelope) (run-aitools "schema" format)
      (expect code :to-be 0)
      (let* ((commands (coerce (value-at envelope "commands") 'list))
             (names (sort (mapcar (lambda (command) (value-at command "name")) commands) #'string<)))
        (expect names :to-equal
                (mapcar (lambda (subcommand) (format nil "~A ~A" format subcommand))
                        '("delete" "diff" "fmt" "get" "merge" "patch" "select" "set")))
        (dolist (command commands)
          (let ((name (value-at command "name"))
                (errors (coerce (value-at command "error_codes") 'list))
                (fields (mapcar (lambda (field) (value-at field "name"))
                                (coerce (value-at command "output_fields") 'list))))
            (expect errors :to-contain "input.unsupported-value")
            (expect errors :to-contain "input.syntax-error")
            (expect fields :to-contain
                    (cond ((search " get" name) "value")
                          ((search " select" name) "items") ((search " diff" name) "ops")
                          (t "changes")))
            (when (member (subseq name 5) '("set" "delete" "merge" "patch" "fmt")
                          :test #'string=)
              (expect fields :to-contain "warnings"))))))))

(describe "YAML and TOML read commands through dispatch"
  (it "maps values, filters, diffs, and leaves source bytes unchanged"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let* ((yaml (ws-file workspace "items.yaml"
                            (format nil "- id: 1~%  n: 5~%  k: a~%- id: 2~%  n: 9~%  k: a~%- id: 3~%  n: 7~%  k: b~%")))
             (yaml-old (ws-file workspace "old.yaml" "{x: 1, y: 2}"))
             (yaml-new (ws-file workspace "new.json" "{\"x\": 2}"))
             (yaml-reordered (ws-file workspace "reordered.yaml" "{y: 2, x: 1}"))
             (toml (ws-file workspace "items.toml"
                            (format nil
                                    (concatenate 'string
                                                 "[[items]]~%id = 1~%n = 5~%k = \"a\"~%~%"
                                                 "[[items]]~%id = 2~%n = 9~%k = \"a\"~%~%"
                                                 "[[items]]~%id = 3~%n = 7~%k = \"b\"~%"))))
             (toml-old (ws-file workspace "old.toml" (format nil "x = 1~%y = 2~%")))
             (toml-new (ws-file workspace "new.toml" (format nil "x = 2~%")))
             (toml-reordered (ws-file workspace "reordered.toml" (format nil "y = 2~%x = 1~%")))
             (before-yaml (aitools.test.support:read-bytes yaml))
             (before-old (aitools.test.support:read-bytes yaml-old))
             (before-new (aitools.test.support:read-bytes yaml-new))
             (before-toml (aitools.test.support:read-bytes toml))
             (before-toml-old (aitools.test.support:read-bytes toml-old))
             (before-toml-new (aitools.test.support:read-bytes toml-new))
             (before-yaml-reordered (aitools.test.support:read-bytes yaml-reordered))
             (before-toml-reordered (aitools.test.support:read-bytes toml-reordered)))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "yaml" "get" yaml "/0")
          (expect code :to-be 0)
          (expect envelope :to-match-envelope "status" "ok" "command" "yaml get")
          (expect (value-at envelope "value" "n") :to-be 5))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "yaml" "get" yaml "/0" "--keys")
          (expect code :to-be 0)
          (expect (coerce (value-at envelope "keys") 'list) :to-equal '("id" "n" "k")))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "yaml" "get" yaml "/0/k" "--raw")
          (expect code :to-be 0)
          (expect (value-at envelope "text") :to-equal "a"))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "yaml" "get" yaml "/0" "--max-bytes" "1")
          (expect code :to-be 3)
          (expect (value-at envelope "truncated") :to-be t))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "yaml" "select" yaml "" "--where" "/n>4"
                         "--where" "/k=a" "--pick" "/id" "--pick" "/n"
                         "--sort-by" "/n" "--desc" "--limit" "1")
          (expect code :to-be 3)
          (expect envelope :to-match-envelope "status" "partial" "command" "yaml select")
          (expect (value-at envelope "total") :to-be 2)
          (expect (value-at envelope "items" 0 "value" "/id") :to-be 2))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "yaml" "select" yaml "" "--where" "/k=a"
                         "--output" "count")
          (expect code :to-be 0)
          (expect (value-at envelope "count") :to-be 2))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "yaml" "diff" yaml-old yaml-new "--limit" "1")
          (expect code :to-be 3)
          (expect (value-at envelope "total") :to-be 2)
          (expect (length (value-at envelope "ops")) :to-be 1))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "toml" "get" toml "/items/0")
          (expect code :to-be 0)
          (expect envelope :to-match-envelope "status" "ok" "command" "toml get")
          (expect (value-at envelope "value" "id") :to-be 1))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "toml" "get" toml "/items/0" "--keys")
          (expect code :to-be 0)
          (expect (coerce (value-at envelope "keys") 'list) :to-equal '("id" "n" "k")))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "toml" "get" toml "/items/0/k" "--raw")
          (expect code :to-be 0)
          (expect (value-at envelope "text") :to-equal "a"))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "toml" "get" toml "/items/0" "--max-bytes" "1")
          (expect code :to-be 3)
          (expect (value-at envelope "truncated") :to-be t))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "toml" "select" toml "/items" "--where" "/n>4"
                         "--where" "/k=a" "--pick" "/id" "--pick" "/n"
                         "--sort-by" "/n" "--desc" "--limit" "1")
          (expect code :to-be 3)
          (expect envelope :to-match-envelope "status" "partial" "command" "toml select")
          (expect (value-at envelope "total") :to-be 2)
          (expect (value-at envelope "items" 0 "value" "/id") :to-be 2))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "toml" "diff" toml-old toml-new "--limit" "1")
          (expect code :to-be 3)
          (expect envelope :to-match-envelope "status" "partial" "command" "toml diff")
          (expect (value-at envelope "total") :to-be 2)
          (expect (length (value-at envelope "ops")) :to-be 1))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "yaml" "diff" yaml-old yaml-reordered)
          (expect code :to-be 0)
          (expect (value-at envelope "identical") :to-be t))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "toml" "diff" toml-old toml-reordered)
          (expect code :to-be 0)
          (expect (value-at envelope "identical") :to-be t))
        (expect (aitools.test.support:read-bytes yaml) :to-equalp before-yaml)
        (expect (aitools.test.support:read-bytes yaml-old) :to-equalp before-old)
        (expect (aitools.test.support:read-bytes yaml-new) :to-equalp before-new)
        (expect (aitools.test.support:read-bytes toml) :to-equalp before-toml)
        (expect (aitools.test.support:read-bytes toml-old) :to-equalp before-toml-old)
        (expect (aitools.test.support:read-bytes toml-new) :to-equalp before-toml-new)
        (expect (aitools.test.support:read-bytes yaml-reordered) :to-equalp before-yaml-reordered)
        (expect (aitools.test.support:read-bytes toml-reordered) :to-equalp before-toml-reordered))))

  (it "maps YAML Core scalars, standard tags, anchors, and TOML datetimes"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let* ((yaml (ws-file workspace "values.yaml"
                            (format nil
                                    (concatenate 'string
                                                 "plain: text~%null_value: null~%true_value: true~%false_value: false~%"
                                                 "decimal: 42~%octal: 0o52~%hex: 0x2A~%finite: 1.25~%"
                                                 "explicit:~%  string: !!str true~%  integer: !!int \"42\"~%"
                                                 "  floating: !!float \"1.5\"~%  boolean: !!bool \"false\"~%"
                                                 "  null_value: !!null \"\"~%  sequence: !!seq [one, two]~%"
                                                 "  mapping: !!map {key: value}~%"
                                                 "anchor:~%  source: &shared {x: 1}~%  copy: *shared~%"
                                                 "merge:~%  <<: literal~%"))))
             (toml (ws-file workspace "dates.toml"
                            (format nil
                                    (concatenate 'string
                                                 "offset = 1979-05-27T07:32:00.123456789+00:00~%"
                                                 "local_datetime = 1979-05-27T07:32:00.123456789~%"
                                                 "local_date = 1979-05-27~%"
                                                 "local_time = 07:32:00.123456789123~%")))))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "yaml" "get" yaml "")
          (expect code :to-be 0)
          (expect (value-at envelope "value" "plain") :to-equal "text")
          (expect (value-at envelope "value" "null_value") :to-be json-kit:+json-null+)
          (expect (value-at envelope "value" "true_value") :to-be t)
          (expect (value-at envelope "value" "false_value") :to-be json-kit:+json-false+)
          (expect (value-at envelope "value" "decimal") :to-be 42)
          (expect (value-at envelope "value" "octal") :to-be 42)
          (expect (value-at envelope "value" "hex") :to-be 42)
          (expect (value-at envelope "value" "finite") :to-be 1.25d0)
          (expect (value-at envelope "value" "explicit" "string") :to-equal "true")
          (expect (value-at envelope "value" "explicit" "integer") :to-be 42)
          (expect (value-at envelope "value" "explicit" "floating") :to-be 1.5d0)
          (expect (value-at envelope "value" "explicit" "boolean") :to-be json-kit:+json-false+)
          (expect (value-at envelope "value" "explicit" "null_value")
                  :to-be json-kit:+json-null+)
          (expect (value-at envelope "value" "explicit" "sequence" 1) :to-equal "two")
          (expect (value-at envelope "value" "explicit" "mapping" "key") :to-equal "value")
          (expect (value-at envelope "value" "anchor" "copy" "x") :to-be 1)
          (expect (value-at envelope "value" "merge" "<<") :to-equal "literal"))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "toml" "get" toml "")
          (expect code :to-be 0)
          (expect (value-at envelope "value" "offset")
                  :to-equal "1979-05-27T07:32:00.123456789Z")
          (expect (value-at envelope "value" "local_datetime")
                  :to-equal "1979-05-27T07:32:00.123456789")
          (expect (value-at envelope "value" "local_date") :to-equal "1979-05-27")
          (expect (value-at envelope "value" "local_time")
                  :to-equal "07:32:00.123456789")))))

  (it "rejects unsupported YAML values and malformed YAML/TOML with repairs"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (dolist (case '(("custom.yaml" "x: !custom value~%" "explicit-tag" "/x")
                      ("cycle.yaml" "loop: &loop [*loop]~%" "anchor-or-alias" "/loop/0")
                      ("duplicate.yaml" "outer: {x: 1, x: 2}~%" "duplicate-key" "/outer")
                      ("key.yaml" "1: c~%" "non-string-key" "")
                      ("documents.yaml" "x: 1~%---~%x: 2~%" "document-count" "")
                      ("positive-inf.yaml" "x: .inf~%" "non-finite-float" "/x")
                      ("negative-inf.yaml" "x: -.inf~%" "non-finite-float" "/x")
                      ("nan.yaml" "x: .nan~%" "non-finite-float" "/x")))
        (let ((path (ws-file workspace (first case) (apply #'format nil (second case) nil))))
          (expect-format-read-error workspace "yaml" path "input.unsupported-value"
                                    (third case) (fourth case))))
      (let ((yaml (ws-file workspace "bad.yaml" (format nil "x: [1,~%")))
            (toml (ws-file workspace "bad.toml" (format nil "x = [~%")))
            (range (ws-file workspace "range.toml" (format nil "x = 9223372036854775808~%")))
            (infinite (ws-file workspace "infinite.toml" (format nil "x = inf~%")))
            (negative-infinite (ws-file workspace "negative-infinite.toml" (format nil "x = -inf~%")))
            (nan (ws-file workspace "nan.toml" (format nil "x = nan~%"))))
        (expect-format-read-error workspace "yaml" yaml "input.syntax-error" "YAML syntax error.")
        (expect-format-read-error workspace "toml" toml "input.syntax-error" "TOML syntax error.")
        (expect-format-read-error workspace "toml" range "input.syntax-error" "TOML syntax error.")
        (expect-format-read-error workspace "toml" infinite "input.unsupported-value"
                                  "non-finite-float" "/x")
        (expect-format-read-error workspace "toml" negative-infinite "input.unsupported-value"
                                  "non-finite-float" "/x")
        (expect-format-read-error workspace "toml" nan "input.unsupported-value"
                                  "non-finite-float" "/x")))))
)
