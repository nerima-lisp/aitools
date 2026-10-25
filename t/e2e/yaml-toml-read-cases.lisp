;;;; Process-level checks for the read-only YAML and TOML command groups.
(in-package #:aitools.e2e.test)

(defun expect-format-read-error (workspace format path code reason action)
  (let* ((result (aitools workspace (list format "get" path "")))
         (envelope (aitools-envelope result))
         (error (jget envelope "error")))
    (expect (aitools-exit-code result) :to-be 1)
    (expect envelope :to-match-envelope "status" "error"
            "command" (format nil "~A get" format))
    (expect (jget error "code") :to-equal code)
    (expect (jget error "exit_code") :to-be 1)
    (expect (jget error "message") :to-contain reason)
    (expect (jget error "repairs" 0 "action") :to-equal action)
    (expect (jget error "repairs" 0 "command")
            :to-equal (format nil "aitools schema ~A get" format))
    (expect (jget error "repairs" 0 "detail")
            :to-equal (if (string= code "input.syntax-error")
                          "Fix the YAML/TOML syntax or TOML integer range, then retry."
                          "Convert the value to the supported subset, then retry."))))

(describe "YAML and TOML read-only commands"
  (it "maps YAML values and TOML tables and datetimes without changing files"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (put ws "values.yaml"
            (format nil
                    (concatenate 'string
                                 "plain: text~%null_value: null~%true_value: true~%false_value: false~%"
                                 "decimal: 42~%octal: 0o52~%hex: 0x2A~%finite: 1.25~%"
                                 "explicit:~%  string: !!str true~%  integer: !!int \"42\"~%"
                                 "  floating: !!float \"1.5\"~%  boolean: !!bool \"false\"~%"
                                 "  null_value: !!null \"\"~%  sequence: !!seq [one, two]~%"
                                 "  mapping: !!map {key: value}~%"
                                 "anchor:~%  source: &shared {x: 1}~%  copy: *shared~%"
                                 "merge:~%  <<: literal~%")))
       (put ws "values.toml"
            (format nil
                    (concatenate 'string
                                 "first_member = 1~%second_member = 2~%"
                                 "min = -9223372036854775808~%max = 9223372036854775807~%"
                                 "finite = 1.25~%inline = { a = 1, b = \"x\" }~%"
                                 "[nested]~%enabled = true~%~%[[items]]~%name = \"first\"~%"
                                 "[[items]]~%name = \"second\"~%")))
       (put ws "dates.toml"
            (format nil
                    (concatenate 'string
                                 "offset = 1979-05-27T07:32:00.123456789+00:00~%"
                                 "local_datetime = 1979-05-27T07:32:00.123456789~%"
                                 "local_date = 1979-05-27~%local_time = 07:32:00.123456789123~%")))
       (let* ((yaml-result (run-ok ws '("yaml" "get" "values.yaml" "")))
               (toml-result (aitools ws '("toml" "get" "values.toml" "")))
               (toml-envelope (aitools-envelope toml-result))
               (date-result (run-ok ws '("toml" "get" "dates.toml" ""))))
           (expect (jget yaml-result "value" "plain") :to-equal "text")
           (expect (jget yaml-result "value" "decimal") :to-be 42)
           (expect (jget yaml-result "value" "octal") :to-be 42)
           (expect (jget yaml-result "value" "hex") :to-be 42)
           (expect (jget yaml-result "value" "finite") :to-be 1.25d0)
           (expect (jget yaml-result "value" "explicit" "string") :to-equal "true")
           (expect (jget yaml-result "value" "explicit" "integer") :to-be 42)
           (expect (jget yaml-result "value" "explicit" "floating") :to-be 1.5d0)
           (expect (jget yaml-result "value" "explicit" "sequence" 1) :to-equal "two")
           (expect (jget yaml-result "value" "explicit" "mapping" "key") :to-equal "value")
           (expect (jget yaml-result "value" "anchor" "copy" "x") :to-be 1)
           (expect (jget yaml-result "value" "merge" "<<") :to-equal "literal")
           (expect (aitools-exit-code toml-result) :to-be 0)
           (expect toml-envelope :to-match-envelope "status" "ok" "command" "toml get")
           (expect (jget toml-envelope "value" "min") :to-be -9223372036854775808)
           (expect (jget toml-envelope "value" "max") :to-be 9223372036854775807)
           (expect (jget toml-envelope "value" "finite") :to-be 1.25d0)
           (expect (jget toml-envelope "value" "inline" "a") :to-be 1)
           (expect (jget toml-envelope "value" "nested" "enabled") :to-be t)
           (expect (jget toml-envelope "value" "items" 1 "name") :to-equal "second")
           (let ((first (search "\"first_member\"" (aitools-stdout toml-result)))
                 (second (search "\"second_member\"" (aitools-stdout toml-result))))
             (expect (and first second (< first second)) :to-be t))
           (expect (jget date-result "value" "offset")
                   :to-equal "1979-05-27T07:32:00.123456789Z")
           (expect (jget date-result "value" "local_datetime")
                   :to-equal "1979-05-27T07:32:00.123456789")
           (expect (jget date-result "value" "local_date") :to-equal "1979-05-27")
           (expect (jget date-result "value" "local_time")
                   :to-equal "07:32:00.123456789")))))

  (it "selects and diffs each format with its declared parser"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (put ws "items.yaml"
            (format nil "- id: 1~%  n: 5~%  k: a~%- id: 2~%  n: 9~%  k: a~%- id: 3~%  n: 7~%  k: b~%"))
       (put ws "items.toml"
            (format nil
                    (concatenate 'string
                                 "[[items]]~%id = 1~%n = 5~%k = \"a\"~%~%[[items]]~%"
                                 "id = 2~%n = 9~%k = \"a\"~%~%[[items]]~%"
                                 "id = 3~%n = 7~%k = \"b\"~%")))
       (put ws "old.yaml" "{x: 1, y: 2}")
       (put ws "new.json" "{\"x\": 2}")
       (put ws "old.toml" (format nil "x = 1~%y = 2~%"))
       (put ws "new.toml" (format nil "x = 2~%"))
       (dolist (format '("yaml" "toml"))
         (let* ((path (if (string= format "yaml") "items.yaml" "items.toml"))
                (pointer (if (string= format "yaml") "" "/items"))
                (result (run-ok ws (list format "select" path pointer "--where" "/n>4" "--where" "/k=a" "--pick" "/id" "--pick" "/n" "--sort-by" "/n" "--desc" "--limit" "2"))))
           (expect (jget result "total") :to-be 2)
           (expect (jget result "command") :to-equal (format nil "~A select" format))
           (expect (jget result "items" 0 "value" "/id") :to-be 2)))
       (let ((yaml-diff (aitools ws '("yaml" "diff" "old.yaml" "new.json" "--limit" "1")))
             (toml-diff (aitools ws '("toml" "diff" "old.toml" "new.toml" "--limit" "1"))))
         (expect (aitools-exit-code yaml-diff) :to-be 3)
         (expect (jget (aitools-envelope yaml-diff) "command") :to-equal "yaml diff")
         (expect (jget (aitools-envelope yaml-diff) "total") :to-be 2)
         (expect (aitools-exit-code toml-diff) :to-be 3)
         (expect (jget (aitools-envelope toml-diff) "command") :to-equal "toml diff")
         (expect (jget (aitools-envelope toml-diff) "total") :to-be 2))
       (let ((wrong-format (aitools ws '("toml" "diff" "old.toml" "new.json"))))
         (expect (aitools-exit-code wrong-format) :to-be 1)
         (expect (aitools-envelope wrong-format) :to-match-envelope
                 "status" "error" "command" "toml diff")
         (expect (jget (aitools-envelope wrong-format) "error" "code")
                 :to-equal "input.syntax-error")
         (expect (jget (aitools-envelope wrong-format) "error" "exit_code") :to-be 1)
         (expect (jget (aitools-envelope wrong-format) "error" "repairs" 0 "command")
                 :to-equal "aitools schema toml diff")))))

  (it "rejects YAML unsupported values and YAML/TOML parse errors with repairs"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (dolist (case '(("custom.yaml" "x: !custom value~%" "explicit-tag")
                       ("cycle.yaml" "loop: &loop [*loop]~%" "anchor-or-alias")
                       ("duplicate.yaml" "outer: {x: 1, x: 2}~%" "duplicate-key")
                       ("key.yaml" "? [a, b]~%: c~%" "non-string-key")
                       ("documents.yaml" "x: 1~%---~%x: 2~%" "document-count")
                       ("positive-inf.yaml" "x: .inf~%" "non-finite-float")
                       ("negative-inf.yaml" "x: -.inf~%" "non-finite-float")
                       ("nan.yaml" "x: .nan~%" "non-finite-float")))
         (let ((path (put ws (first case) (format nil (second case)))))
           (expect-format-read-error ws "yaml" (namestring path)
                                     "input.unsupported-value" (third case)
                                     "convert-to-supported-value")))
       (dolist (case '(("bad.yaml" "x: [1,~%" "yaml" "YAML syntax error.")
                       ("bad.toml" "x = [~%" "toml" "TOML syntax error.")
                       ("range.toml" "x = 9223372036854775808~%" "toml" "TOML syntax error.")
                       ("infinite.toml" "x = inf~%" "toml" "non-finite-float")
                       ("negative-infinite.toml" "x = -inf~%" "toml" "non-finite-float")
                       ("nan.toml" "x = nan~%" "toml" "non-finite-float")))
         (let* ((path (put ws (first case) (format nil (second case))))
                (format (third case))
                (code (if (string= (fourth case) "non-finite-float")
                          "input.unsupported-value"
                          "input.syntax-error")))
           (expect-format-read-error ws format (namestring path) code (fourth case)
                                     (if (string= code "input.syntax-error")
                                         "fix-format-syntax"
                                         "convert-to-supported-value")))))))
)

(describe "YAML mapping violation pointers"
  (it "reports the containing mapping for duplicate and non-string keys"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (dolist (case '(("duplicate" "outer: {x: 1, x: 2}~%" "duplicate-key")
                       ("non-string" "outer: {? [a, b]: c}~%" "non-string-key")))
         (let* ((path (put ws (format nil "~A.yaml" (first case))
                           (format nil (second case))))
                (result (aitools ws (list "yaml" "get" (namestring path) "")))
                (error (jget (aitools-envelope result) "error"))
                (message (jget error "message")))
           (expect (aitools-exit-code result) :to-be 1)
           (expect (jget error "code") :to-equal "input.unsupported-value")
           (expect message :to-contain "JSON Pointer /outer")
           (expect message :to-contain (third case))))))))

(describe "TOML datetime normalization"
  (it "normalizes date-time separators and fractional seconds"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (put ws "datetime.toml"
            (format nil
                    (concatenate 'string
                                 "space_separator = 1979-05-27 07:32:00.123400000~%"
                                 "lower_z = 1979-05-27T07:32:00.123456789z~%"
                                 "zero_fraction = 1979-05-27T07:32:00.000000000+00:00~%"
                                 "short_fraction = 1979-05-27T07:32:00.120000000+00:00~%")))
       (let ((result (run-ok ws '("toml" "get" "datetime.toml" ""))))
         (expect (jget result "value" "space_separator")
                 :to-equal "1979-05-27T07:32:00.1234")
         (expect (jget result "value" "lower_z")
                 :to-equal "1979-05-27T07:32:00.123456789Z")
         (expect (jget result "value" "zero_fraction")
                 :to-equal "1979-05-27T07:32:00Z")
         (expect (jget result "value" "short_fraction")
                 :to-equal "1979-05-27T07:32:00.12Z"))))))
