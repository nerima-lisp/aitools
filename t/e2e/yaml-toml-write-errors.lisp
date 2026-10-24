(in-package #:aitools.e2e.test)

(defun expect-format-write-error (workspace format verb path code reason pointer
                                  &optional stdin &rest extra-arguments)
  (let* ((arguments (append (list format verb path)
                            (when stdin '("--stdin"))
                            extra-arguments))
         (result (if stdin
                     (aitools workspace arguments :stdin stdin)
                     (aitools workspace arguments)))
         (envelope (aitools-envelope result))
         (error (jget envelope "error"))
         (repair (jget error "repairs" 0))
         (unsupported (string= code "input.unsupported-value")))
    (expect (aitools-exit-code result) :to-be 1)
    (expect envelope :to-match-envelope "status" "error"
            "command" (format nil "~A ~A" format verb))
    (expect (jget error "code") :to-equal code)
    (expect (jget error "exit_code") :to-be 1)
    (expect (and (vectorp (jget error "repairs"))
                 (plusp (length (jget error "repairs")))) :to-be t)
    (expect (jget repair "action")
            :to-equal (if unsupported "convert-to-supported-value" "fix-format-syntax"))
    (expect (jget repair "detail")
            :to-equal (if unsupported
                          "Convert the value to the supported subset, then retry."
                          "Fix the YAML/TOML syntax or TOML integer range, then retry."))
    (expect (jget repair "command") :to-equal (format nil "aitools schema ~A ~A" format verb))
    (expect (jget error "message")
            :to-equal (if unsupported
                          (format nil "~A value at JSON Pointer ~A uses unsupported ~A."
                                  (string-upcase format) pointer reason)
                          (format nil "~A syntax error." (string-upcase format))))
    (expect (nth-value 1 (gethash "warnings" envelope)) :to-be nil)))

(describe "YAML and TOML write errors"
  (it "rejects unsupported source values before applying a write and gives repairs"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (dolist (case '(("tag.yaml" "tagged: !!str value~%" "yaml" "explicit-tag" "/tagged")
                       ("invalid-tag.yaml" "value: !!int nope~%" "yaml" "explicit-tag" "/value")
                       ("duplicate-before-invalid-tag.yaml"
                        "same: first~%same: second~%later: !!int nope~%"
                        "yaml" "duplicate-key" "")
                       ("anchor-before-invalid-tag.yaml"
                        "first: &shared {x: 1}~%later: !!int nope~%"
                        "yaml" "anchor-or-alias" "/first")
                       ("alias.yaml" "shared: &s {k: v}~%copy: *s~%" "yaml" "anchor-or-alias" "/shared")
                       ("duplicate.yaml" "outer: {x: 1, x: 2}~%" "yaml" "duplicate-key" "/outer")
                       ("key.yaml" "outer: {? [a, b]: c}~%" "yaml" "non-string-key" "/outer")
                       ("documents.yaml" "x: 1~%---~%x: 2~%" "yaml" "document-count" "")
                       ("infinite.yaml" "value: .inf~%" "yaml" "non-finite-float" "/value")
                       ("source-order.yaml"
                        "first: !!str value~%later: &shared {x: 1}~%copy: *shared~%"
                        "yaml" "explicit-tag" "/first")
                       ("key-before-value.yaml" "? [a, b]~%: !!str value~%"
                        "yaml" "non-string-key" "")
                       ("key-reasons.yaml" "? &key !!seq [a, b]~%: *key~%"
                        "yaml" "explicit-tag" "")
                       ("key-anchor.yaml" "? &key [a, b]~%: !!str value~%"
                        "yaml" "anchor-or-alias" "")
                       ("duplicate-before-value.yaml" "x: first~%x: !!str second~%"
                        "yaml" "duplicate-key" "")
                       ("key-tag-duplicate.yaml" "x: first~%? !!str x~%: second~%" "yaml" "explicit-tag" "")
                       ("key-anchor-duplicate.yaml" "x: first~%? &key x~%: second~%" "yaml" "anchor-or-alias" "")
                       ("date.toml" "when = 1979-05-27~%" "toml" "local-date" "/when")
                       ("local-date-time.toml" "when = 1979-05-27T07:32:00~%"
                        "toml" "local-date-time" "/when")
                       ("local-time.toml" "when = 07:32:00~%"
                        "toml" "local-time" "/when")
                       ("offset.toml" "when = 1979-05-27T07:32:00Z~%" "toml" "offset-date-time" "/when")
                       ("nan.toml" "value = nan~%" "toml" "non-finite-float" "/value")))
         (let* ((path (put ws (first case) (format nil (second case))))
                (format (third case))
                (before (file-octets ws (first case))))
           (expect-format-write-error ws format "fmt" (namestring path)
                                      "input.unsupported-value" (fourth case) (fifth case))
           (expect (file-octets ws (first case)) :to-equalp before)))
       (let* ((path (put ws "tag-set.yaml" (format nil "tagged: !!str value~%")))
              (before (file-octets ws "tag-set.yaml")))
         (expect-format-write-error ws "yaml" "set" (namestring path)
                                    "input.unsupported-value" "explicit-tag" "/tagged"
                                    nil "/tagged" "\"changed\"")
         (expect (file-octets ws "tag-set.yaml") :to-equalp before)))))
  (it "rejects TOML nulls and non-table roots after edit operations"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (put ws "null.toml" (format nil "nested = { value = 1 }~%"))
       (expect-format-write-error ws "toml" "set" "null.toml"
                                  "input.unsupported-value" "toml-null" "/nested/value"
                                  nil "/nested/value" "null")
       (expect-format-write-error ws "toml" "patch" "null.toml"
                                  "input.unsupported-value" "toml-null" "/nested/value"
                                  "[{\"op\":\"replace\",\"path\":\"/nested/value\",\"value\":null}]")
       (put ws "root.toml" (format nil "key = \"value\"~%"))
       (expect-format-write-error ws "toml" "set" "root.toml"
                                  "input.unsupported-value" "non-table-root" ""
                                  nil "" "\"text\"")
       (put ws "overflow.toml" (format nil "value = 1~%"))
       (let* ((before (file-octets ws "overflow.toml"))
              (result (aitools ws '("toml" "set" "overflow.toml" "/value"
                                    "9223372036854775808")))
              (envelope (aitools-envelope result)))
         (expect (aitools-exit-code result) :to-be 1)
         (expect envelope :to-match-envelope "status" "error" "command" "toml set")
         (expect (jget envelope "error" "code") :to-equal "argument.invalid")
         (expect (jget envelope "error" "exit_code") :to-be 1)
         (expect (file-octets ws "overflow.toml") :to-equalp before)))))
  (it "returns syntax repairs for malformed YAML and TOML documents"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (dolist (case '(("bad.yaml" "value: [~%" "yaml")
                       ("bad.toml" "value = [~%" "toml")
                       ("range.toml" "value = 9223372036854775808~%" "toml")))
         (let ((path (put ws (first case) (format nil (second case)))))
           (expect-format-write-error ws (third case) "fmt" (namestring path)
                                      "input.syntax-error" nil ""))))))
  (it "rejects nonzero JSON-number underflow and accepts zero values"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (dolist (case (quote (("yaml" "underflow.yaml" "value: 1~%")
                             ("toml" "underflow.toml" "value = 1~%"))))
         (let* ((format (first case))
                (path (put ws (second case) (format nil (third case))))
                (before (file-octets ws (second case)))
                (result (aitools ws (list format "set" (namestring path)
                                          "/value" "1e-9999")))
                (envelope (aitools-envelope result)))
           (expect (aitools-exit-code result) :to-be 1)
           (expect envelope :to-match-envelope "status" "error"
                   "command" (format nil "~A set" format))
           (expect (jget envelope "error" "code") :to-equal "argument.invalid")
           (expect (jget envelope "error" "exit_code") :to-be 1)
           (expect (file-octets ws (second case)) :to-equalp before)))
       (dolist (case (quote (("yaml" "zero.yaml" "value: 1~%")
                             ("toml" "zero.toml" "value = 1~%"))))
         (dolist (number (quote ("0" "0e-9999")))
           (let* ((format (first case))
                  (path (put ws (second case) (format nil (third case))))
                  (before (file-octets ws (second case)))
                  (result (aitools ws (list format "set" (namestring path)
                                            "/value" number)))
                  (envelope (aitools-envelope result)))
             (expect (aitools-exit-code result) :to-be 0)
             (expect envelope :to-match-envelope "status" "ok"
                     "command" (format nil "~A set" format))
             (expect (equalp (file-octets ws (second case)) before) :to-be nil)
             (let* ((read-result (aitools ws (list format "get" (namestring path) "")))
                    (read-envelope (aitools-envelope read-result)))
               (expect (aitools-exit-code read-result) :to-be 0)
               (expect read-envelope :to-match-envelope "status" "ok"
                       "command" (format nil "~A get" format))
               (expect (zerop (jget read-envelope "value" "value")) :to-be t)))))))))
