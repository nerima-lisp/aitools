;;;; TOML write document validation and conversion.
(in-package #:aitools.edit.test)

(defun toml-write-refusal (text)
  (handler-case
      (progn (parse-toml-write-document text) nil)
    (edit-refusal (condition) condition)))

(describe "aitools.edit.domain TOML write documents"
  (it "maps table values and counts quoted, inline, and adjacent comments"
    (multiple-value-bind (value comments)
        (parse-toml-write-document
         "value = \"# quoted\" # inline\n# first\n# second\n")
      (expect (serialize-json value) :to-equal "{\"value\":\"# quoted\"}")
      (expect comments :to-be 3)))

  (it "rejects all TOML datetime types with the source pointer"
    (let ((offset (toml-write-refusal "value = 1979-05-27T00:00:00Z\n"))
          (local-date-time (toml-write-refusal "value = 1979-05-27T00:00:00\n"))
          (local-date (toml-write-refusal "value = 1979-05-27\n"))
          (local-time (toml-write-refusal "value = 00:00:00\n")))
      (expect (edit-refusal-detail offset) :to-equal
              "TOML value at JSON Pointer /value uses unsupported offset-date-time.")
      (expect (edit-refusal-detail local-date-time) :to-equal
              "TOML value at JSON Pointer /value uses unsupported local-date-time.")
      (expect (edit-refusal-detail local-date) :to-equal
              "TOML value at JSON Pointer /value uses unsupported local-date.")
      (expect (edit-refusal-detail local-time) :to-equal
              "TOML value at JSON Pointer /value uses unsupported local-time.")))

  (it "checks the root before nulls and rejects JSON integers outside int64"
    (let ((root (handler-case
                    (progn (serialize-toml-document (parse-json-text "[null]")) nil)
                  (edit-refusal (condition) condition)))
          (null-value (handler-case
                          (progn (serialize-toml-document
                                  (parse-json-text "{\"a\":null}")) nil)
                        (edit-refusal (condition) condition)))
          (large-int (handler-case
                         (progn (serialize-toml-document
                                 (parse-json-text "{\"a\":9223372036854775808}")) nil)
                       (edit-refusal (condition) condition))))
      (expect (edit-refusal-detail root) :to-equal
              "TOML value at JSON Pointer  uses unsupported non-table-root.")
      (expect (edit-refusal-detail null-value) :to-equal
              "TOML value at JSON Pointer /a uses unsupported toml-null.")
      (expect (edit-refusal-code large-int) :to-equal "argument.invalid")
      (expect (edit-refusal-detail large-int) :to-equal
              "TOML integer must fit signed 64-bit.")))

  (it "serializes an ordered JSON object as a TOML table"
    (expect (stringp (serialize-toml-document
                      (parse-json-text "{\"first\":1,\"second\":true}")))
            :to-be t))
  (it "rejects positive and negative infinite TOML floats"
    (let ((positive (toml-write-refusal "value = inf\n"))
          (negative (toml-write-refusal "value = -inf\n"))
          (nan-value (toml-write-refusal "value = nan\n")))
      (dolist (failure (list positive negative nan-value))
        (expect (edit-refusal-code failure) :to-equal "input.unsupported-value")
        (expect (edit-refusal-detail failure) :to-equal
                "TOML value at JSON Pointer /value uses unsupported non-finite-float.")))))
