(in-package #:aitools.e2e.test)

(defun crlf-octets (text)
  (octets (with-output-to-string (out)
            (loop for char across text
                  do (when (char= char #\Newline) (write-char #\Return out))
                     (write-char char out)))))

(defun expect-canonical-format-bytes (bytes)
  (expect (> (length bytes) 3) :to-be t)
  (expect (equalp (subseq bytes 0 3) #(239 187 191)) :to-be nil)
  (expect (position 13 bytes) :to-be nil)
  (expect (aref bytes (1- (length bytes))) :to-be 10)
  (expect (equalp (octets (utf8 bytes)) bytes) :to-be t))

(defun format-warning-array-empty (envelope)
  (let ((warnings (jget envelope "warnings")))
    (expect (and (vectorp warnings) (zerop (length warnings))) :to-be t)))

(describe "YAML and TOML format bytes"
  (it "is idempotent and emits UTF-8 without BOM using LF and a final newline"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (put ws "canonical.yaml"
            (format nil "body: |~%  alpha~%  βeta~%empty: {}"))
       (put ws "canonical.toml"
            (format nil "[zeta]~%value = 1~%[alpha]~%value = \"雪\"~%[empty]"))
       (dolist (format '("yaml" "toml"))
         (let* ((path (format nil "canonical.~A" format))
                (first (run-ok ws (list format "fmt" path)))
                (first-bytes (file-octets ws path))
                (second (run-ok ws (list format "fmt" path))))
           (format-warning-array-empty first)
           (format-warning-array-empty second)
           (expect-canonical-format-bytes first-bytes)
           (expect (file-octets ws path) :to-equalp first-bytes)
           (let ((value (jget (run-ok ws (list format "get" path "")) "value")))
             (expect (hash-table-p (jget value "empty")) :to-be t)
             (when (string= format "yaml")
               (expect (jget value "body") :to-equal (format nil "alpha~%βeta~%")))))))))

  (it "puts TOML child tables before arrays of tables and remains idempotent"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (let* ((input
                (format nil "~{~A~%~}"
                        '("[beta]"
                          "value = 2"
                          "[[beta.rows]]"
                          "name = \"row\""
                          "[beta.zeta]"
                          "value = 3"
                          "[[beta.items]]"
                          "name = \"item\""
                          "[beta.alpha]"
                          "value = 1")))
              (input-bytes (octets input)))
         (put ws "mixed.toml" input)
         (let* ((first (run-ok ws '("toml" "fmt" "mixed.toml")))
                (first-bytes (file-octets ws "mixed.toml"))
                (formatted (utf8 first-bytes))
                (second (run-ok ws '("toml" "fmt" "mixed.toml"))))
           (format-warning-array-empty first)
           (format-warning-array-empty second)
           (expect-canonical-format-bytes first-bytes)
           (expect (equalp input-bytes first-bytes) :to-be nil)
           (expect (< (search "[beta]" formatted)
                      (search "[beta.zeta]" formatted)
                      (search "[beta.alpha]" formatted)
                      (search "[[beta.rows]]" formatted)
                      (search "[[beta.items]]" formatted))
                   :to-be t)
           (expect (file-octets ws "mixed.toml") :to-equalp first-bytes))))))

  (it "undo restores YAML BOM and TOML CRLF and non-ASCII comment bytes exactly"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (let* ((yaml-body (crlf-octets (format nil "key: old~%# original comment~%")))
              (yaml-before (concatenate '(vector (unsigned-byte 8)) #(239 187 191) yaml-body))
              (toml-before (crlf-octets (format nil "key = \"old\"~%# 元のコメント~%"))))
         (put ws "undo.yaml" yaml-before)
         (put ws "undo.toml" toml-before)
         (dolist (case `(("yaml" "undo.yaml" ,yaml-before)
                         ("toml" "undo.toml" ,toml-before)))
           (let* ((format (first case))
                  (path (second case))
                  (before (third case))
                  (write (run-ok ws (list format "set" path "/key" "\"new\"")))
                  (op-id (jget write "op_id")))
             (expect (stringp op-id) :to-be t)
             (expect-format-warnings write 1)
             (expect (not (equalp (file-octets ws path) before)) :to-be t)
             (let ((undo (run-ok ws (list "undo" op-id))))
               (expect (jget undo "undoes") :to-equal op-id))
             (expect (file-octets ws path) :to-equalp before))))))))
