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
(defun yaml-toml-document-generator ()
  (labels ((generated-object (values)
             (json-kit:make-json-object
              (loop for value in values
                    for index from 0
                    collect (cons (format nil "key-~D" index) value)))))
    (let ((scalar-generator
            (cl-weave:gen-one-of
             (cl-weave:gen-integer :min -100 :max 100)
             (cl-weave:gen-string :min-length 0 :max-length 16 :alphabet "abcXYZ09 _-.")
             (cl-weave:gen-map
              (lambda (value)
                (if value t json-kit:+json-false+))
              (cl-weave:gen-boolean))
             (cl-weave:gen-map
              (lambda (numerator) (/ (1+ (* 2 numerator)) 4))
              (cl-weave:gen-integer :min -100 :max 100)))))
      (cl-weave:gen-map
       #'generated-object
       (cl-weave:gen-list
        (cl-weave:gen-recursive
         scalar-generator
         (lambda (nested-generator)
           (cl-weave:gen-one-of
            (cl-weave:gen-vector scalar-generator :min-length 0 :max-length 4)
            (cl-weave:gen-map
             #'generated-object
             (cl-weave:gen-list nested-generator :min-length 1 :max-length 3))))
         :max-depth 2)
        :min-length 1
        :max-length 3)))))
(defun assert-generated-format-idempotent (format value)
  (aitools-binary)
  (call-with-workspace
   (lambda (ws)
     (let ((path (format nil "generated.~A" format)))
       (put ws
            path
            (aitools.edit.infrastructure:serialize-document
             format
             (aitools.edit.domain:parse-json-text
              (json-kit:stringify value))))
       (let* ((first (run-ok ws (list "doc" "fmt" path)))
              (first-bytes (file-octets ws path))
              (second (run-ok ws (list "doc" "fmt" path))))
         (format-warning-array-empty first)
         (format-warning-array-empty second)
         (expect-canonical-format-bytes first-bytes)
         (expect (file-octets ws path) :to-equalp first-bytes))))))

(describe "YAML and TOML format bytes"
  (cl-weave:it-property
   "keeps generated YAML documents unchanged on a second format"
   ((value (yaml-toml-document-generator)))
   (assert-generated-format-idempotent "yaml" value))
  (cl-weave:it-property
   "keeps generated TOML documents unchanged on a second format"
   ((value (yaml-toml-document-generator)))
   (assert-generated-format-idempotent "toml" value))
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
                (first (run-ok ws (list "doc" "fmt" path)))
                (first-bytes (file-octets ws path))
                (second (run-ok ws (list "doc" "fmt" path))))
           (format-warning-array-empty first)
           (format-warning-array-empty second)
           (expect-canonical-format-bytes first-bytes)
           (expect (file-octets ws path) :to-equalp first-bytes)
           (let ((value (jget (run-ok ws (list "doc" "get" path "")) "value")))
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
         (let* ((first (run-ok ws '("doc" "fmt" "mixed.toml")))
                (first-bytes (file-octets ws "mixed.toml"))
                (formatted (utf8 first-bytes))
                (second (run-ok ws '("doc" "fmt" "mixed.toml"))))
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

  (it "rejects JSON-only formatting options for YAML and TOML without writing"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (put ws "options.yaml" (format nil "value: 1~%"))
       (put ws "options.toml" (format nil "value = 1~%"))
       (dolist (path '("options.yaml" "options.toml"))
         (dolist (option '(("--indent" "2") ("--minify") ("--sort-keys")))
           (let* ((before (file-octets ws path))
                  (result (aitools ws (append (list "doc" "fmt" path) option)))
                  (envelope (aitools-envelope result)))
             (expect (aitools-exit-code result) :to-be 1)
             (expect envelope :to-match-envelope "status" "error" "command" "doc fmt")
             (expect (jget envelope "error" "code") :to-equal "argument.invalid")
             (expect (file-octets ws path) :to-equalp before)))))))

  (it "undo restores exact bytes after every YAML and TOML write command"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (let* ((yaml-body (crlf-octets (format nil "key: old~%drop: true~%# original comment~%")))
              (yaml-before (concatenate '(vector (unsigned-byte 8)) #(239 187 191) yaml-body))
              (toml-before (crlf-octets (format nil "key = \"old\"~%drop = true~%# 元のコメント~%"))))
         (put ws "undo.yaml" yaml-before)
         (put ws "undo.toml" toml-before)
         (dolist (case `(("undo.yaml" ,yaml-before)
                         ("undo.toml" ,toml-before)))
           (let* ((path (first case))
                  (before (second case)))
             (dolist (verb '("set" "delete" "merge" "patch" "fmt"))
               (let* ((write
                        (cond
                          ((string= verb "set")
                           (run-ok ws (list "doc" verb path "/key" "\"updated\"")))
                          ((string= verb "delete")
                           (run-ok ws (list "doc" verb path "/drop")))
                          ((string= verb "merge")
                           (run-ok ws (list "doc" verb path "--stdin")
                                   :stdin "{\"key\":\"updated\"}"))
                          ((string= verb "patch")
                           (run-ok ws (list "doc" verb path "--stdin")
                                   :stdin "[{\"op\":\"replace\",\"path\":\"/key\",\"value\":\"updated\"}]"))
                          ((string= verb "fmt")
                           (run-ok ws (list "doc" verb path)))))
                      (op-id (jget write "op_id")))
                 (expect write :to-match-envelope
                         "status" "ok" "command" (format nil "doc ~A" verb))
                 (expect (stringp op-id) :to-be t)
                 (expect-format-warnings write 1)
                 (expect (not (equalp (file-octets ws path) before)) :to-be t)
                 (let ((undo (run-ok ws (list "undo" op-id))))
                   (expect undo :to-match-envelope
                           "status" "ok" "command" "undo" "undoes" op-id))
                 (expect (file-octets ws path) :to-equalp before))))))))))
