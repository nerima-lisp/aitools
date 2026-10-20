(in-package #:aitools.inspect.test)

(describe "archive list"
  (it-each (("a.zip" "zip") ("a.tar" "tar") ("a.tar.gz" "tar.gz"))
      "lists the members of ~A as ~A"
      (file format)
    (multiple-value-bind (kind fields) (run-archive #'archive-list-flow file)
      (expect kind :to-be :ok)
      (expect (field fields "format") :to-equal format)
      (expect (field fields "total") :to-be 3)
      (expect (mapcar (lambda (item) (json-object-get item "path")) (field fields "items"))
              :to-equal '("docs" "docs/readme.txt" "src/main.lisp"))
      (let ((readme (second (field fields "items"))))
        (expect (json-object-get readme "size") :to-be 14)
        (expect (json-object-get readme "kind") :to-equal "file")
        (expect (json-object-get readme "mtime") :to-equal "2023-11-14T22:13:20Z"))))

  (it "lists a gz stream as its one member"
    (multiple-value-bind (kind fields) (run-archive #'archive-list-flow "notes.txt.gz")
      (expect kind :to-be :ok)
      (expect (field fields "format") :to-equal "gz")
      (expect (json-object-get (first (field fields "items")) "path") :to-equal "notes.txt")
      (expect (json-object-get (first (field fields "items")) "size") :to-be 9)))

  (it "lists the safe gzip name that extraction writes"
    (let* ((gzip (aitools.text.domain:gzip-compress (%as-octets "payload") :name "../x"))
           (listed-name
             (multiple-value-bind (kind fields)
                 (run-flow #'archive-list-flow
                           (make-test-ports :files `(("/work/bundle.gz" . ,gzip)))
                           "bundle.gz")
               (expect kind :to-be :ok)
               (json-object-get (first (field fields "items")) "path")))
           (steps (aitools.edit.domain:plan-archive-extraction
                   gzip :gz "" :archive-path "bundle.gz"
                   :max-bytes 100 :max-entries 1
                   :lookup-kind (lambda (path) (declare (ignore path)) :absent))))
      (expect listed-name :to-equal "bundle")
      (expect (mapcar #'aitools.edit.domain:extract-step-path steps)
              :to-equal (list listed-name))))

  (it "keeps malformed gzip header failures at the list boundary"
    (let ((gzip (%as-octets #(31 139 8 8 0 0 0 0 0 255 120))))
      (multiple-value-bind (kind fields)
          (run-flow #'archive-list-flow
                    (make-test-ports :files `(("/work/broken.gz" . ,gzip)))
                    "broken.gz")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.unsupported-format")
        (expect (getf fields :message)
                :to-equal "broken.gz cannot be read as an archive: unterminated gzip header string"))))

  (it "stops at --limit as partial"
    (multiple-value-bind (kind fields) (run-archive #'archive-list-flow "a.zip" :limit 2)
      (expect kind :to-be :partial)
      (expect (length (field fields "items")) :to-be 2)
      (expect (field fields "total") :to-be 3)))
  (it "reports the decompression limit with a list repair"
    (let ((octets (cdr (assoc "notes.txt.gz" (sample-archives) :test (function string=)))))
      (cl-weave:with-mocked-functions
          (((symbol-function (find-symbol "GZIP-DECOMPRESS" "AITOOLS.TEXT.DOMAIN"))
            (lambda (input &key max-output)
              (declare (ignore input max-output))
              (error (find-symbol "ARCHIVE-LIMIT-EXCEEDED" "AITOOLS.TEXT.DOMAIN")
                     :limit +archive-max-decompressed+ :reason "output limit"))))
        (multiple-value-bind (kind fields)
            (run-archive (function archive-list-flow) "notes.txt.gz")
          (expect kind :to-be :error)
          (expect (error-code fields) :to-equal "refusal.too-large")
          (expect (getf (first (getf fields :repairs)) :command)
                  :to-equal "aitools archive list notes.txt.gz")))))

  (it "rejects a file that is not an archive as input.unsupported-format"
    (multiple-value-bind (kind fields)
        (run-flow #'archive-list-flow (make-test-ports :files '(("/work/plain.txt" . "just text"))) "plain.txt")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.unsupported-format")))

  (it "rejects a truncated zip without signalling"
    (let ((zip (cdr (assoc "a.zip" (sample-archives) :test #'string=))))
      (multiple-value-bind (kind fields)
          (run-flow #'archive-list-flow (make-test-ports :files `(("/work/cut.zip" . ,(subseq zip 0 40)))) "cut.zip")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.unsupported-format")))))
