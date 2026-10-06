;;;; t/unit/inspect/archive-test.lisp
;;;;
;;;; `archive list` and `archive read` through fake ports, for zip, tar,
;;;; tar.gz, and gz.
(in-package #:aitools.inspect.test)

(defun archive-member (name text &key (kind :file))
  (aitools.text.domain:make-archive-member
   :name name :kind kind :mode (if (eq kind :directory) #o755 #o644) :mtime 1700000000
   :data (%as-octets (or text ""))))

(defun sample-members ()
  (list (archive-member "docs" nil :kind :directory)
        (archive-member "docs/readme.txt" (format nil "one~%two~%three~%"))
        (archive-member "src/main.lisp" (format nil "(defun main ()~%  (print 1))~%"))))

(defun sample-archives ()
  "(file-name . octets) for each supported format."
  (let ((tar (aitools.text.domain:write-tar (sample-members))))
    (list (cons "a.zip" (aitools.text.domain:write-zip (sample-members)))
          (cons "a.tar" tar)
          (cons "a.tar.gz" (aitools.text.domain:gzip-compress tar))
          (cons "notes.txt.gz" (aitools.text.domain:gzip-compress (%as-octets (format nil "hello~%gz~%"))
                                                                  :name "notes.txt")))))

(defun archive-files ()
  (loop for (name . octets) in (sample-archives) collect (cons (concatenate 'string "/work/" name) octets)))

(defun run-archive (flow &rest arguments)
  (apply #'run-flow flow (make-test-ports :files (archive-files)) arguments))

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

  (it "stops at --limit as partial"
    (multiple-value-bind (kind fields) (run-archive #'archive-list-flow "a.zip" :limit 2)
      (expect kind :to-be :partial)
      (expect (length (field fields "items")) :to-be 2)
      (expect (field fields "total") :to-be 3)))

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

(describe "archive read"
  (it-each (("a.zip") ("a.tar") ("a.tar.gz"))
      "reads a member of ~A in read's shape"
      (file)
    (multiple-value-bind (kind fields) (run-archive #'archive-read-flow file "docs/readme.txt")
      (expect kind :to-be :ok)
      (expect (field fields "mode") :to-equal "text")
      (expect (field fields "entry") :to-equal "docs/readme.txt")
      (expect (field fields "start_line") :to-be 1)
      (expect (field fields "lines") :to-equal '("one" "two" "three"))
      (expect (field fields "total_lines") :to-be 3)))

  (it "reads a gz member without an entry name"
    (multiple-value-bind (kind fields) (run-archive #'archive-read-flow "notes.txt.gz" nil)
      (expect kind :to-be :ok)
      (expect (field fields "lines") :to-equal '("hello" "gz"))))

  (it "applies selectors and the line limit"
    (expect (field (nth-value 1 (run-archive #'archive-read-flow "a.zip" "docs/readme.txt" :range "2")) "lines")
            :to-equal '("two"))
    (expect (field (nth-value 1 (run-archive #'archive-read-flow "a.tar" "src/main.lisp" :symbol "main")) "lines")
            :to-equal '("(defun main ()" "  (print 1))"))
    (multiple-value-bind (kind fields) (run-archive #'archive-read-flow "a.zip" "docs/readme.txt" :max-lines 1)
      (expect kind :to-be :partial)
      (expect (field fields "next_commands") :to-equal '("aitools archive read a.zip docs/readme.txt --range 2:2"))))

  (it "dumps a member as hex"
    (multiple-value-bind (kind fields) (run-archive #'archive-read-flow "a.zip" "docs/readme.txt" :as "hex")
      (expect kind :to-be :ok)
      (expect (field fields "mode") :to-equal "hex")
      (expect (json-object-get (first (field fields "rows")) "hex") :to-equal "6f 6e 65 0a 74 77 6f 0a 74 68 72 65 65 0a")))

  (it "fails with input.not-found and near entry names for a missing entry"
    (multiple-value-bind (kind fields) (run-archive #'archive-read-flow "a.tar" "docs/redme.txt")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (json-object-get (first (getf fields :candidates)) "path") :to-equal "docs/readme.txt")))

  (it "refuses a directory entry and requires an entry for multi-member archives"
    (expect (error-code (nth-value 1 (run-archive #'archive-read-flow "a.zip" "docs"))) :to-equal "refusal.not-a-file")
    (expect (error-code (nth-value 1 (run-archive #'archive-read-flow "a.zip" nil))) :to-equal "argument.invalid"))

  (it "refuses a gzip stream that expands past the entry limit"
    (let ((bomb (aitools.text.domain:gzip-compress
                 (make-array (1+ +archive-max-entry+) :element-type '(unsigned-byte 8) :initial-element 0))))
      (multiple-value-bind (kind fields)
          (run-flow #'archive-read-flow (make-test-ports :files `(("/work/zeros.gz" . ,bomb))) "zeros.gz" nil)
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "refusal.too-large")))))

(describe "archive read outcomes"
  (it "names the whole hex dump as the next command when --max-lines cuts it"
    (multiple-value-bind (kind fields) (run-archive #'archive-read-flow "a.tar" "src/main.lisp" :as "hex" :max-lines 1)
      (expect kind :to-be :partial)
      (expect (field fields "end") :to-be 16)
      (expect (field fields "next_commands")
              :to-equal '("aitools archive read a.tar src/main.lisp --as hex --max-lines 2"))))

  (it "describes a binary member instead of printing it"
    (let ((zip (aitools.text.domain:write-zip (list (archive-member "img.png" (coerce #(137 80 78 71 13 10 26 10 0 0 0 13) '(vector (unsigned-byte 8))))))))
      (multiple-value-bind (kind fields)
          (run-flow #'archive-read-flow (make-test-ports :files `(("/work/b.zip" . ,zip))) "b.zip" "img.png")
        (expect kind :to-be :ok)
        (expect (field fields "binary") :to-be t)
        (expect (field fields "size") :to-be 12)
        (expect (field fields "lines") :to-be nil))))

  (it "reads a gz member by its stored name and offers it for any other name"
    (expect (field (nth-value 1 (run-archive #'archive-read-flow "notes.txt.gz" "notes.txt")) "lines")
            :to-equal '("hello" "gz"))
    (multiple-value-bind (kind fields) (run-archive #'archive-read-flow "notes.txt.gz" "other.txt")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (json-object-get (first (getf fields :candidates)) "path") :to-equal "notes.txt")))

  (it "rejects --as hex with a selector"
    (multiple-value-bind (kind fields) (run-archive #'archive-read-flow "a.zip" "docs/readme.txt" :as "hex" :range "1")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (getf fields :message) :to-equal "--range, --as hex cannot be combined: one selector per call")))

  (it "reports a member whose compressed data is corrupt as input.unsupported-format"
    (let* ((zip (copy-seq (aitools.text.domain:write-zip
                           (list (archive-member "z.txt" (make-string 200 :initial-element #\z))))))
           ;; The local header is 30 bytes plus the name; the deflated data follows it.
           (data-start (+ 30 (length "z.txt"))))
      (setf (aref zip data-start) (logxor (aref zip data-start) #xFF))
      (multiple-value-bind (kind fields)
          (run-flow #'archive-read-flow (make-test-ports :files `(("/work/bad.zip" . ,zip))) "bad.zip" "z.txt")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.unsupported-format")
        (expect (getf fields :message) :to-contain "entry z.txt of bad.zip cannot be decoded")
        (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools archive list bad.zip"))))

  (it "refuses a tar member larger than the entry limit before copying it"
    (let ((tar (aitools.text.domain:write-tar
                (list (aitools.text.domain:make-archive-member
                       :name "big.bin" :kind :file :mode #o644 :mtime 1700000000
                       :data (make-array (1+ +archive-max-entry+) :element-type '(unsigned-byte 8)
                                                                  :initial-element 0))))))
      (multiple-value-bind (kind fields)
          (run-flow #'archive-read-flow (make-test-ports :files `(("/work/big.tar" . ,tar))) "big.tar" "big.bin")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "refusal.too-large")
        (expect (getf fields :message)
                :to-equal (format nil "entry big.bin of big.tar is larger than ~D bytes" +archive-max-entry+))))))

(describe "archive read of a member with a line past the per-line byte cap"
  (it "cuts the line, marks it, and names the hex dump through the cut"
    (let ((zip (aitools.text.domain:write-zip
                (list (archive-member "long.txt" (format nil "~A~%" (make-string 20000 :initial-element #\a)))))))
      (multiple-value-bind (kind fields)
          (run-flow #'archive-read-flow (make-test-ports :files `(("/work/l.zip" . ,zip))) "l.zip" "long.txt")
        (expect kind :to-be :partial)
        (expect (length (first (field fields "lines"))) :to-be 16384)
        (expect (field fields "cut_lines") :to-equal '(1))
        (expect (field fields "next_commands")
                :to-equal '("aitools archive read l.zip long.txt --as hex --max-lines 1040"))))))
