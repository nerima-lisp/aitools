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
  (it "preserves candidate lines when an archive selector matches nothing"
    (multiple-value-bind (kind fields)
        (run-archive #'archive-read-flow "a.zip" "docs/readme.txt" :match "threeX")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "selection.no-match")
      (let ((candidate (first (getf fields :candidates))))
        (expect (json-object-get candidate "text") :to-equal "three")
        (expect (json-object-get candidate "line") :to-be 3))
      (expect (null (getf fields :repairs)) :to-be nil)))
  (it "forwards invalid-regex repairs from archive member text"
    (multiple-value-bind (kind fields)
        (run-archive #'archive-read-flow "a.zip" "docs/readme.txt" :match "(")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.syntax-error")
      (expect (null (getf fields :repairs)) :to-be nil)))

  (it "fails with input.not-found and near entry names for a missing entry"
    (multiple-value-bind (kind fields) (run-archive #'archive-read-flow "a.tar" "docs/redme.txt")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (json-object-get (first (getf fields :candidates)) "path") :to-equal "docs/readme.txt")))

  (it "refuses a directory entry and requires an entry for multi-member archives"
    (expect (error-code (nth-value 1 (run-archive #'archive-read-flow "a.zip" "docs"))) :to-equal "refusal.not-a-file")
    (expect (error-code (nth-value 1 (run-archive #'archive-read-flow "a.zip" nil))) :to-equal "argument.invalid"))

  (it "refuses a gzip stream that expands past the entry limit"
    (let ((original
            (symbol-function
             (quote aitools.text.domain:gzip-decompress)))
          (observed-limit nil))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.text.domain:gzip-decompress))
            (lambda (octets &key max-output)
              (setf observed-limit max-output)
              (funcall original octets :max-output max-output))))
        (let ((bomb
                (deflate-kit:gzip-encode
                 (make-array (1+ +archive-max-entry+)
                             :element-type (quote (unsigned-byte 8))
                             :initial-element 0)
                 :level 0)))
          (multiple-value-bind (kind fields)
              (run-flow
                (symbol-function (quote archive-read-flow))
                (make-test-ports :files `(("/work/zeros.gz" . ,bomb)))
                "zeros.gz"
                nil)
            (expect kind :to-be :error)
            (expect (error-code fields) :to-equal "refusal.too-large")))
        (expect observed-limit :to-be +archive-max-entry+)))))

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

  (it "refuses a tar member larger than the entry limit before reading its data"
    (let* ((entry (aitools.text.domain::make-archive-entry
                   :format :tar
                   :name "big.bin"
                   :kind :file
                   :size (1+ +archive-max-entry+)
                   :mode #o644
                   :mtime 1700000000))
           (item (aitools.inspect.domain::make-archive-item
                  "big.bin" :file (1+ +archive-max-entry+) #o644 1700000000 entry))
           (archive (aitools.inspect.domain::make-archive
                     :tar (list item)
                     (make-array 0 :element-type '(unsigned-byte 8)))))
      (multiple-value-bind (kind limit)
          (archive-item-content/k
           archive item
           :on-content (lambda (content) (values :content content))
           :on-too-large (lambda (maximum) (values :too-large maximum))
           :on-malformed (lambda (reason) (values :malformed reason)))
        (expect kind :to-be :too-large)
        (expect limit :to-be +archive-max-entry+)))))

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
