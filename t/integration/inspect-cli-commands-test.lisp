;;;; t/integration/inspect-cli-commands-test.lisp
;;;;
;;;; The `json`, `table`, `archive`, and `snapshot` groups end to end through
;;;; dispatch, and `read` of a path that is neither a file nor a directory.
;;;; Workspace helpers come from inspect-cli-test.lisp.
(in-package #:aitools.integration.inspect-cli-test)

(defun ws-file (workspace name content)
  (namestring (write-bytes (inspect-test-path workspace name) content)))

(defun envelope-list (envelope &rest keys)
  (coerce (apply #'value-at envelope keys) 'list))

(describe "json commands through dispatch"
  (it "parses json get's pointer, --keys, --raw, and --max-bytes"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let ((file (ws-file workspace "d.json" "{\"a\": {\"b\": [1, 2]}, \"s\": \"x\"}")))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "json" "get" file "/a" "--keys")
          (expect code :to-be 0)
          (expect envelope :to-match-envelope '("status" "ok" "command" "json get"))
          (expect (envelope-list envelope "keys") :to-equal '("b")))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "json" "get" file "/s" "--raw")
          (expect code :to-be 0)
          (expect (value-at envelope "text") :to-equal "x"))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "json" "get" file "/a" "--max-bytes" "4")
          (expect code :to-be 3)
          (expect (value-at envelope "value_preview") :to-equal "{\"b\""))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "json" "get" file "/a" "--max-bytes" "big")
          (expect code :to-be 1)
          (expect (value-at envelope "error" "code") :to-equal "argument.invalid")))))

  (it "parses json select's repeated --where and --pick, --sort-by, --desc, --output, and --limit"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let ((file (ws-file workspace "i.json" "[{\"id\": 1, \"n\": 5, \"k\": \"a\"}, {\"id\": 2, \"n\": 9, \"k\": \"a\"},
                                            {\"id\": 3, \"n\": 7, \"k\": \"b\"}]")))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "json" "select" file "" "--where" "/n>4" "--where" "/k=a"
                         "--pick" "/id" "--pick" "/n" "--sort-by" "/n" "--desc")
          (expect code :to-be 0)
          (expect (mapcar (lambda (item) (value-at item "value" "/id")) (envelope-list envelope "items")) :to-equal '(2 1))
          (expect (value-at envelope "items" 0 "value" "/n") :to-be 9))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "json" "select" file "" "--output" "count")
          (expect code :to-be 0)
          (expect (value-at envelope "count") :to-be 3))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "json" "select" file "" "--limit" "1")
          (expect code :to-be 3)
          (expect (value-at envelope "total") :to-be 3)))))

  (it "runs json diff with --limit"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let ((a (ws-file workspace "a.json" "{\"x\": 1, \"y\": 2}"))
            (b (ws-file workspace "b.json" "{\"x\": 2}")))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "json" "diff" a b "--limit" "1")
          (expect code :to-be 3)
          (expect (value-at envelope "total") :to-be 2)
          (expect (length (envelope-list envelope "ops")) :to-be 1))))))

(describe "table commands through dispatch"
  (it "parses table read's --columns list, repeated --where, --range, --limit, and reading options"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let ((file (ws-file workspace "p.txt" (format nil "a:1:x~%b:2:y~%c:3:x~%d:4:x~%"))))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "table" "read" file "--format" "sep" "--delimiter" ":" "--no-header"
                         "--columns" "1,2" "--where" "3=x" "--where" "2>1" "--range" "1:2" "--limit" "1"
                         "--encoding" "utf-8")
          (expect code :to-be 3)
          (expect envelope :to-match-envelope '("status" "partial" "command" "table read"))
          (expect (mapcar (lambda (row) (coerce row 'list)) (envelope-list envelope "rows")) :to-equal '(("c" 3)))
          (expect (value-at envelope "next_commands" 0) :to-contain "--range 2:2 --limit 1")))))

  (it "parses --ws-columns and --pointer"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let ((ws (ws-file workspace "w.txt" (format nil "1 a b c~%")))
            (json (ws-file workspace "j.json" "{\"r\": [{\"v\": 7}]}")))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "table" "read" ws "--format" "ws" "--no-header" "--ws-columns" "2")
          (expect code :to-be 0)
          (expect (coerce (value-at envelope "rows" 0) 'list) :to-equal '(1 "a b c")))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "table" "read" json "--pointer" "/r")
          (expect code :to-be 0)
          (expect (coerce (value-at envelope "rows" 0) 'list) :to-equal '(7))))))

  (it "rejects a --format outside its choices as a usage error"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let ((file (ws-file workspace "p.csv" (format nil "a~%1~%"))))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "table" "read" file "--format" "xml")
          (expect code :to-be 1)
          (expect (value-at envelope "error" "code") :to-equal "argument.invalid")
          (expect (value-at envelope "error" "message") :to-contain "xml")))))

  (it "parses table agg's repeated --group-by, every aggregate, --min-count, --sort, --desc, and --limit"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let ((file (ws-file workspace "s.csv" (format nil "r,i,n~%e,a,1~%e,a,3~%w,b,5~%e,b,7~%"))))
        (multiple-value-bind (code envelope)
            (run-aitools "--root" workspace "table" "agg" file "--group-by" "r" "--group-by" "i" "--count"
                         "--sum" "n" "--avg" "n" "--min" "n" "--max" "n" "--distinct" "n"
                         "--min-count" "1" "--sort" "sum" "--desc" "--limit" "2")
          (expect code :to-be 3)
          (expect (value-at envelope "total_groups") :to-be 3)
          (let ((top (value-at envelope "groups" 0)))
            (expect (list (value-at top "key" "r") (value-at top "key" "i")) :to-equal '("e" "b"))
            (expect (mapcar (lambda (name) (value-at top name)) '("count" "sum" "min" "max" "distinct"))
                    :to-equal '(1 7 7 7 1))))))))

(defun zip-of (&rest name-and-texts)
  (aitools.text.domain:write-zip
   (loop for (name text) on name-and-texts by #'cddr
         collect (aitools.text.domain:make-archive-member
                  :name name :kind :file :mode #o644 :mtime 0
                  :data (sb-ext:string-to-octets text :external-format :utf-8)))))

(describe "archive commands through dispatch"
  (it "parses archive list --limit and archive read's entry, --as, --max-lines, and selectors"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let ((file (ws-file workspace "a.zip" (zip-of "one.txt" (format nil "1~%2~%3~%") "two.txt" "2"))))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "archive" "list" file "--limit" "1")
          (expect code :to-be 3)
          (expect envelope :to-match-envelope '("status" "partial" "command" "archive list"))
          (expect (value-at envelope "total") :to-be 2))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "archive" "read" file "one.txt" "--range" "2:3"
                                                          "--max-lines" "1")
          (expect code :to-be 3)
          (expect (envelope-list envelope "lines") :to-equal '("2")))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "archive" "read" file "two.txt" "--as" "hex")
          (expect code :to-be 0)
          (expect (value-at envelope "rows" 0 "hex") :to-equal "32"))))))

(describe "snapshot commands through dispatch"
  (it "parses snapshot create's repeated --glob and filters, and snapshot diff --limit"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (ws-file workspace "a.txt" "a")
      (ws-file workspace "b.md" "b")
      (ws-file workspace "c.lisp" "c")
      (multiple-value-bind (code envelope)
          (run-aitools "--root" workspace "snapshot" "create" "--glob" "*.txt" "--glob" "*.md" "--no-ignore"
                       "--skip-larger-than" "1KiB")
        (expect code :to-be 0)
        (expect envelope :to-match-envelope '("status" "ok" "command" "snapshot create"))
        (expect (value-at envelope "files") :to-be 2)
        (let ((id (value-at envelope "snapshot_id")))
          (ws-file workspace "d.txt" "d")
          (ws-file workspace "e.txt" "e")
          (ws-file workspace "f.lisp" "f")
          (multiple-value-bind (code envelope) (run-aitools "--root" workspace "snapshot" "diff" id "--limit" "1")
            (expect code :to-be 3)
            (expect (envelope-list envelope "added") :to-equal '("d.txt")))))
      (multiple-value-bind (code envelope) (run-aitools "--root" workspace "snapshot" "create" "--lang" "markdown" "--newer" "1h")
        (expect code :to-be 0)
        (expect (value-at envelope "files") :to-be 1)))))

(describe "read of a path that is neither a file nor a directory"
  (it "refuses a FIFO as refusal.not-a-file without calling it a directory"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let ((fifo (inspect-test-path workspace "pipe")))
        (sb-posix:mkfifo fifo #o600)
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "read" fifo)
          (expect code :to-be 1)
          (expect (value-at envelope "error" "code") :to-equal "refusal.not-a-file")
          (expect (value-at envelope "error" "message") :to-equal (format nil "~A is not a regular file" fifo))
          (expect (value-at envelope "error" "repairs" 0 "command")
                  :to-equal (format nil "aitools --root ~A info ~A" workspace fifo)))))))
(describe "inspect read against the production ranged source"
  (it "reads a bounded hexadecimal window without whole-file APIs"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let* ((path (inspect-test-path workspace "big.bin"))
             (size (* 1024 1024))
             (offset 600000)
             (bytes (make-array size :element-type (quote (unsigned-byte 8)) :initial-element 0))
             (range-reads (quote ()))
             (whole-file-reads 0)
             (range-reader (function aitools.text.application:source-read-range))
             (octets-reader (function aitools.text.application:source-read-octets))
             (chunks-reader (function aitools.text.application:source-call-with-chunks))
             (sniffed-reader (function aitools.text.application:source-read-sniffed)))
        (replace bytes #(65 66 67 68) :start1 offset)
        (write-bytes path bytes)
        (cl-weave:with-mocked-functions
            (((symbol-function (quote aitools.text.application:source-read-range))
              (lambda (source file start end)
                (push (list file start end) range-reads)
                (funcall range-reader source file start end)))
             ((symbol-function (quote aitools.text.application:source-read-octets))
              (lambda (source file)
                (incf whole-file-reads)
                (funcall octets-reader source file)))
             ((symbol-function (quote aitools.text.application:source-call-with-chunks))
              (lambda (source file chunk-size function)
                (incf whole-file-reads)
                (funcall chunks-reader source file chunk-size function)))
             ((symbol-function (quote aitools.text.application:source-read-sniffed))
              (lambda (source file count continue-p)
                (incf whole-file-reads)
                (funcall sniffed-reader source file count continue-p))))
          (multiple-value-bind (code envelope)
              (run-aitools "--root" workspace "read" path "--as" "hex" "--bytes" "600000:600016")
            (expect code :to-be 0)
            (expect envelope :to-match-envelope (quote ("status" "ok" "command" "read")))
            (expect (value-at envelope "size") :to-be size)
            (expect (value-at envelope "start") :to-be offset)
            (expect (value-at envelope "end") :to-be (+ offset 16))
            (expect (value-at envelope "rows" 0 "hex") :to-contain "41 42 43 44"))
          (expect whole-file-reads :to-be 0)
          (expect (length range-reads) :to-be 1)
          (destructuring-bind (file start end) (first range-reads)
            (expect file :to-equal path)
            (expect (<= start offset) :to-be t)
            (expect (>= end (+ offset 16)) :to-be t)
            (expect (<= (- end start) (+ 16 (* 2 4096))) :to-be t)))))))
