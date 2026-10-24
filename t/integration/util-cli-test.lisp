;;;; t/integration/util-cli-test.lisp
;;;;
;;;; `util` end to end through the composition root: argv parsing, the
;;;; production ports (OS random source, real file reads), the envelope, and
;;;; the exit code. Standard input is exercised by pointing file
;;;; descriptor 0 at a file for the duration of a spec.
(in-package #:cl-user)

(defpackage #:aitools.integration.util-cli-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect))

(in-package #:aitools.integration.util-cli-test)

(defun run-util-command (&rest arguments)
  (aitools.test.support:run-aitools (uiop:native-namestring (uiop:getcwd)) arguments))

(describe "aitools util commands through dispatch"
  (it "evaluates calc with big integers and an exact rational"
    (multiple-value-bind (code envelope stream) (run-util-command "util" "calc" "1/3 + 2**100")
      (expect code :to-be 0)
      (progn (expect stream :to-be :stdout) (expect envelope :to-match-envelope "status" "ok" "command" "util calc"))
      (expect (aitools.test.support:envelope-value envelope "command") :to-equal "util calc")
      (expect (aitools.test.support:envelope-value envelope "result") :to-equal "1267650600228229401496703205376.3333333333")
      (expect (aitools.test.support:envelope-value envelope "exact") :to-equal "3802951800684688204490109616129/3")))

  (it "exits 1 with input.syntax-error on division by zero"
    (multiple-value-bind (code envelope stream) (run-util-command "util" "calc" "1/(3-3)")
      (expect code :to-be 1)
      (progn (expect stream :to-be :stderr) (expect envelope :to-match-envelope "status" "error" "command" "util calc"))
      (expect (aitools.test.support:envelope-value envelope "error" "code") :to-equal "input.syntax-error")))

  (it "exits 1 on invalid decode input and on a missing input source"
    (expect (run-util-command "util" "decode" "hex" "--content" "xyz") :to-be 1)
    (multiple-value-bind (code envelope) (run-util-command "util" "encode" "base64")
      (expect code :to-be 1)
      (expect (aitools.test.support:envelope-value envelope "error" "code") :to-equal "argument.invalid")))

  (it "round-trips a binary file read with --content-file"
    (uiop:with-temporary-file (:pathname path :type "bin" :element-type '(unsigned-byte 8)
                               :stream stream :direction :output)
      (write-sequence (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(0 255 1 254)) stream)
      (finish-output stream)
      (multiple-value-bind (code envelope) (run-util-command "util" "encode" "base64" "--content-file"
                                                        (uiop:native-namestring path))
        (expect code :to-be 0)
        (multiple-value-bind (decode-code decoded)
            (run-util-command "util" "decode" "base64" "--content" (aitools.test.support:envelope-value envelope "output"))
          (expect decode-code :to-be 0)
          (expect (aitools.test.support:envelope-value decoded "binary") :to-be t)
          (expect (aitools.test.support:envelope-value decoded "output_hex") :to-equal "00ff01fe")))))

  (it "reports input.not-found for a missing --content-file"
    (multiple-value-bind (code envelope) (run-util-command "util" "tokens" "--content-file" "/nonexistent/aitools-util-test")
      (expect code :to-be 1)
      (expect (aitools.test.support:envelope-value envelope "error" "code") :to-equal "input.not-found")))

  (it "generates distinct v4 UUIDs and time-ordered v7 UUIDs from the OS source"
    (let ((v4 (coerce (aitools.test.support:envelope-value (nth-value 1 (run-util-command "util" "uuid" "--count" "50")) "values") 'list))
          (v7 (coerce (aitools.test.support:envelope-value (nth-value 1 (run-util-command "util" "uuid" "--kind" "v7" "--count" "50")) "values")
                      'list)))
      (expect (length (remove-duplicates v4 :test #'string=)) :to-be 50)
      (expect (every (lambda (uuid) (char= (char uuid 14) #\4)) v4) :to-be-truthy)
      (expect (every (lambda (uuid) (char= (char uuid 14) #\7)) v7) :to-be-truthy)
      (expect (loop for (a b) on v7 while b always (string< a b)) :to-be-truthy)))

  (it "generates random strings of the requested length and alphabet"
    (let ((values (coerce (aitools.test.support:envelope-value (nth-value 1 (run-util-command "util" "random" "--length" "64" "--count" "3"))
                                        "values")
                          'list)))
      (expect (length values) :to-be 3)
      (expect (every (lambda (value) (and (= (length value) 64) (every (lambda (c) (digit-char-p c 16)) value)))
                     values)
              :to-be-truthy)))

  (it "rejects an unknown --alphabet choice at the argument parser"
    (expect (run-util-command "util" "random" "--alphabet" "emoji") :to-be 1))

  (it "lists every util command in the schema"
    (let ((names (map 'list (lambda (entry) (gethash "name" entry))
                      (aitools.test.support:envelope-value (nth-value 1 (run-util-command "schema")) "commands"))))
      (dolist (name '("util encode" "util decode" "util redact" "util tokens" "util calc" "util uuid" "util random"))
        (expect (find name names :test #'string=) :to-equal name)))))

(describe "aitools util decode --to"
  (it "writes the decoded bytes as a new file through the journal, and undo removes it"
    (aitools.test.support:with-workspace (root workspace-state)
      (declare (ignore workspace-state))
      (let ((out (concatenate 'string root "/out.bin")))
        (multiple-value-bind (code envelope stream)
            (aitools.test.support:run-aitools root (list "util" "decode" "base64" "--content" "AP8B/g==" "--to" out))
          (expect (list code stream) :to-equal '(0 :stdout))
          (expect (aitools.test.support:envelope-value envelope "bytes") :to-be 4)
          (expect (aitools.test.support:envelope-value envelope "output") :to-be nil)
          (let ((change (aref (aitools.test.support:envelope-value envelope "changes") 0)))
            (expect (aitools.test.support:envelope-value change "path") :to-equal "out.bin")
            (expect (aitools.test.support:envelope-value change "action") :to-equal "created"))
          (expect (coerce (aitools.test.support:read-bytes out) (quote list)) :to-equal '(0 255 1 254))
          (let ((op (aitools.test.support:envelope-value envelope "op_id")))
            (expect (stringp op) :to-be t)
            (multiple-value-bind (code history) (aitools.test.support:run-aitools root (list "history"))
              (expect code :to-be 0)
              (expect (aitools.test.support:envelope-value (aref (aitools.test.support:envelope-value history "items") 0) "op_id") :to-equal op))
            (expect (aitools.test.support:run-aitools root (list "undo" op)) :to-be 0)
            (expect (probe-file out) :to-be nil))))))

  (it "refuses an existing target with refusal.exists and leaves it unchanged"
    (aitools.test.support:with-workspace (root workspace-state)
      (declare (ignore workspace-state))
      (let ((out (concatenate 'string root "/out.txt")))
        (with-open-file (stream out :direction :output) (write-string "keep" stream))
        (multiple-value-bind (code envelope stream)
            (aitools.test.support:run-aitools root (list "util" "decode" "hex" "--content" "6869" "--to" out))
          (expect (list code stream) :to-equal '(1 :stderr))
          (expect (aitools.test.support:envelope-value envelope "error" "code") :to-equal "refusal.exists")
          (expect (plusp (length (aitools.test.support:envelope-value envelope "error" "repairs"))) :to-be t))
        (expect (coerce (aitools.test.support:read-bytes out) (quote list)) :to-equal (coerce (sb-ext:string-to-octets "keep") 'list)))))

  (it "allows util decode --to to write below mktemp"
    (aitools.test.support:with-workspace (root workspace-state)
      (declare (ignore workspace-state))
      (multiple-value-bind (tmp-code tmp-envelope tmp-stream)
          (aitools.test.support:run-aitools root (list "mktemp" "--dir"))
        (expect (list tmp-code tmp-stream) :to-equal (list 0 :stdout))
        (let ((out (concatenate 'string
                                (aitools.test.support:envelope-value tmp-envelope "path")
                                "/decoded.bin")))
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools
               root
               (list "util" "decode" "base64" "--content" "AP8B/g==" "--to" out))
            (expect (list code stream) :to-equal (list 0 :stdout))
            (expect (aitools.test.support:envelope-value envelope "bytes") :to-be 4)
            (expect (coerce (aitools.test.support:read-bytes out) (quote list))
                    :to-equal (list 0 255 1 254)))))))

  (it "writes nothing with --dry-run and stages the file with --tx until commit"
    (aitools.test.support:with-workspace (root workspace-state)
      (declare (ignore workspace-state))
      (let ((out (concatenate 'string root "/sub/hi.txt")))
        (multiple-value-bind (code envelope) (aitools.test.support:run-aitools root (list "util" "decode" "hex" "--content" "6869"
                                                          "--to" out "--dry-run"))
          (expect code :to-be 0)
          (expect (aitools.test.support:envelope-value envelope "dry_run") :to-be t)
          (expect (aitools.test.support:envelope-value envelope "op_id") :to-be nil))
        (expect (probe-file out) :to-be nil)
        (let ((tx (aitools.test.support:envelope-value (nth-value 1 (aitools.test.support:run-aitools root (list "tx" "begin"))) "tx")))
          (multiple-value-bind (code envelope) (aitools.test.support:run-aitools root (list "util" "decode" "hex" "--content" "6869"
                                                            "--to" out "--tx" tx))
            (expect code :to-be 0)
            (expect (aitools.test.support:envelope-value envelope "tx") :to-equal tx)
            (expect (aitools.test.support:envelope-value envelope "tx_op") :to-be 1))
          (expect (probe-file out) :to-be nil)
          (expect (aitools.test.support:run-aitools root (list "tx" "commit" tx)) :to-be 0)
          (expect (coerce (aitools.test.support:read-bytes out) (quote list)) :to-equal (coerce (sb-ext:string-to-octets "hi") 'list))))))

  (it "refuses a target outside the workspace, and --dry-run or --tx without --to"
    (aitools.test.support:with-workspace (root workspace-state)
      (declare (ignore workspace-state))
      (multiple-value-bind (code envelope) (aitools.test.support:run-aitools root (list "util" "decode" "hex" "--content" "6869"
                                                        "--to" (concatenate 'string root "/../escape.txt")))
        (expect code :to-be 1)
        (expect (aitools.test.support:envelope-value envelope "error" "code") :to-equal "refusal.outside-workspace"))
      (expect (probe-file (concatenate 'string root "/../escape.txt")) :to-be nil)
      (multiple-value-bind (code envelope) (aitools.test.support:run-aitools root (list "util" "decode" "hex" "--content" "6869" "--dry-run"))
        (expect code :to-be 1)
        (expect (aitools.test.support:envelope-value envelope "error" "code") :to-equal "argument.invalid")))))

(defun reader-outcome (reader &rest arguments)
  (multiple-value-bind (status result)
      (apply reader arguments)
    (case status
      (:octets (list status (coerce result (quote list))))
      (:failure (list status (stringp result)))
      (otherwise (list status)))))

(describe "aitools util production input readers"
  (it "reads a file's octets up to the limit, and reports a missing, oversized, or unreadable one"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (let ((file (uiop:native-namestring (merge-pathnames "in.bin" directory)))
            (read #'aitools.util.infrastructure::read-file-octets))
        (aitools.test.support:write-bytes file
          (coerce '(0 255 7) (quote (vector (unsigned-byte 8)))))
        (expect (reader-outcome read file 3) :to-equal '(:octets (0 255 7)))
        (expect (reader-outcome read file 2) :to-equal '(:too-large))
        (expect (reader-outcome read (uiop:native-namestring (merge-pathnames "none" directory)) 3) :to-equal '(:missing))
        (expect (reader-outcome read (uiop:native-namestring directory) 3) :to-equal '(:failure t))))
    :prefix "aitools-process"))

  (it "reads standard input's octets up to the limit, and reports an unreadable one"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (let ((file (uiop:native-namestring (merge-pathnames "in.bin" directory)))
            (read #'aitools.util.infrastructure::read-stdin-octets))
        (aitools.test.support:write-bytes file
          (coerce '(1 2 3) (quote (vector (unsigned-byte 8)))))
        (aitools.test.support:with-stdin-from (file) (expect (reader-outcome read 3) :to-equal '(:octets (1 2 3))))
        (aitools.test.support:with-stdin-from (file) (expect (reader-outcome read 2) :to-equal '(:too-large)))
        (aitools.test.support:with-stdin-from ((uiop:native-namestring directory)) (expect (reader-outcome read 3) :to-equal '(:failure t)))))
    :prefix "aitools-process")))

(describe "aitools util --stdin and redact through dispatch"
  (it "encodes and evaluates text read from standard input"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (let ((file (uiop:native-namestring (merge-pathnames "in.txt" directory))))
        (aitools.test.support:write-bytes file
          (coerce (map 'list #'char-code (format nil "6 * 7~%")) (quote (vector (unsigned-byte 8)))))
        (aitools.test.support:with-stdin-from (file)
          (expect (aitools.test.support:envelope-value (nth-value 1 (run-util-command "util" "calc" "--stdin")) "result") :to-equal "42"))
        (aitools.test.support:with-stdin-from (file)
          (expect (aitools.test.support:envelope-value (nth-value 1 (run-util-command "util" "encode" "hex" "--stdin")) "output")
                  :to-equal "36202a20370a"))))
    :prefix "aitools-process"))

  (it "masks secrets with util redact"
    (multiple-value-bind (code envelope) (run-util-command "util" "redact" "--content" "password=hunter2")
      (expect code :to-be 0)
      (expect (aitools.test.support:envelope-value envelope "text") :to-equal "password=[REDACTED_SECRET]")
      (expect (aitools.test.support:envelope-value envelope "redactions") :to-be 1))))

(describe "aitools util bounded file reader"
  (it "reads files across a 64 KiB chunk boundary and enforces the supplied limit"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (let* ((file (uiop:native-namestring (merge-pathnames "large.bin" directory)))
             (reader (function aitools.util.infrastructure::read-file-octets))
             (contents (make-array 65537
                                   :element-type (quote (unsigned-byte 8))
                                   :initial-element #xa5)))
        (aitools.test.support:write-bytes file contents)
        (multiple-value-bind (status octets) (funcall reader file 65537)
          (expect status :to-be :octets)
          (expect octets :to-equalp contents))
        (expect (reader-outcome reader file 65536) :to-equal (list :too-large))))
    :prefix "aitools-process")))
