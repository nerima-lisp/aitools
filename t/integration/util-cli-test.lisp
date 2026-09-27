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

(defun run-aitools (&rest arguments)
  "Return (VALUES EXIT-CODE ENVELOPE STREAM) where ENVELOPE is the parsed
JSON hash table and STREAM is :STDOUT or :STDERR, whichever received it."
  (multiple-value-bind (app registry) (aitools/cli:build-app)
    (let* ((out (make-string-output-stream))
           (err (make-string-output-stream))
           (code (aitools/cli:dispatch app registry (cons "aitools" arguments) :stdout out :stderr err))
           (stdout (get-output-stream-string out))
           (stderr (get-output-stream-string err)))
      (if (plusp (length stdout))
          (values code (json-kit:parse stdout) :stdout)
          (values code (json-kit:parse stderr) :stderr)))))

(defun member-value (object &rest keys)
  (reduce (lambda (value key) (and value (gethash key value))) keys :initial-value object))

(describe "aitools util commands through dispatch"
  (it "evaluates calc with big integers and an exact rational"
    (multiple-value-bind (code envelope stream) (run-aitools "util" "calc" "1/3 + 2**100")
      (expect code :to-be 0)
      (expect stream :to-be :stdout)
      (expect (member-value envelope "command") :to-equal "util calc")
      (expect (member-value envelope "result") :to-equal "1267650600228229401496703205376.3333333333")
      (expect (member-value envelope "exact") :to-equal "3802951800684688204490109616129/3")))

  (it "exits 1 with input.syntax-error on division by zero"
    (multiple-value-bind (code envelope stream) (run-aitools "util" "calc" "1/(3-3)")
      (expect code :to-be 1)
      (expect stream :to-be :stderr)
      (expect (member-value envelope "error" "code") :to-equal "input.syntax-error")))

  (it "exits 1 on invalid decode input and on a missing input source"
    (expect (run-aitools "util" "decode" "hex" "--content" "xyz") :to-be 1)
    (multiple-value-bind (code envelope) (run-aitools "util" "encode" "base64")
      (expect code :to-be 1)
      (expect (member-value envelope "error" "code") :to-equal "argument.invalid")))

  (it "round-trips a binary file read with --content-file"
    (uiop:with-temporary-file (:pathname path :type "bin" :element-type '(unsigned-byte 8)
                               :stream stream :direction :output)
      (write-sequence (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(0 255 1 254)) stream)
      (finish-output stream)
      (multiple-value-bind (code envelope) (run-aitools "util" "encode" "base64" "--content-file"
                                                        (uiop:native-namestring path))
        (expect code :to-be 0)
        (multiple-value-bind (decode-code decoded)
            (run-aitools "util" "decode" "base64" "--content" (member-value envelope "output"))
          (expect decode-code :to-be 0)
          (expect (member-value decoded "binary") :to-be t)
          (expect (member-value decoded "output_hex") :to-equal "00ff01fe")))))

  (it "reports input.not-found for a missing --content-file"
    (multiple-value-bind (code envelope) (run-aitools "util" "tokens" "--content-file" "/nonexistent/aitools-util-test")
      (expect code :to-be 1)
      (expect (member-value envelope "error" "code") :to-equal "input.not-found")))

  (it "generates distinct v4 UUIDs and time-ordered v7 UUIDs from the OS source"
    (let ((v4 (coerce (member-value (nth-value 1 (run-aitools "util" "uuid" "--count" "50")) "values") 'list))
          (v7 (coerce (member-value (nth-value 1 (run-aitools "util" "uuid" "--kind" "v7" "--count" "50")) "values")
                      'list)))
      (expect (length (remove-duplicates v4 :test #'string=)) :to-be 50)
      (expect (every (lambda (uuid) (char= (char uuid 14) #\4)) v4) :to-be-truthy)
      (expect (every (lambda (uuid) (char= (char uuid 14) #\7)) v7) :to-be-truthy)
      (expect (loop for (a b) on v7 while b always (string< a b)) :to-be-truthy)))

  (it "generates random strings of the requested length and alphabet"
    (let ((values (coerce (member-value (nth-value 1 (run-aitools "util" "random" "--length" "64" "--count" "3"))
                                        "values")
                          'list)))
      (expect (length values) :to-be 3)
      (expect (every (lambda (value) (and (= (length value) 64) (every (lambda (c) (digit-char-p c 16)) value)))
                     values)
              :to-be-truthy)))

  (it "rejects an unknown --alphabet choice at the argument parser"
    (expect (run-aitools "util" "random" "--alphabet" "emoji") :to-be 1))

  (it "lists every util command in the schema"
    (let ((names (map 'list (lambda (entry) (gethash "name" entry))
                      (member-value (nth-value 1 (run-aitools "schema")) "commands"))))
      (dolist (name '("util encode" "util decode" "util redact" "util tokens" "util calc" "util uuid" "util random"))
        (expect (find name names :test #'string=) :to-equal name)))))

(defun call-with-util-workspace (function)
  "Call FUNCTION with a fresh workspace root (a real path, no trailing slash)
while XDG_STATE_HOME points at a sibling directory, so the journal never
reaches the user's state."
  (let* ((base (sb-posix:mkdtemp (format nil "~A/aitools-util-XXXXXX"
                                         (string-right-trim "/" (or (sb-posix:getenv "TMPDIR") "/tmp")))))
         (base (string-right-trim "/" (sb-ext:native-namestring (truename (concatenate 'string base "/")))))
         (root (concatenate 'string base "/work"))
         (previous (sb-posix:getenv "XDG_STATE_HOME")))
    (sb-posix:mkdir root #o755)
    (sb-posix:mkdir (concatenate 'string base "/state") #o755)
    (unwind-protect
         (progn
           (sb-posix:setenv "XDG_STATE_HOME" (concatenate 'string base "/state") 1)
           (funcall function root))
      (if previous
          (sb-posix:setenv "XDG_STATE_HOME" previous 1)
          (sb-posix:unsetenv "XDG_STATE_HOME"))
      (uiop:delete-directory-tree (sb-ext:parse-native-namestring (concatenate 'string base "/"))
                                  :validate (lambda (path) (search "aitools-util-" (namestring path)))
                                  :if-does-not-exist :ignore))))

(defmacro with-util-workspace ((root) &body body)
  `(call-with-util-workspace (lambda (,root) ,@body)))

(defun file-octets (path)
  (with-open-file (in (sb-ext:parse-native-namestring path) :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (and in
         (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
           (read-sequence octets in)
           (coerce octets 'list)))))

(describe "aitools util decode --to"
  (it "writes the decoded bytes as a new file through the journal, and undo removes it"
    (with-util-workspace (root)
      (let ((out (concatenate 'string root "/out.bin")))
        (multiple-value-bind (code envelope stream)
            (run-aitools "--root" root "util" "decode" "base64" "--content" "AP8B/g==" "--to" out)
          (expect (list code stream) :to-equal '(0 :stdout))
          (expect (member-value envelope "bytes") :to-be 4)
          (expect (member-value envelope "output") :to-be nil)
          (let ((change (aref (member-value envelope "changes") 0)))
            (expect (member-value change "path") :to-equal "out.bin")
            (expect (member-value change "action") :to-equal "created"))
          (expect (file-octets out) :to-equal '(0 255 1 254))
          (let ((op (member-value envelope "op_id")))
            (expect (stringp op) :to-be t)
            (multiple-value-bind (code history) (run-aitools "--root" root "history")
              (expect code :to-be 0)
              (expect (member-value (aref (member-value history "items") 0) "op_id") :to-equal op))
            (expect (run-aitools "--root" root "undo" op) :to-be 0)
            (expect (probe-file out) :to-be nil))))))

  (it "refuses an existing target with refusal.exists and leaves it unchanged"
    (with-util-workspace (root)
      (let ((out (concatenate 'string root "/out.txt")))
        (with-open-file (stream out :direction :output) (write-string "keep" stream))
        (multiple-value-bind (code envelope stream)
            (run-aitools "--root" root "util" "decode" "hex" "--content" "6869" "--to" out)
          (expect (list code stream) :to-equal '(1 :stderr))
          (expect (member-value envelope "error" "code") :to-equal "refusal.exists")
          (expect (plusp (length (member-value envelope "error" "repairs"))) :to-be t))
        (expect (file-octets out) :to-equal (coerce (sb-ext:string-to-octets "keep") 'list)))))

  (it "writes nothing with --dry-run and stages the file with --tx until commit"
    (with-util-workspace (root)
      (let ((out (concatenate 'string root "/sub/hi.txt")))
        (multiple-value-bind (code envelope) (run-aitools "--root" root "util" "decode" "hex" "--content" "6869"
                                                          "--to" out "--dry-run")
          (expect code :to-be 0)
          (expect (member-value envelope "dry_run") :to-be t)
          (expect (member-value envelope "op_id") :to-be nil))
        (expect (probe-file out) :to-be nil)
        (let ((tx (member-value (nth-value 1 (run-aitools "--root" root "tx" "begin")) "tx")))
          (multiple-value-bind (code envelope) (run-aitools "--root" root "util" "decode" "hex" "--content" "6869"
                                                            "--to" out "--tx" tx)
            (expect code :to-be 0)
            (expect (member-value envelope "tx") :to-equal tx)
            (expect (member-value envelope "tx_op") :to-be 1))
          (expect (probe-file out) :to-be nil)
          (expect (run-aitools "--root" root "tx" "commit" tx) :to-be 0)
          (expect (file-octets out) :to-equal (coerce (sb-ext:string-to-octets "hi") 'list))))))

  (it "refuses a target outside the workspace, and --dry-run or --tx without --to"
    (with-util-workspace (root)
      (multiple-value-bind (code envelope) (run-aitools "--root" root "util" "decode" "hex" "--content" "6869"
                                                        "--to" (concatenate 'string root "/../escape.txt"))
        (expect code :to-be 1)
        (expect (member-value envelope "error" "code") :to-equal "refusal.outside-workspace"))
      (expect (probe-file (concatenate 'string root "/../escape.txt")) :to-be nil)
      (multiple-value-bind (code envelope) (run-aitools "--root" root "util" "decode" "hex" "--content" "6869" "--dry-run")
        (expect code :to-be 1)
        (expect (member-value envelope "error" "code") :to-equal "argument.invalid")))))

(defmacro with-stdin-from ((path) &body body)
  "Run BODY with file descriptor 0 reading PATH, restoring the original
standard input afterwards."
  (let ((saved (gensym "SAVED")) (fd (gensym "FD")))
    `(let ((,saved (sb-posix:dup 0))
           (,fd (sb-posix:open ,path sb-posix:o-rdonly)))
       (unwind-protect (progn (sb-posix:dup2 ,fd 0) ,@body)
         (sb-posix:dup2 ,saved 0)
         (sb-posix:close ,saved)
         (sb-posix:close ,fd)))))

(defmacro with-temp-directory ((directory) &body body)
  `(let ((,directory (sb-posix:mkdtemp (format nil "~A/aitools-util-XXXXXX"
                                               (string-right-trim "/" (or (sb-posix:getenv "TMPDIR") "/tmp"))))))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree (uiop:ensure-directory-pathname ,directory)
                                   :validate (lambda (path) (search "aitools-util-" (namestring path)))))))

(defun write-octets (path octets)
  (with-open-file (out path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence (coerce octets '(vector (unsigned-byte 8))) out)))

(defun reader-outcome (reader &rest arguments)
  "The continuation a production input READER called, with its argument summarized."
  (apply reader (append arguments
                        (list :on-octets (lambda (octets) (list :octets (coerce octets 'list)))
                              :on-too-large (lambda () (list :too-large))
                              :on-failure (lambda (message) (list :failure (stringp message))))
                        (when (eq reader #'aitools.util.infrastructure:read-file-octets)
                          (list :on-missing (lambda () (list :missing)))))))

(describe "aitools util production input readers"
  (it "reads a file's octets up to the limit, and reports a missing, oversized, or unreadable one"
    (with-temp-directory (directory)
      (let ((file (format nil "~A/in.bin" directory))
            (read #'aitools.util.infrastructure:read-file-octets))
        (write-octets file '(0 255 7))
        (expect (reader-outcome read file 3) :to-equal '(:octets (0 255 7)))
        (expect (reader-outcome read file 2) :to-equal '(:too-large))
        (expect (reader-outcome read (format nil "~A/none" directory) 3) :to-equal '(:missing))
        (expect (reader-outcome read directory 3) :to-equal '(:failure t)))))

  (it "reads standard input's octets up to the limit, and reports an unreadable one"
    (with-temp-directory (directory)
      (let ((file (format nil "~A/in.bin" directory))
            (read #'aitools.util.infrastructure:read-stdin-octets))
        (write-octets file '(1 2 3))
        (with-stdin-from (file) (expect (reader-outcome read 3) :to-equal '(:octets (1 2 3))))
        (with-stdin-from (file) (expect (reader-outcome read 2) :to-equal '(:too-large)))
        (with-stdin-from (directory) (expect (reader-outcome read 3) :to-equal '(:failure t)))))))

(describe "aitools util --stdin and redact through dispatch"
  (it "encodes and evaluates text read from standard input"
    (with-temp-directory (directory)
      (let ((file (format nil "~A/in.txt" directory)))
        (write-octets file (map 'list #'char-code (format nil "6 * 7~%")))
        (with-stdin-from (file)
          (expect (member-value (nth-value 1 (run-aitools "util" "calc" "--stdin")) "result") :to-equal "42"))
        (with-stdin-from (file)
          (expect (member-value (nth-value 1 (run-aitools "util" "encode" "hex" "--stdin")) "output")
                  :to-equal "36202a20370a")))))

  (it "masks secrets with util redact"
    (multiple-value-bind (code envelope) (run-aitools "util" "redact" "--content" "password=hunter2")
      (expect code :to-be 0)
      (expect (member-value envelope "text") :to-equal "password=[REDACTED_SECRET]")
      (expect (member-value envelope "redactions") :to-be 1))))
