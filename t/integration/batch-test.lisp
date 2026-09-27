;;;; t/integration/batch-test.lisp
;;;;
;;;; `batch` end to end through the composition root: elements run
;;;; through the same dispatch as standalone calls, stop at the first
;;;; failure unless --continue-on-error, never nest, and under --atomic
;;;; become one tx that commits as one op or writes nothing. Standard input
;;;; is the batch document, bound for each call; XDG_STATE_HOME points at a
;;;; temporary directory so no journal reaches the user's state.
;;;; Elements refused before they run and the --atomic tx plumbing are in
;;;; batch-failure-test.lisp.
(in-package #:cl-user)

(defpackage #:aitools.integration.batch-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect))

(in-package #:aitools.integration.batch-test)

(defvar *root* nil "The current test workspace's real root, no trailing slash.")

(defun call-with-batch-workspace (function)
  (let* ((base (sb-posix:mkdtemp (format nil "~A/aitools-batch-XXXXXX"
                                         (string-right-trim "/" (or (sb-posix:getenv "TMPDIR") "/tmp")))))
         (base (string-right-trim "/" (sb-ext:native-namestring (truename (concatenate 'string base "/")))))
         (previous (sb-posix:getenv "XDG_STATE_HOME")))
    (sb-posix:mkdir (concatenate 'string base "/work") #o755)
    (sb-posix:mkdir (concatenate 'string base "/state") #o755)
    (unwind-protect
         (let ((*root* (concatenate 'string base "/work"))
               (previous-cwd (sb-posix:getcwd)))
           (sb-posix:setenv "XDG_STATE_HOME" (concatenate 'string base "/state") 1)
           ;; Element paths are relative to the working directory, as typed.
           (sb-posix:chdir *root*)
           (unwind-protect (funcall function)
             (sb-posix:chdir previous-cwd)))
      (if previous
          (sb-posix:setenv "XDG_STATE_HOME" previous 1)
          (sb-posix:unsetenv "XDG_STATE_HOME"))
      (uiop:delete-directory-tree (sb-ext:parse-native-namestring (concatenate 'string base "/"))
                                  :validate (lambda (path) (search "aitools-batch-" (namestring path)))
                                  :if-does-not-exist :ignore))))

(defmacro with-batch-workspace (() &body body)
  `(call-with-batch-workspace (lambda () ,@body)))

(defun disk (relative)
  (concatenate 'string *root* "/" relative))

(defun put (relative text)
  (with-open-file (out (sb-ext:parse-native-namestring (disk relative)) :direction :output :if-exists :supersede
                                                                        :external-format :utf-8)
    (write-string text out))
  relative)

(defun text (relative)
  (with-open-file (in (sb-ext:parse-native-namestring (disk relative)) :if-does-not-exist nil :external-format :utf-8)
    (if in (let ((string (make-string (file-length in)))) (subseq string 0 (read-sequence string in))) :absent)))

(defun run-aitools (arguments &key stdin)
  "(values exit-code envelope stream) of `aitools --root <workspace> ARGUMENTS`
with standard input reading STDIN (a string)."
  (multiple-value-bind (app registry) (aitools/cli:build-app)
    (let* ((out (make-string-output-stream))
           (err (make-string-output-stream))
           (code (with-input-from-string (*standard-input* (or stdin ""))
                   (aitools/cli:dispatch app registry (list* "aitools" "--root" *root* arguments)
                                         :stdout out :stderr err)))
           (stdout (get-output-stream-string out))
           (stderr (get-output-stream-string err)))
      (if (plusp (length stdout))
          (values code (json-kit:parse stdout) :stdout)
          (values code (json-kit:parse stderr) :stderr)))))

(defun run-batch (argvs &rest flags)
  (run-aitools (list* "batch" "--stdin" flags) :stdin (json-kit:stringify (coerce (mapcar (lambda (argv) (coerce argv 'vector)) argvs) 'vector))))

(defun value (object &rest keys)
  (reduce (lambda (value key)
            (cond ((null value) nil)
                  ((integerp key) (and (< key (length value)) (aref value key)))
                  (t (gethash key value))))
          keys :initial-value object))

(defun statuses (results)
  (map 'list (lambda (result) (value result "status")) results))

(defun history-count ()
  (length (value (nth-value 1 (run-aitools '("history"))) "items")))

(defun open-tx-count ()
  (length (value (nth-value 1 (run-aitools '("tx" "status"))) "items")))

(describe "aitools batch"
  (it "runs every element through dispatch and returns their envelopes in order"
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (multiple-value-bind (code envelope stream)
          (run-batch (list (list "read" (disk "a.txt"))
                           (list "edit" "a.txt" "--old" "one" "--new" "two")
                           (list "util" "calc" "1+1")))
        (expect (list code stream) :to-equal '(0 :stdout))
        (expect (value envelope "command") :to-equal "batch")
        (let ((results (value envelope "results")))
          (expect (statuses results) :to-equal '("ok" "ok" "ok"))
          (expect (map 'list (lambda (result) (value result "command")) results) :to-equal '("read" "edit" "util calc"))
          (expect (coerce (value results 0 "lines") 'list) :to-equal '("one"))
          (expect (stringp (value results 1 "op_id")) :to-be t)
          (expect (value results 2 "result") :to-equal "2")))
      (expect (text "a.txt") :to-equal (format nil "two~%"))))

  (it "stops at the first failure, skips the rest, and exits with that element's code"
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (multiple-value-bind (code envelope stream)
          (run-batch (list (list "edit" "a.txt" "--old" "one" "--new" "two")
                           (list "edit" "a.txt" "--old" "absent" "--new" "x")
                           (list "edit" "a.txt" "--old" "two" "--new" "three")))
        (expect (list code stream) :to-equal '(2 :stderr))
        (expect (value envelope "error" "code") :to-equal "selection.no-match")
        (expect (value envelope "error" "exit_code") :to-be 2)
        (expect (plusp (length (value envelope "error" "repairs"))) :to-be t)
        (let ((results (value envelope "error" "diagnostics")))
          (expect (statuses results) :to-equal '("ok" "error" "skipped"))
          (expect (coerce (value results 2 "argv") 'list) :to-equal '("edit" "a.txt" "--old" "two" "--new" "three"))))
      ;; Without --atomic the first element committed on its own.
      (expect (text "a.txt") :to-equal (format nil "two~%"))))

  (it "runs every element with --continue-on-error and still exits with the first failure's code"
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (multiple-value-bind (code envelope)
          (run-batch (list (list "read" (disk "missing.txt"))
                           (list "edit" "a.txt" "--old" "absent" "--new" "x")
                           (list "edit" "a.txt" "--old" "one" "--new" "two"))
                     "--continue-on-error")
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "input.not-found")
        (expect (statuses (value envelope "error" "diagnostics")) :to-equal '("error" "error" "ok")))
      (expect (text "a.txt") :to-equal (format nil "two~%"))))

  (it "refuses a nested batch and runs nothing"
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (multiple-value-bind (code envelope)
          (run-batch (list (list "edit" "a.txt" "--old" "one" "--new" "two")
                           (list "batch" "--stdin")))
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "argument.invalid"))
      (expect (text "a.txt") :to-equal (format nil "one~%"))))

  (it "requires --stdin and rejects input that is not an array of argv arrays"
    (with-batch-workspace ()
      (multiple-value-bind (code envelope) (run-aitools '("batch"))
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "argument.invalid"))
      (multiple-value-bind (code envelope) (run-aitools '("batch" "--stdin") :stdin "[[\"read\"")
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "input.syntax-error"))
      (multiple-value-bind (code envelope) (run-aitools '("batch" "--stdin") :stdin "[\"read\", \"a\"]")
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "argument.invalid"))
      (multiple-value-bind (code envelope) (run-batch '())
        (expect code :to-be 0)
        (expect (length (value envelope "results")) :to-be 0))))

  (it "is listed by schema with its rules"
    (with-batch-workspace ()
      (multiple-value-bind (code envelope) (run-aitools '("schema" "batch"))
        (expect code :to-be 0)
        (expect (value envelope "commands" 0 "name") :to-equal "batch")))))

(describe "aitools batch --atomic"
  (it "commits every element as one op that one undo reverses"
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (put "b.txt" (format nil "bee~%"))
      (let ((ops-before (history-count)))
        (multiple-value-bind (code envelope)
            (run-batch (list (list "edit" "a.txt" "--old" "one" "--new" "two")
                             (list "read" (disk "a.txt"))
                             (list "move" "b.txt" "c.txt"))
                       "--atomic")
          (expect code :to-be 0)
          (let ((results (value envelope "results"))
                (tx (value envelope "tx")))
            (expect (statuses results) :to-equal '("ok" "ok" "ok"))
            (expect (value results 0 "tx") :to-equal tx)
            (expect (value results 0 "tx_op") :to-be 1)
            ;; The read saw the tx state, not the disk.
            (expect (coerce (value results 1 "lines") 'list) :to-equal '("two"))
            (expect (value results 2 "tx_op") :to-be 2))
          (expect (text "a.txt") :to-equal (format nil "two~%"))
          (expect (text "c.txt") :to-equal (format nil "bee~%"))
          (expect (text "b.txt") :to-be :absent)
          (expect (history-count) :to-be (1+ ops-before))
          (expect (open-tx-count) :to-be 0)
          (expect (run-aitools (list "undo" (value envelope "op_id"))) :to-be 0))
        (expect (text "a.txt") :to-equal (format nil "one~%"))
        (expect (text "b.txt") :to-equal (format nil "bee~%"))
        (expect (text "c.txt") :to-be :absent))))

  (it "writes nothing and leaves no tx when an element fails"
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (let ((ops-before (history-count)))
        (multiple-value-bind (code envelope)
            (run-batch (list (list "edit" "a.txt" "--old" "one" "--new" "two")
                             (list "edit" "a.txt" "--old" "absent" "--new" "x")
                             (list "util" "calc" "1"))
                       "--atomic")
          (expect code :to-be 2)
          (expect (value envelope "error" "code") :to-equal "selection.no-match")
          (expect (statuses (value envelope "error" "diagnostics")) :to-equal '("ok" "error" "skipped")))
        (expect (text "a.txt") :to-equal (format nil "one~%"))
        (expect (history-count) :to-be ops-before)
        (expect (open-tx-count) :to-be 0))))

  (it "writes nothing when the commit conflicts with a change made during the batch"
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (put "b.txt" (format nil "bee~%"))
      ;; After the last element is staged (tx op 2), change a.txt on disk
      ;; behind the tx's back: the commit then meets a write conflict.
      (let ((aitools.store.application:*fault-hook*
              (lambda (point &rest details)
                (when (and (eq point :tx-after-index) (eql (second details) 2))
                  (put "a.txt" (format nil "external~%"))))))
        (multiple-value-bind (code envelope)
            (run-batch (list (list "edit" "a.txt" "--old" "one" "--new" "two")
                             (list "edit" "b.txt" "--old" "bee" "--new" "wasp"))
                       "--atomic")
          (expect code :to-be 2)
          (expect (value envelope "error" "code") :to-equal "refusal.target-changed")
          (expect (value envelope "error" "conflicts" 0 "path") :to-equal "a.txt")
          (expect (search "batch --stdin --atomic" (value envelope "error" "repairs" 0 "command")) :to-be-truthy)))
      (expect (text "a.txt") :to-equal (format nil "external~%"))
      (expect (text "b.txt") :to-equal (format nil "bee~%"))
      (expect (open-tx-count) :to-be 0)))

  (it "refuses an element carrying --tx, and --atomic with --continue-on-error, before opening a tx"
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (multiple-value-bind (code envelope)
          (run-batch (list (list "edit" "a.txt" "--old" "one" "--new" "two" "--tx" "tx-x")) "--atomic")
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "argument.invalid"))
      (multiple-value-bind (code envelope)
          (run-batch (list (list "util" "calc" "1")) "--atomic" "--continue-on-error")
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "argument.invalid"))
      (expect (open-tx-count) :to-be 0)
      (expect (text "a.txt") :to-equal (format nil "one~%"))))

  (it "renames every file find lists in one atomic batch of moves"
    (with-batch-workspace ()
      (sb-posix:mkdir (disk "logs") #o755)
      (let ((names (loop for n from 1 to 12 collect (format nil "logs/app-~2,'0D.log" n))))
        (dolist (name names) (put name name))
        (put "logs/keep.txt" "keep")
        (let* ((found (nth-value 1 (run-aitools (list "find" "*.log" (disk "logs") "--type" "file"))))
               (paths (map 'list (lambda (item) (value item "path")) (value found "items")))
               (moves (mapcar (lambda (path)
                                (list "move" path (concatenate 'string (subseq path 0 (- (length path) 4)) ".txt")))
                              paths))
               (ops-before (history-count)))
          (expect (length paths) :to-be 12)
          (multiple-value-bind (code envelope) (apply #'run-batch moves '("--atomic"))
            (expect code :to-be 0)
            (expect (length (value envelope "changes")) :to-be 24))
          (expect (history-count) :to-be (1+ ops-before))
          (dolist (name names)
            (let ((renamed (concatenate 'string (subseq name 0 (- (length name) 4)) ".txt")))
              (expect (text name) :to-be :absent)
              (expect (text renamed) :to-equal name)))
          (expect (text "logs/keep.txt") :to-equal "keep"))))))
