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
  (:import-from #:cl-weave
                #:it #:it-each #:it-property #:gen-string #:expect))

(in-package #:aitools.integration.batch-test)

(defvar *root* nil "The current test workspace's real root, no trailing slash.")

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
  (it "inserts the transaction after the command rather than a matching global value"
    (multiple-value-bind (app registry) (aitools/cli:build-app)
      (declare (ignore registry))
      (expect (aitools/cli::%with-tx app '("--root" "write" "write" "a.txt" "--content" "--") "tx-test")
              :to-equal '("--root" "write" "write" "--tx" "tx-test" "a.txt" "--content" "--"))))

  (it "does not interpret an inline value as a batch-restricted option"
    (aitools.test.support:with-workspace
      (*root* state-home :chdir t)
      (declare (ignore state-home))
      (dolist (flags '(nil ("--atomic")))
        (loop for literal in '("--stdin" "--stdin=true" "--tx" "--")
              for index from 0
              for path = (format nil "literal-~A-~D.txt" (if flags "atomic" "plain") index)
              do (multiple-value-bind (code envelope)
                     (apply #'run-batch (list (list "write" path "--content" literal)) flags)
                   (expect code :to-be 0)
                   (expect (statuses (value envelope "results")) :to-equal '("ok"))
                   (expect (text path) :to-equal literal))))))

  (it "runs every element through dispatch and returns their envelopes in order"
    (aitools.test.support:with-workspace
      (*root* state-home :chdir t)
      (declare (ignore state-home))
      (put "a.txt" (format nil "one~%"))
      (multiple-value-bind (code envelope stream)
          (run-batch (list (list "read" (disk "a.txt"))
                           (list "edit" "a.txt" "--old" "one" "--new" "two")
                           (list "util" "calc" "1+1")))
        (expect (list code stream) :to-equal '(0 :stdout))
        (expect envelope :to-match-envelope "command" "batch" "status" "ok")
        (expect (value envelope "command") :to-equal "batch")
        (let ((results (value envelope "results")))
          (expect (statuses results) :to-equal '("ok" "ok" "ok"))
          (expect (map 'list (lambda (result) (value result "command")) results) :to-equal '("read" "edit" "util calc"))
          (expect (coerce (value results 0 "lines") 'list) :to-equal '("one"))
          (expect (stringp (value results 1 "op_id")) :to-be t)
          (expect (value results 2 "result") :to-equal "2")))
      (expect (text "a.txt") :to-equal (format nil "two~%"))))
  (it "stops at the first failure, skips the rest, and exits with that element's code"
    (aitools.test.support:with-workspace
      (*root* state-home :chdir t)
      (declare (ignore state-home))
      (put "a.txt" (format nil "one~%"))
      (multiple-value-bind (code envelope stream)
          (run-batch (list (list "edit" "a.txt" "--old" "one" "--new" "two")
                           (list "edit" "a.txt" "--old" "absent" "--new" "x")
                           (list "edit" "a.txt" "--old" "two" "--new" "three")))
        (expect (list code stream) :to-equal '(2 :stderr))
        (expect (value envelope "error" "code") :to-equal "selection.no-match")
        (expect envelope :to-match-envelope "command" "batch" "status" "error")
        (expect (value envelope "error" "exit_code") :to-be 2)
        (expect (plusp (length (value envelope "error" "repairs"))) :to-be t)
        (let ((results (value envelope "error" "diagnostics")))
          (expect (statuses results) :to-equal '("ok" "error" "skipped"))
          (expect (coerce (value results 2 "argv") 'list) :to-equal '("edit" "a.txt" "--old" "two" "--new" "three"))))
      ;; Without --atomic the first element committed on its own.
      (expect (text "a.txt") :to-equal (format nil "two~%"))))

  (it "runs every element with --continue-on-error and still exits with the first failure's code"
    (aitools.test.support:with-workspace
      (*root* state-home :chdir t)
      (declare (ignore state-home))
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
    (aitools.test.support:with-workspace
      (*root* state-home :chdir t)
      (declare (ignore state-home))
      (put "a.txt" (format nil "one~%"))
      (multiple-value-bind (code envelope)
          (run-batch (list (list "edit" "a.txt" "--old" "one" "--new" "two")
                           (list "batch" "--stdin")))
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "argument.invalid"))
      (expect (text "a.txt") :to-equal (format nil "one~%"))))

  (it "requires --stdin and rejects input that is not an array of argv arrays"
    (aitools.test.support:with-workspace
      (*root* state-home :chdir t)
      (declare (ignore state-home))
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
    (aitools.test.support:with-workspace
      (*root* state-home :chdir t)
      (declare (ignore state-home))
      (multiple-value-bind (code envelope)
          (run-aitools '("schema" "batch"))
        (expect code :to-be 0)
        (expect (value envelope "commands" 0 "name") :to-equal "batch")
        (let* ((detail (value envelope "commands" 0))
               (output-fields
                 (map 'list (lambda (field) (value field "name"))
                      (value detail "output_fields")))
               (error-codes (coerce (value detail "error_codes") 'list)))
          (expect output-fields :to-contain "next_commands")
          (dolist (code '("selection.no-match" "input.not-found" "environment.io"))
            (expect error-codes :to-contain code))
          (multiple-value-bind (app registry) (aitools/cli:build-app)
            (declare (ignore app))
            (expect
             error-codes
             :to-equal
             (remove-duplicates
              (append
               '("argument.invalid" "input.syntax-error"
                 "refusal.target-changed" "environment.busy")
               (loop for schema in
                       (aitools.protocol.application:all-command-schemas registry)
                     unless (string=
                             (aitools.protocol.domain:command-schema-name schema)
                             "batch")
                     append
                       (aitools.protocol.domain:command-schema-error-codes schema)))
              :test #'string= :from-end t)))))))
  (it-property "batch parser preserves generated argv tokens"
      ((token
        (gen-string
         :min-length 0
         :max-length 80
         :alphabet (format nil "aB09\"\\~%~C" #\Tab))))
    (let ((encoded (json-kit:stringify (vector (vector "read" token)))))
      (multiple-value-bind (argvs code message)
          (aitools/cli::%parse-batch-argvs encoded)
        (expect (list code message) :to-equal '(nil nil))
        (expect argvs :to-equal (list (list "read" token)))))))

