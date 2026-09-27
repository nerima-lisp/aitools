;;;; t/integration/batch-failure-test.lisp
;;;;
;;;; `batch` elements refused or failing before they run, and the
;;;; --atomic tx plumbing: --tx and --lock-timeout passed to each element, a
;;;; tx that cannot begin, and a commit that fails past its commit point.
;;;; Workspace helpers come from batch-test.lisp.
(in-package #:aitools.integration.batch-test)

(defun state-directory ()
  (aitools.store.application:store-state-directory (aitools.store.infrastructure:make-posix-store *root*)))

(describe "aitools batch: elements refused or failing before they run"
  (it-each (((("edit" "a.txt" "--old" "one" "--new" "two" "--tx=tx-x")) ("--atomic")
                      "element 0 has --tx; batch --atomic runs every element in its own tx")
                     ((("util" "calc" "1") ("write" "b.txt" "--stdin")) ()
                      "element 1 reads --stdin, which batch has consumed; pass the input inline (--content, --stdin-data)"))
      "refuses ~S before running any element"
      (argvs flags message)
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (multiple-value-bind (code envelope stream) (apply #'run-batch argvs flags)
        (expect (list code stream) :to-equal '(1 :stderr))
        (expect (value envelope "error" "code") :to-equal "argument.invalid")
        (expect (value envelope "error" "message") :to-equal message)
        (expect (value envelope "error" "repairs" 0 "command") :to-equal "aitools schema batch")
        (expect (value envelope "error" "diagnostics") :to-be nil))
      (expect (text "a.txt") :to-equal (format nil "one~%"))
      (expect (text "b.txt") :to-be :absent)
      (expect (open-tx-count) :to-be 0)))

  (it-each ((("nosuch" "x") "unknown command nosuch" "aitools schema")
                     (("read" "--nope" "a.txt") "Unknown option: --nope" "aitools schema read"))
      "runs the unparsable element ~S and answers with its own usage error"
      (argv element-message repair)
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (multiple-value-bind (code envelope) (run-batch (list argv (list "edit" "a.txt" "--old" "one" "--new" "two")))
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "argument.invalid")
        (expect (value envelope "error" "message")
                :to-equal (format nil "element 0 of 2 (~{~A~^ ~}) failed: ~A" argv element-message))
        (expect (value envelope "error" "repairs" 0 "command") :to-equal repair)
        (expect (statuses (value envelope "error" "diagnostics")) :to-equal '("error" "skipped")))
      (expect (text "a.txt") :to-equal (format nil "one~%"))))

  (it-each (("{\"argv\":[\"read\"]}") ("\"read a.txt\"") ("[[]]") ("[[\"read\",1]]"))
      "refuses the --stdin document ~A, which is not an array of non-empty string arrays"
      (document)
    (with-batch-workspace ()
      (multiple-value-bind (code envelope) (run-aitools '("batch" "--stdin") :stdin document)
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "argument.invalid")
        (expect (value envelope "error" "message")
                :to-equal "--stdin must be a JSON array of argv arrays of strings, such as [[\"read\",\"a.lisp\"]]")
        (expect (value envelope "error" "repairs" 0 "command") :to-equal "aitools schema batch"))))

  (it "refuses a --stdin document larger than its input limit"
    (with-batch-workspace ()
      (multiple-value-bind (code envelope)
          (run-aitools '("batch" "--stdin") :stdin (make-string (1+ (* 16 1024 1024)) :initial-element #\Space))
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "argument.invalid")
        (expect (value envelope "error" "message")
                :to-equal (format nil "--stdin is larger than ~D characters" (* 16 1024 1024)))))))

(describe "aitools batch --atomic: tx plumbing"
  (it "adds --tx before an element's -- so the element still runs in the batch's tx"
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (multiple-value-bind (code envelope)
          (run-batch (list (list "edit" "a.txt" "--old" "one" "--new" "two")
                           (list "read" "--" "a.txt"))
                     "--atomic")
        (expect code :to-be 0)
        (expect (statuses (value envelope "results")) :to-equal '("ok" "ok"))
        ;; The read saw the staged edit, which only the tx holds before commit.
        (expect (coerce (value envelope "results" 1 "lines") 'list) :to-equal '("two")))
      (expect (text "a.txt") :to-equal (format nil "two~%"))))

  (it "passes the global --lock-timeout to every element and into its retry repair"
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (let ((aitools.store.application:*fault-hook*
              (lambda (point &rest details)
                (when (and (eq point :tx-after-index) (eql (second details) 1))
                  (put "a.txt" (format nil "external~%"))))))
        (multiple-value-bind (code envelope)
            (run-aitools '("--lock-timeout" "5s" "batch" "--stdin" "--atomic")
                         :stdin "[[\"edit\",\"a.txt\",\"--old\",\"one\",\"--new\",\"two\"]]")
          (expect code :to-be 2)
          (expect (value envelope "error" "code") :to-equal "refusal.target-changed")
          (expect (value envelope "error" "repairs" 0 "command")
                  :to-equal (format nil "aitools --root ~A --lock-timeout 5s batch --stdin --atomic" *root*))))
      (expect (text "a.txt") :to-equal (format nil "external~%"))
      (expect (open-tx-count) :to-be 0)))

  (it "reports a tx that could not begin and runs no element"
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      ;; A regular file where the tx directory must be created.
      (let ((tx-root (concatenate 'string (state-directory) "/tx")))
        (ensure-directories-exist (sb-ext:parse-native-namestring (concatenate 'string (state-directory) "/")))
        (with-open-file (out (sb-ext:parse-native-namestring tx-root) :direction :output)
          (write-string "x" out))
        (multiple-value-bind (code envelope stream)
            (run-batch (list (list "edit" "a.txt" "--old" "one" "--new" "two")) "--atomic")
          (expect (list code stream) :to-equal '(1 :stderr))
          (expect (value envelope "error" "code") :to-equal "environment.io")
          (let ((message (value envelope "error" "message")))
            (expect (search "batch --atomic could not begin its tx: " message) :to-be 0)
            (expect (search tx-root message) :to-be-truthy))
          (expect (value envelope "error" "diagnostics") :to-be nil)))
      (expect (text "a.txt") :to-equal (format nil "one~%"))
      (expect (probe-file (sb-ext:parse-native-namestring (concatenate 'string (state-directory) "/journal/ops.jsonl")))
              :to-be nil)))

  ;; An I/O failure after the commit point leaves the op committed, so
  ;; `tx abort` has nothing left to abort and the batch must not claim that
  ;; nothing was written.
  (it "does not claim nothing was written when the commit fails past its commit point"
    (with-batch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (let ((fired nil))
        (let ((aitools.store.application:*fault-hook*
                (lambda (point &rest details)
                  (declare (ignore details))
                  (when (and (eq point :after-apply) (not fired))
                    (setf fired t)
                    (error 'aitools.store.application:store-io-error
                           :operation "write" :path (disk "a.txt") :detail "injected")))))
          (multiple-value-bind (code envelope stream)
              (run-batch (list (list "edit" "a.txt" "--old" "one" "--new" "two")) "--atomic")
            (expect fired :to-be t)
            (expect (list code stream) :to-equal '(1 :stderr))
            (let* ((tx (value envelope "error" "diagnostics" 0 "tx"))
                   (message (value envelope "error" "message")))
              (expect (stringp tx) :to-be t)
              (expect (search (format nil "tx ~A could not commit and could not be aborted" tx) message) :to-be 0)
              (expect (search "recovery will complete the committed op on the next command" message) :to-be-truthy)
              (expect (search "nothing was written" message) :to-be nil)
              (expect (value envelope "error" "repairs" 0 "action") :to-equal "inspect-state")
              (expect (value envelope "error" "repairs" 0 "command") :to-equal (format nil "aitools tx status ~A" tx)))
            (expect (value envelope "error" "code") :to-equal "environment.io"))))
      ;; The commit's op is in the journal and its write reached the file.
      (expect (text "a.txt") :to-equal (format nil "two~%"))
      (expect (history-count) :to-be 1)
      (expect (open-tx-count) :to-be 0))))
