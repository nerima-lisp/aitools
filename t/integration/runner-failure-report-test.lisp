(in-package #:aitools/test)

(progn
  (defun %run-runner-failure-fixture ()
    (multiple-value-bind (status stdout stderr)
        (aitools.workspace.integration-support:run-command
         (namestring (asdf:system-source-directory "aitools"))
         "sbcl"
         (list "--noinform" "--no-sysinit" "--no-userinit" "--script"
               "t/integration/fixtures/runner-failure-report.lisp"))
      (values status
              (sb-ext:octets-to-string stdout :external-format :utf-8)
              stderr)))
  (defun %run-host-runner-error ()
    (multiple-value-bind (status stdout stderr)
        (aitools.workspace.integration-support:run-command
         (namestring (asdf:system-source-directory "aitools"))
         "sbcl"
         (list "--noinform" "--no-sysinit" "--no-userinit" "--load"
               "run-host-tests.lisp")
         :environment (list (cons "AITOOLS_HOST_CACHE_DIR" "/dev/null")))
      (values status
              (sb-ext:octets-to-string stdout :external-format :utf-8)
              stderr))))

(describe "runner failure report"
          (it "prints failure, error, and skip details after a runner error"
              (multiple-value-bind (status stdout stderr)
                  (%run-runner-failure-fixture)
                (declare (ignore stderr))
                (expect status :to-be 1)
                (expect stdout
                        :to-contain
                        "fixture runner failed: SIMPLE-ERROR: fixture policy failure")
                (expect stdout
                        :to-contain
                        "failed, errored, or skipped specs (3)")
                (expect stdout
                        :to-contain
                        "[fail] runner failure report fixture > has a failing assertion")
                (expect stdout
                        :to-contain
                        "[error] runner failure report fixture > signals a condition")
                (expect stdout
                        :to-contain
                        "SIMPLE-ERROR: fixture condition message")
                (expect stdout
                        :to-contain
                        "[skip] runner failure report fixture > skips with a reason")
                (expect stdout :to-contain "reason: fixture skip reason")
                (let ((runner-error
                        (search "fixture runner failed: SIMPLE-ERROR: fixture policy failure"
                                stdout))
                      (summary
                        (search "failed, errored, or skipped specs (3)" stdout))
                      (skip-reason (search "reason: fixture skip reason" stdout)))
                  (expect (and runner-error summary skip-reason
                               (< runner-error summary)
                               (< summary skip-reason))
                          :to-be t))))
          (it "summarizes failures from JSON coverage events"
              (let* ((condition-event (make-hash-table :test (function equal)))
                     (skip-event (make-hash-table :test (function equal)))
                     (stream (make-string-output-stream)))
                (setf (gethash "status" condition-event) "error"
                      (gethash "pathString" condition-event) "coverage > failing setup"
                      (gethash "condition" condition-event)
                      "SIMPLE-ERROR: coverage fixture condition"
                      (gethash "status" skip-event) "skip"
                      (gethash "pathString" skip-event) "coverage > skipped setup"
                      (gethash "reason" skip-event) "coverage fixture skip reason")
                (%report-test-runner-outcomes
                 (cons :json (list condition-event skip-event)) stream)
                (let ((output (get-output-stream-string stream)))
                  (expect output :to-contain "failed, errored, or skipped specs (2)")
                  (expect output
                          :to-contain
                          "[error] coverage > failing setup condition SIMPLE-ERROR: coverage fixture condition")
                  (expect output
                          :to-contain
                          "[skip] coverage > skipped setup reason: coverage fixture skip reason"))))
          (it "prints typed errors from host runner load failures"
              (multiple-value-bind (status stdout stderr)
                  (%run-host-runner-error)
                (declare (ignore stderr))
                (expect status :to-be 1)
                (let* ((prefix "Host full suite failed: ")
                       (start (search prefix stdout))
                       (type-end (and start
                                      (position #\: stdout
                                                :start (+ start (length prefix)))))
                       (typed-message
                         (and type-end
                              (> type-end (+ start (length prefix)))
                              (search "/dev/" stdout :start2 type-end))))
                  (expect (not (null typed-message)) :to-be t)))))
