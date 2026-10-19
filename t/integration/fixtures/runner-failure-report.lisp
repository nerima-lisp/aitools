(require :asdf)

(asdf:initialize-source-registry
 `(:source-registry (:tree ,(uiop:getcwd)) :inherit-configuration))

(asdf:load-system "aitools/test")

(in-package #:aitools/test)

(describe "runner failure report fixture"
          (it "has a failing assertion" (expect :actual :to-equal :expected))
          (it "signals a condition" (error "fixture condition message"))
          (it "skips with a reason" (cl-weave:skip "fixture skip reason")))

(defun run-runner-failure-report-fixture ()
  (%call-with-test-runner-reporting
   (lambda ()
     (setf *last-test-outcomes* nil)
     (let* ((json (make-string-output-stream))
            (events
              (cl-weave:run nil
                            :name-filter "runner failure report fixture"
                            :reporter :json
                            :stream json)))
       (setf *last-test-outcomes* events)
       (unless (= (length (cl-weave::normalize-run-results events)) 3)
         (error "fixture did not select all three specs"))
       (error "fixture policy failure")))))

(handler-case (run-runner-failure-report-fixture)
  (error () (uiop:quit 1)))

(uiop:quit 0)
