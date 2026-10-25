(require :asdf)

(asdf:initialize-source-registry
 `(:source-registry (:tree ,(uiop:getcwd)) :inherit-configuration))

(asdf:load-system "aitools/test")

(in-package #:aitools/test)

(describe "runner timeout fixture"
          (it "exceeds its configured timeout" (sleep 0.25)))

(let ((cl-weave:*test-name-filter* "runner timeout fixture"))
  (handler-case (%call-with-test-runner-reporting
                 (lambda ()
                   (run-tests :timeout-ms 25)))
    (error ()
      (uiop:quit 1))))

(uiop:quit 0)
