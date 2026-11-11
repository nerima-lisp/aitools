(in-package #:aitools.util.test)

(describe "aitools util presentation contract"
  (it "passes --stdin into the application input request"
    (multiple-value-bind (app registry) (aitools/cli:build-app)
      (declare (ignore registry))
      (let* ((root (uiop:native-namestring (uiop:getcwd)))
             (stdin-invocation
               (cl-cli:parse-argv app
                 (list "aitools" "--root" root "util" "encode" "base64" "--stdin")))
             (content-invocation
               (cl-cli:parse-argv app
                 (list "aitools" "--root" root "util" "encode" "base64" "--content" "hi")))
             (presentation (find-package "AITOOLS.UTIL.PRESENTATION"))
             (application (find-package "AITOOLS.UTIL.APPLICATION"))
             (request-maker (and presentation (find-symbol "%INPUT-REQUEST" presentation)))
             (stdin-reader (and application (find-symbol "INPUT-REQUEST-STDIN" application))))
        (expect request-maker :to-be-truthy)
        (expect stdin-reader :to-be-truthy)
        (expect (funcall (symbol-function stdin-reader)
                         (funcall (symbol-function request-maker) stdin-invocation))
                :to-be t)
        (expect (funcall (symbol-function stdin-reader)
                         (funcall (symbol-function request-maker) content-invocation))
                :to-be nil))))
  (it "raises when util schema data is missing"
    (let ((helper
            (find-symbol "%SCHEMA-DATA"
                         (find-package "AITOOLS.UTIL.PRESENTATION"))))
      (expect helper :to-be-truthy)
      (expect (handler-case
                  (progn
                    (funcall (symbol-function helper) "missing")
                    :returned)
                (error () :error))
              :to-be :error))))
