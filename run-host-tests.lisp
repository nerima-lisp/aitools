;;;; Full-suite host gate for the Nix-delivered executable.
(require :asdf)

(let ((events nil))
  (handler-case (progn
                  (asdf:initialize-source-registry
                   `(:source-registry
                     (:directory
                      ,(uiop:pathname-directory-pathname *load-truename*))
                     :inherit-configuration))
                  (asdf:initialize-output-translations
                   `(:output-translations
                     (t
                      (,(uiop:ensure-directory-pathname
                         (uiop:getenv "AITOOLS_HOST_CACHE_DIR")) :**/
                       :*.*.*))
                     :ignore-inherited-configuration))
                  (asdf:load-system "aitools/test")
                  (format t
                          "~&SOURCE=~A~%"
                          (asdf:system-source-directory "aitools"))
                  (format t "BINARY=~A~%" (uiop:getenv "AITOOLS_E2E_BINARY"))
                  (let* ((json (make-string-output-stream))
                         (run-events
                          (uiop:symbol-call :aitools/test
                                            :call-with-spec-progress
                                            (lambda ()
                                              (uiop:symbol-call :cl-weave :run nil :timeout-ms 60000 :reporter
                                                                :json
                                                                :stream
                                                                json
                                                                :exclude-tags
                                                                (when (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")
                                                                  (list :trace-positive))))))
                         (report (get-output-stream-string json))
                         (parsed (uiop:symbol-call :json-kit :parse report))
                         (selected (gethash "events" parsed)))
                    (setf events run-events)
                    (multiple-value-bind (problems executed)
                        (uiop:symbol-call :aitools/test
                                          :%policy-problems
                                          (coerce selected (quote list)))
                      (format t "~&POLICY_EXECUTED=~D~%" executed)
                      (when problems
                        (error "Host full suite policy failed: ~S" problems)))
                    (unless
                        (and (plusp (length selected))
                             (= (length selected) (gethash "passed" parsed))
                             (zerop (gethash "skipped" parsed))
                             (zerop (gethash "todos" parsed))
                             (zerop (gethash "failed" parsed))
                             (zerop (gethash "errored" parsed))
                             (uiop:symbol-call :cl-weave :results-status events))
                      (error
                       "Host full suite failed, skipped tests, or selected no tests."))
                    (uiop:symbol-call :aitools/test
                                      :%report-test-outcomes
                                      events)))
    (error (condition)
      (format t
              "~&Host full suite failed: ~A: ~A~%"
              (type-of condition)
              condition)
      (when
          (and events
               (find-package :aitools/test)
               (fboundp (find-symbol "%REPORT-TEST-OUTCOMES" :aitools/test)))
        (uiop:symbol-call :aitools/test
                          :%report-test-outcomes
                          events
                          *standard-output*))
      (finish-output *standard-output*)
      (uiop:quit 1))))
