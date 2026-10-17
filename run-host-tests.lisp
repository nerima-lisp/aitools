;;;; Full-suite host gate for the Nix-delivered executable.
(require :asdf)

(asdf:initialize-source-registry
 `(:source-registry
   (:directory ,(uiop:pathname-directory-pathname *load-truename*))
   :inherit-configuration))
(asdf:initialize-output-translations
 `(:output-translations
   (t (,(uiop:ensure-directory-pathname (uiop:getenv "AITOOLS_HOST_CACHE_DIR")) :**/ :*.*.*))
   :ignore-inherited-configuration))
(asdf:load-system "aitools/test")
(format t "~&SOURCE=~A~%" (asdf:system-source-directory "aitools"))
(format t "BINARY=~A~%" (uiop:getenv "AITOOLS_E2E_BINARY"))
(let* ((json (make-string-output-stream))
       (events (aitools/test::call-with-spec-progress
                (lambda () (cl-weave:run nil :reporter :json :stream json))))
       (report (get-output-stream-string json))
       (parsed (json-kit:parse report))
       (selected (gethash "events" parsed)))
  (write-string report)
  (multiple-value-bind (problems executed)
      (aitools/test::%policy-problems (coerce selected 'list))
    (format t "~&POLICY_EXECUTED=~D~%" executed)
    (when problems
      (error "Host full suite policy failed: ~S" problems)))
  (unless (and (plusp (length selected))
               (= (length selected) (gethash "passed" parsed))
               (zerop (gethash "skipped" parsed))
               (zerop (gethash "todos" parsed))
               (zerop (gethash "failed" parsed))
               (zerop (gethash "errored" parsed))
               (cl-weave:results-status events))
    (error "Host full suite failed, skipped tests, or selected no tests.")))
