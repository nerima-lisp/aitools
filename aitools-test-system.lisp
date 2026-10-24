(in-package #:asdf-user)

(defsystem "aitools/test"
           :description "Full suite: shared, structural, integration, and end-to-end tests."
           :depends-on ("aitools/test/base" "aitools" "aitools/cli" "cl-weave")
           :pathname "."
           :components ((:module "t"
                                 :pathname "t/"
                                 :components ((:module "structure" :pathname "structure/" :serial t
                                                       :components ((:file "cps-contract-reader")
                                                                    (:file "cps-contract-analysis")
                                                                    (:file "cps-contract-analysis-positional")
                                                                    (:file "cps-contract-analysis-positional-flow")
                                                                    (:file "cps-contract-analysis-source")
                                                                    (:file "cps-contract-analysis-positional-inventory")
                                                                    (:file "cps-contract-allowlist-slash-k")
                                                                    (:file "cps-contract-allowlist-on-key")
                                                                    (:file "cps-contract-test")
                                                                    (:file "cps-contract-positional-test")))
                                              (:module "integration"
                                                       :pathname "integration/"
                                                       :components ((:file "dispatch-test")
                                                                    (:file "dispatch-format-repairs-test"
                                                                           :depends-on ("dispatch-test"))
                                                                    (:file "dispatch-partial-test"
                                                                           :depends-on ("dispatch-test"))
                                                                    (:file "schema-test"
                                                                           :depends-on ("dispatch-test"))
                                                                    (:file "dispatch-recovery-failure-test")
                                                                    (:file "dispatch-invocation-test")
                                                                    (:file "batch-test")
                                                                    (:file "batch-trace-test")
                                                                    (:file "env-host-trace-test")
                                                                    (:file "process-ports-launch-trace-test")
                                                                    (:file "batch-atomic-test")
                                                                    (:file "batch-failure-test")
                                                                    (:file "cli-schema-drift-test")
                                                                    (:file "org-dispatch-test")
                                                                    (:file "org-read-errors-test" :depends-on ("org-dispatch-test"))
                                                                    (:file "org-read-contract-test" :depends-on ("org-read-errors-test"))
                                                                    (:file "org-read-port-contract-test" :depends-on ("org-read-contract-test"))
                                                                    (:file "skill-contract-test")
                                                                    (:file "runner-failure-report-test")
                                                                    (:file "structure-test")
                                                                    (:file "structure-source-scanner")
                                                                    (:file "store-structure-contract-test")
                                                                    (:file "coverage-baseline-reader")
                                                                    (:file "coverage-baseline-test")
                                                                    (:file "structure-timeout-test")
                                                                    (:file "u11-sec7-gates")
                                                                    (:file "u11-sec7-gates-test" :depends-on ("u11-sec7-gates"))
                                                                    (:file "structure-budget-baseline")
                                                                    (:file "structure-budget-reader")
                                                                    (:file "structure-budget-lines-test")
                                                                    (:file "structure-budget-literals-test")
                                                                    (:file "structure-budget-complexity-test")
                                                                    (:file "structure-budget-complexity-fixture-test"
                                                                           :depends-on ("structure-budget-complexity-test"))))
                                              ;; Keep the correspondence meta-test last so its cases see earlier rows.
                                              (:module "e2e"
                                                       :pathname "e2e/"
                                                       :components ((:file "package")
                                                                    (:file "harness")
                                                                    (:file "byte-capture")
                                                                    (:file "harness-cases")
                                                                    (:file "trace-harness")
                                                                    (:file "correspondence-rows")
                                                                    (:file "read-search-cases")
                                                                    (:file "search-find-cases")
                                                                    (:file "edit-cases")
                                                                    (:file "edit-envelope-compatibility")
                                                                    (:file "transform-file-cases")
                                                                    (:file "file-operation-cases")
                                                                    (:file "format-cases")
                                                                    (:file "yaml-toml-read-cases")
                                                                    (:file "yaml-toml-write-cases")
                                                                    (:file "yaml-toml-write-errors")
                                                                    (:file "yaml-toml-write-format-cases")
                                                                    (:file "archive-failure-cases")
                                                                    (:file "org-cases")
                                                                    (:file "org-read-error-cases")
                                                                    (:file "org-query-cases")
                                                                    (:file "org-export-cases")
                                                                    (:file "process-env-cases")
                                                                    (:file "process-env-host-cases")
                                                                    (:file "process-env-util-cases")
                                                                    (:file "store-trace-cases")
                                                                    (:file "byte-gate")
                                                                    (:file "byte-gate-inventory-reader" :depends-on ("byte-gate"))
                                                                    (:file "byte-gate-report" :depends-on ("byte-gate"))
                                                                    (:file "byte-gate-compare" :depends-on ("byte-gate" "byte-gate-report"))
                                                                    (:file "byte-gate-test" :depends-on ("byte-gate-compare" "byte-gate-inventory-reader"))
                                                                    (:file "byte-gate-inventory-test" :depends-on ("byte-gate-test"))
                                                                    (:file "meta-test"))))))
           :perform (test-op (op system)
                             (declare (ignore op system))
                             (unless (uiop:symbol-call
                                      :aitools/test :%call-with-test-runner-reporting
                                      (lambda () (uiop:symbol-call :aitools/test :run-tests)))
                               (error "aitools self test suite failed."))))
(defsystem "aitools/test/u11-sec7-gates"
  :description "Focused section 7 gate tests."
  :depends-on ("aitools" "cl-weave")
  :pathname "."
  :components ((:module "t"
                        :pathname "t/"
                        :components ((:file "package")
                                     (:module "integration"
                                              :pathname "integration/"
                                              :components ((:file "u11-sec7-gates")
                                                           (:file "u11-sec7-gates-test"
                                                                  :depends-on ("u11-sec7-gates")))))))
  :perform (test-op (op system)
    (declare (ignore op system))
    (let* ((json (make-string-output-stream))
           (events (uiop:symbol-call :cl-weave :run
                                     nil :reporter :json :stream json))
           (parsed (uiop:symbol-call :json-kit :parse
                                     (get-output-stream-string json)))
           (selected (gethash "events" parsed)))
      (uiop:symbol-call :cl-weave :explain! events)
      (unless (and (uiop:symbol-call :cl-weave :results-status events)
                   selected
                   (plusp (length selected)))
        (error "U-11 focused gate specs failed or none were selected."))
      (format t "~&U-11 focused gate target: ~D specs selected~%"
              (length selected)))))
