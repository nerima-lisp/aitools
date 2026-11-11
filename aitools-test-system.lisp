(in-package #:asdf-user)

(progn
  (defun %registered-aitools-test-systems ()
    (let ((prefix "aitools/test/"))
      (sort
       (remove-if-not
        (lambda (name)
          (and (stringp name)
               (> (length name) (length prefix))
               (string= prefix name :end2 (length prefix))))
        (asdf:registered-systems))
       #'string<)))
  (defun %load-aitools-test-systems (&optional (context (uiop:getenv "AITOOLS_TEST_CONTEXT")))
    (let* ((registered (%registered-aitools-test-systems))
           (shared "aitools/test/shared")
           (contexts (remove-if (lambda (name)
                                  (member name
                                          (list "aitools/test/base" "aitools/test/shared")
                                          :test (function string=)))
                                registered)))
      (when (and (uiop:getenv "AITOOLS_COVERAGE") context)
        (error "AITOOLS_TEST_CONTEXT cannot be combined with AITOOLS_COVERAGE."))
      (unless (member shared registered :test (function string=))
        (error "No registered shared test system ~S." shared))
      (if context
          (let ((selected (format nil "aitools/test/~A" context)))
            (unless (member selected contexts :test (function string=))
              (error "No registered test system for context ~S." context))
            (asdf:load-system selected))
        (progn
          (unless contexts
            (error "No context test systems are registered."))
          (dolist (system contexts)
            (asdf:load-system system))
          (asdf:load-system shared)))))
  (defsystem "aitools/test/shared"
             :description "Shared, structural, integration, and end-to-end tests."
             :depends-on ("aitools/test/base" "aitools" "aitools/cli" "cl-weave")
             :pathname "."
             :components ((:module "t" :pathname "t/" :components
                                   ((:file "unit/trace-context-test")
                                    (:file "unit/trace-store-io-test" :depends-on ("unit/trace-context-test"))
                                    (:module "structure" :pathname "structure/" :serial t
                                             :components ((:file "cps-contract-reader")
                                                          (:file "cps-contract-analysis")
                                                          (:file "cps-contract-analysis-positional")
                                                          (:file "cps-contract-analysis-positional-flow")
                                                          (:file
                                                           "cps-contract-analysis-positional-flow-rules")
                                                          (:file
                                                           "cps-contract-analysis-positional-flow-walker")
                                                          (:file
                                                           "cps-contract-analysis-positional-flow-z-tail")
                                                          (:file "cps-contract-analysis-source")
                                                          (:file "cps-contract-analysis-positional-inventory")
                                                          (:file "cps-contract-test")
                                                          (:file "cps-contract-positional-test")
                                                          (:file "cps-contract-z-positional-terminal-test")))
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
                                                          (:file "batch-atomic-test")
                                                          (:file "batch-failure-test")
                                                          (:file "cli-schema-drift-test")
                                                          (:file "skill-contract-test")
                                                          (:file "runner-failure-report-test")
                                                          (:file "structure-test")
                                                          (:file "structure-context-allowances" :depends-on ("structure-test"))
                                                          ;; Keep dynamically loaded allowances in filtered source trees.
                                                          (:module "structure-context-data"
                                                           :pathname "structure-context-allowances/"
                                                           :depends-on ("structure-context-allowances")
                                                           :serial t
                                                           :components
                                                           ((:file "edit")
                                                            (:file "org")
                                                            (:file "md")
                                                            (:file "store")
                                                            (:file "text")))
                                                          (:file "structure-source-scanner-reader" :depends-on ("structure-context-allowances"))
                                                          (:file "structure-source-scanner-reader-cache" :depends-on ("structure-source-scanner-reader"))
                                                          (:file "structure-dead-code-source" :depends-on ("structure-source-scanner" "structure-source-scanner-reader"))
                                                          (:file "structure-dead-code-definitions" :depends-on ("structure-dead-code-source" "structure-source-scanner"))
                                                          (:file "structure-dead-code-symbols" :depends-on ("structure-dead-code-source" "structure-source-scanner"))
                                                          (:file "structure-dead-code-reference-core" :depends-on ("structure-dead-code-symbols" "structure-source-scanner-reader" "structure-source-scanner"))
                                                          (:file "structure-dead-code-reference-bindings" :depends-on ("structure-dead-code-reference-core"))
                                                          (:file "structure-dead-code-reference-definitions" :depends-on ("structure-dead-code-reference-core"))
                                                          (:file "structure-dead-code-reference-forms" :depends-on ("structure-dead-code-reference-core" "structure-dead-code-reference-bindings" "structure-dead-code-reference-definitions" "structure-dead-code-symbols"))
                                                          (:file "structure-dead-code-reference-index" :depends-on ("structure-dead-code-reference-diagnostics" "structure-dead-code-reference-core" "structure-dead-code-reference-forms" "structure-dead-code-definitions" "structure-dead-code-symbols"))
                                                          (:file "structure-dead-code-reference-diagnostics" :depends-on ("structure-dead-code-reference-forms" "structure-source-scanner-reader" "structure-source-scanner"))
                                                          (:file "structure-dead-code-scan" :depends-on ("structure-dead-code-reference-diagnostics" "structure-dead-code-reference-index" "structure-dead-code-definitions" "structure-dead-code-source" "structure-source-scanner"))
                                                          (:file "structure-source-scanner" :depends-on ("structure-context-allowances" "structure-source-scanner-reader"))
                                                          (:file "structure-dead-code-fixtures-test" :depends-on ("structure-dead-code-scan"))
                                                          (:file "structure-dead-code-qualified-roots-test" :depends-on ("structure-dead-code-scan"))
                                                          (:file "structure-dead-code-reader-roots-test" :depends-on ("structure-dead-code-scan"))
                                                          (:file "structure-dead-code-production-scan-test" :depends-on ("structure-dead-code-scan"))
                                                          (:file "store-structure-contract-test" :depends-on ("structure-context-allowances"))
                                                          (:file "coverage-baseline-reader")
                                                          (:file "coverage-exclusion-runner")
                                                          (:file "coverage-gap-report" :depends-on ("coverage-exclusion-runner"))
                                                          (:file "coverage-baseline-test" :depends-on ("coverage-exclusion-runner" "coverage-gap-report"))
                                                          (:file "coverage-gap-report-test" :depends-on ("coverage-gap-report"))
                                                          (:file "structure-timeout-test")
                                                          (:file "coverage-artifact-validation")
                                                          (:file "run-policy-and-coverage-artifact-validation-test"
                                                                 :depends-on ("coverage-artifact-validation"))
                                                          (:file "structure-budget-baseline")
                                                          ;; Keep dynamically loaded per-context budgets in filtered source trees.
                                                          (:module "structure-budget-baseline-data"
                                                           :pathname "structure-budget-baselines/"
                                                           :depends-on ("structure-budget-baseline")
                                                           :serial t
                                                           :components
                                                           ((:file "edit")
                                                            (:file "env")
                                                            (:file "md")
                                                            (:file "inspect")
                                                            (:file "process")
                                                            (:file "store")
                                                            (:file "text")
                                                            (:file "workspace")))
                                                          (:file "structure-budget-reader")
                                                          (:file "structure-budget-lines-test")
                                                          (:file "structure-budget-literals-test")
                                                          (:file "structure-budget-complexity-test")
                                                          (:file "structure-budget-complex-function-test"
                                                                 :depends-on ("structure-budget-complexity-test"))
                                                          (:file "structure-budget-complexity-fixture-test"
                                                                 :depends-on ("structure-budget-complexity-test"))
                                                          (:file "context-registration-test")
                                                          (:file "composition-coverage-test")))
                                    ;; Keep the correspondence meta-test last so its cases see earlier rows.
                                    (:module "e2e" :pathname "e2e/" :serial t
                                             :components ((:file "process-env-host-cases")
                                                         (:file "trace-byte-parity-cases")
                                                         (:file "byte-gate-test")
                                                         (:file "byte-gate-inventory-test" :depends-on ("byte-gate-test"))
                                                         (:file "net-cases") (:file "meta-test"))))))))
(defsystem "aitools/test"
           :description "Run shared tests or dynamically discovered context tests."
           :depends-on ("aitools/test/base")
           :pathname "."
           :perform (test-op (op system)
                             (declare (ignore op system))
                             (%load-aitools-test-systems)
                             (unless (uiop:symbol-call
                                      :aitools/test :%call-with-test-runner-reporting
                                      (lambda () (uiop:symbol-call :aitools/test :run-tests)))
                               (error "aitools self test suite failed."))))
