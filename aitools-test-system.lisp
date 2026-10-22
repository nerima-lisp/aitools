(in-package #:asdf-user)

(defsystem "aitools/test"
           :description "Test system for aitools."
           :author "takeokunn <bararararatty@gmail.com>"
           :maintainer "takeokunn <bararararatty@gmail.com>"
           :license "MIT"
           :version "0.2.0"
           :homepage "https://github.com/nerima-lisp/aitools"
           :bug-tracker "https://github.com/nerima-lisp/aitools/issues"
           :source-control (:git "https://github.com/nerima-lisp/aitools.git")
           ;; This project uses cl-weave as its only test framework.
           :depends-on ("aitools/test/base" "aitools" "aitools/cli" "cl-weave")
           :pathname "."
           ;; t/package and support lead, context tests follow, then structure, integration, and e2e suites.
           :components ((:module "t"
                                 :pathname "t/"
                                 :components ((:module "structure" :pathname "structure/" :serial t
                                                       :components ((:file "cps-contract-reader") (:file "cps-contract-analysis")
                                                                    (:file "cps-contract-allowlist-slash-k")
                                                                    (:file "cps-contract-allowlist-on-key")
                                                                    (:file "cps-contract-test")))
                                              (:module "integration"
                                                       :pathname "integration/"
                                                       :components ((:file "dispatch-test") (:file "dispatch-format-repairs-test" :depends-on ("dispatch-test")) (:file "dispatch-partial-test" :depends-on ("dispatch-test")) (:file "schema-test" :depends-on ("dispatch-test"))
                                                                    (:file "dispatch-recovery-failure-test") (:file "dispatch-invocation-test")
                                                                    (:file "batch-test")
                                                                    (:file "batch-atomic-test") (:file "batch-failure-test")
                                                                    (:file "cli-schema-drift-test")
                                                                    (:file "org-dispatch-test")
                                                                    (:file "org-read-errors-test" :depends-on ("org-dispatch-test"))
                                                                    (:file "org-read-contract-test" :depends-on ("org-read-errors-test"))
                                                                    (:file "org-read-port-contract-test" :depends-on ("org-read-contract-test"))
                                                                    (:file "skill-contract-test") (:file "runner-failure-report-test") (:file "structure-test")
                                                                    (:file "structure-source-scanner")
                                                                    (:file "store-structure-contract-test")
                                                                    (:file "coverage-baseline-reader") (:file "coverage-baseline-test") (:file "structure-timeout-test") (:file "structure-budget-baseline")
                                                                    (:file "structure-budget-reader") (:file "structure-budget-lines-test") (:file "structure-budget-literals-test")
                                                                    (:file "structure-budget-complexity-test")
                                                                    (:file "structure-budget-complexity-fixture-test"
                                                                           :depends-on ("structure-budget-complexity-test"))))
                                              ;; Keep meta-test last; it checks that every correspondence row has an earlier case.
                                              (:module "e2e"
                                                       :pathname "e2e/"
                                                       :components ((:file "package") (:file "harness")
                                                                    (:file "harness-cases")
                                                                    (:file "correspondence-rows")
                                                                    (:file "read-search-cases") (:file "search-find-cases")
                                                                    (:file "edit-cases") (:file "transform-file-cases")
                                                                    (:file "format-cases")
                                                                    (:file "yaml-toml-read-cases")
                                                                    (:file "archive-failure-cases")
                                                                    (:file "org-cases")
                                                                    (:file "org-read-error-cases")
                                                                    (:file "org-query-cases")
                                                                    (:file "org-export-cases")
                                                                    (:file "process-env-cases")
                                                                    (:file "process-env-host-cases")
                                                                    (:file "process-env-util-cases")
                                                                    (:file "meta-test"))))))
           :perform (test-op (op system) (declare (ignore op system))
                             (unless (uiop:symbol-call :aitools/test :%call-with-test-runner-reporting (lambda () (uiop:symbol-call :aitools/test :run-tests)))
                               (error "aitools self test suite failed."))))
