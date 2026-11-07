(in-package #:asdf-user)

;; Keep the shared test list separate to stay under CI's 250-line source limit.
(defsystem "aitools/test/base"
           :description "Shared test components; use aitools/test to run them."
           :author "takeokunn <bararararatty@gmail.com>"
           :maintainer "takeokunn <bararararatty@gmail.com>"
           :license "MIT"
           :version "0.3.0"
           :homepage "https://github.com/nerima-lisp/aitools"
           :bug-tracker "https://github.com/nerima-lisp/aitools/issues"
           :source-control (:git "https://github.com/nerima-lisp/aitools.git")
           :depends-on ("aitools" "aitools/cli" "cl-weave")
           :pathname "."
           :components ((:module "t"
                                 :pathname "t/"
                                 :components ((:file "package")
                                              (:file "spec-count-baselines/loader")
                                              (:module "support"
                                                       :pathname "support/"
                                                       :components ((:file "package")
                                                                    (:file "json-assertions")
                                                                    (:file "files")
                                                                    (:file "tools")
                                                                    (:file "workspace")
                                                                    (:file "cli")
                                                                    (:file "envelope-matchers")
                                                                    (:file "process-test-support")
                                                                    (:file "store-fault-injection")
                                                                    (:file "workspace-integration" :depends-on ("package" "workspace"))))
                                              (:module "e2e" :pathname "e2e/" :serial t
                                               :components ((:file "package")
                                                            (:file "harness")
                                                            (:file "byte-capture")
                                                            (:file "harness-cases")
                                                            (:file "harness-cases-extra" :depends-on ("harness-cases"))
                                                            (:file "trace-harness")
                                                            (:file "correspondence-rows")
                                                            (:file "byte-gate")
                                                            (:file "byte-gate-inventory-reader" :depends-on ("byte-gate"))
                                                            (:file "byte-gate-report" :depends-on ("byte-gate"))
                                                            (:file "byte-gate-compare" :depends-on ("byte-gate" "byte-gate-report"))))))))
