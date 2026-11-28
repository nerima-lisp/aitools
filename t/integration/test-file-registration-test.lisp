(in-package #:aitools/test)

(describe "Test file registration"
  (it "requires exactly one test system for each non-excluded file"
    (expect (%test-file-registration-problems nil nil nil) :to-equal nil)
    (expect (%test-file-registration-problems
             '("unit/example.lisp")
             '(("unit/example.lisp" . "aitools/test/example")) nil)
            :to-equal nil)
    (let ((missing (%test-file-registration-problems '("unit/missing.lisp") nil nil))
          (duplicate (%test-file-registration-problems
                      '("unit/example.lisp")
                      '(("unit/example.lisp" . "one") ("unit/example.lisp" . "two"))
                      nil))
          (same-system (%test-file-registration-problems
                        '("unit/example.lisp")
                        '(("unit/example.lisp" . "one") ("unit/example.lisp" . "one"))
                        nil)))
      (expect (length missing) :to-equal 1)
      (expect (search "unit/missing.lisp" (first missing)) :to-be-truthy)
      (expect (length duplicate) :to-equal 1)
      (expect (search "unit/example.lisp" (first duplicate)) :to-be-truthy)
      (expect (length same-system) :to-equal 1)
      (expect (search "unit/example.lisp" (first same-system)) :to-be-truthy)))
  (it "excludes fixture, performance, and dynamically loaded baseline directories"
    (expect (%test-file-registration-problems
             '("e2e/fixtures/example.lisp" "integration/a/fixtures/example.lisp"
               "perf/example.lisp" "integration/structure-budget-baselines/example.lisp"
               "integration/structure-context-allowances/example.lisp"
               "spec-count-baselines/example.lisp")
             nil nil)
            :to-equal nil)
    (expect (length (%test-file-registration-problems
                     '("integration/fixtures-extra/example.lisp") nil nil))
            :to-equal 1))
  (it "matches pidfd files to the same feature condition as their ASD declarations"
    (dolist (path (mapcar #'car *conditional-test-files*))
      (expect (length (%test-file-registration-problems (list path) nil '(:linux :x86-64)))
              :to-equal 1)
      (expect (%test-file-registration-problems (list path) nil '(:darwin :arm64))
              :to-equal nil))))
