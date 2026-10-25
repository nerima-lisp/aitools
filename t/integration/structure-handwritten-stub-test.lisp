(in-package #:aitools.integration.structure-test)

(defun %scan-handwritten-stub-fixture (kind relative &optional
                                                  (allowances *handwritten-stub-allowances*))
  (let ((pathname
          (merge-pathnames
           (format nil "t/integration/fixtures/handwritten-stub-~A/cases.lisp" kind)
           (asdf:system-source-directory "aitools/test"))))
    (with-open-file (input pathname)
      (let ((*read-eval* nil))
        (multiple-value-bind (candidates unresolved)
            (%scan-handwritten-text (read input) relative)
          (%handwritten-results candidates unresolved allowances))))))


(describe "aitools handwritten-stub gate"
  (it "ignores comments, strings, reader comments, and production constructor callbacks"
    (multiple-value-bind (violations unresolved)
        (%scan-handwritten-stub-fixture "pass" "pass/cases.lisp")
      (expect violations :to-equal nil)
      (expect (not (null (some (lambda (entry) (search "dynamic SYMBOL-FUNCTION" entry))
                               unresolved)))
              :to-equal t)))

  (it "detects static fdefinition, symbol-function, and defun replacements"
    (multiple-value-bind (violations unresolved)
        (%scan-handwritten-stub-fixture "fail" "fail/cases.lisp")
      (expect unresolved :to-equal nil)
      (expect violations
              :to-equal
              '("fail/cases.lisp:3: hand-written replacement of AITOOLS.KERNEL.DOMAIN:JSON-MODEL-EQUAL"
                "fail/cases.lisp:4: hand-written replacement of AITOOLS.KERNEL.DOMAIN:JSON-MODEL-POINTER-GET"
                "fail/cases.lisp:5: hand-written replacement of AITOOLS.KERNEL.DOMAIN:HANDWRITTEN-STUB-FIXTURE"
                "fail/cases.lisp:6: hand-written replacement of AITOOLS.KERNEL.DOMAIN:JSON-MODEL-POINTER-PUT"
                "fail/cases.lisp:7: hand-written replacement of AITOOLS.KERNEL.DOMAIN:HANDWRITTEN-CL-DEFUN-FIXTURE"
                "fail/cases.lisp:8: hand-written replacement of AITOOLS.KERNEL.DOMAIN:HANDWRITTEN-AFTER-READER-STRING"))))

  (it "requires a reason for each used allowance and rejects stale entries"
    (multiple-value-bind (violations unresolved)
        (%scan-handwritten-stub-fixture
         "fail" "fail/cases.lisp"
         (list (list "AITOOLS.KERNEL.DOMAIN" "JSON-MODEL-EQUAL" "")))
      (declare (ignore unresolved))
      (expect (not (null (some (lambda (entry) (search "requires a nonempty reason" entry))
                               violations)))
              :to-equal t))
    (multiple-value-bind (violations unresolved)
        (%scan-handwritten-stub-fixture
         "fail" "fail/cases.lisp"
         (list (list "AITOOLS.KERNEL.DOMAIN" "JSON-MODEL-EQUAL" "approved reason")))
      (declare (ignore unresolved))
      (expect (length violations) :to-equal 5)
      (expect (notany (lambda (entry) (search "JSON-MODEL-EQUAL" entry)) violations)
              :to-equal t))
    (multiple-value-bind (violations unresolved)
        (%scan-handwritten-stub-fixture
         "fail" "fail/cases.lisp"
         (list (list "AITOOLS.KERNEL.DOMAIN" "NO-SUCH-SYMBOL" "reason")))
      (declare (ignore unresolved))
      (expect (not (null (some (lambda (entry) (search "stale handwritten-stub allowance" entry))
                               violations)))
              :to-equal t)))

  (it "scans non-fixture test Lisp and reports every dynamic target"
    (multiple-value-bind (violations unresolved scanned-files)
        (scan-handwritten-stub-tree (repository-root))
      (expect (plusp scanned-files) :to-equal t)
      (expect violations :to-equal nil)
      (expect unresolved
              :to-equal
              '("t/perf/search-allocation-timing-runner.lisp:13: dynamic SYMBOL-FUNCTION target AITOOLS.STRUCTURE-TEST.READER::SPAWN-SYMBOL"
                "t/perf/search-allocation-timing-runner.lisp:18: dynamic SYMBOL-FUNCTION target AITOOLS.STRUCTURE-TEST.READER::WAIT-SYMBOL")))))

