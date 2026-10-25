(in-package #:aitools.integration.structure-test)

(describe "aitools structure test (dead code)"
  (it "finds an unused private definition and ignores its string mention"
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "u05-dead-code-definition-fails")
      (expect (not (null records)) :to-equal t)
      (expect violations
              :to-equal
              (list
               (format nil
                       "~A:3: unused definition ~A:~A"
                       "packages/feature/fixture-dead/src/domain/package.lisp"
                       "AITOOLS.FIXTURE.DEAD"
                       "DEAD-DEFINITION"))))
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "u05-dead-code-definition-passes")
      (expect (not (null records)) :to-equal t)
      (expect violations :to-equal nil)))

  (it "finds an export used only by tests"
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "u05-dead-code-test-export-fails")
      (expect (not (null records)) :to-equal t)
      (expect violations
              :to-equal
              (list
               (format nil
                       "~A:1: exported symbol ~A:~A is referenced only from tests"
                       "packages/feature/fixture-test-export/src/domain/package.lisp"
                       "AITOOLS.FIXTURE.TEST-EXPORT"
                       "PUBLIC-ENTRY"))))
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "u05-dead-code-test-export-passes")
      (expect (not (null records)) :to-equal t)
      (expect violations :to-equal nil)))

  (it "finds unused imports and preserves a used import"
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "u05-dead-code-import-fails")
      (expect (not (null records)) :to-equal t)
      (expect violations
              :to-equal
              (list
               (format nil "~A:3: unused :shadowing-import-from ~A:~A"
                       "packages/feature/fixture-import-consumer/src/domain/package.lisp"
                       "AITOOLS.FIXTURE.IMPORT-LIBRARY"
                       "LIBRARY-SHADOWED-CALL")
               (format nil "~A:6: unused :import-from ~A:~A"
                       "packages/feature/fixture-import-consumer/src/domain/package.lisp"
                       "AITOOLS.FIXTURE.IMPORT-LIBRARY"
                       "LIBRARY-CALL"))))
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "u05-dead-code-import-passes")
      (expect (not (null records)) :to-equal t)
      (expect violations :to-equal nil))))
