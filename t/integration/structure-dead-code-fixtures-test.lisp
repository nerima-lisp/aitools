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
      (expect violations :to-equal nil))
    (progn
      (format *error-output* "~&U05_FAIL_FIXTURE_SPEC_PASS: ~A~%" "finds an unused private definition and ignores its string mention")
      (finish-output *error-output*)))

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
                       "PUBLIC-ENTRY")
               (format nil
                       "~A:1: exported symbol ~A:~A is referenced only from tests"
                       "packages/feature/fixture-test-export/src/domain/package.lisp"
                       "AITOOLS.FIXTURE.TEST-EXPORT"
                       "TEST-SETF-PLACE"))))
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "u05-dead-code-test-export-passes")
      (expect (not (null records)) :to-equal t)
      (expect violations :to-equal nil))
    (progn
      (format *error-output* "~&U05_FAIL_FIXTURE_SPEC_PASS: ~A~%" "finds an export used only by tests")
      (finish-output *error-output*)))

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
      (expect violations :to-equal nil)
      (expect
       (not
        (null
         (find-if
          (lambda (definition)
            (and (eq (getf definition :namespace) :function)
                 (string= (getf definition :package)
                          "AITOOLS.FIXTURE.IMPORT-LIBRARY-LIVE")
                 (string= (getf definition :name)
                          "(SETF LIBRARY-SETF-PLACE)")))
          (%liveness-definitions records))))
       :to-equal t))
    (progn
      (format *error-output* "~&U05_FAIL_FIXTURE_SPEC_PASS: ~A~%" "finds unused imports and preserves a used import")
      (finish-output *error-output*)))
  (it "distinguishes SETF function names from ordinary functions"
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "u05-dead-code-setf-name-fails")
      (expect (not (null records)) :to-equal t)
      (expect violations
              :to-equal
              (list
               (format nil
                       "~A:5: unused definition ~A:~A"
                       "packages/feature/fixture-setf-fail/src/domain/package.lisp"
                       "AITOOLS.FIXTURE.SETF-FAIL"
                       "(SETF %DEAD-SETF-PLACE)"))))
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "u05-dead-code-setf-name-passes")
      (expect (not (null records)) :to-equal t)
      (expect violations :to-equal nil)
      (let* ((definitions (%liveness-definitions records))
             (setter
               (find-if
                (lambda (definition)
                  (and (eq (getf definition :namespace) :function)
                       (string= (getf definition :package)
                                "AITOOLS.FIXTURE.SETF-PASS")
                       (string= (getf definition :file)
                                "packages/feature/fixture-setf-pass/src/domain/package.lisp")
                       (string= (getf definition :name)
                                "(SETF %LIVE-SETF-PLACE)")))
                definitions))
             (ordinary
               (find-if
                (lambda (definition)
                  (and (eq (getf definition :namespace) :function)
                       (string= (getf definition :package)
                                "AITOOLS.FIXTURE.SETF-PASS")
                       (string= (getf definition :file)
                                "packages/feature/fixture-setf-pass/src/domain/package.lisp")
                       (string= (getf definition :name)
                                "%LIVE-SETF-PLACE")))
                definitions)))
        (expect (not (null setter)) :to-equal t)
        (expect (not (null ordinary)) :to-equal t)
        (expect (string= (getf setter :name)
                         "(SETF %LIVE-SETF-PLACE)")
                :to-equal t)
        (expect (string= (getf ordinary :name) "%LIVE-SETF-PLACE")
                :to-equal t)))
    (progn
      (format *error-output* "~&U05_FAIL_FIXTURE_SPEC_PASS: ~A~%" "distinguishes SETF function names from ordinary functions")
      (finish-output *error-output*))))
