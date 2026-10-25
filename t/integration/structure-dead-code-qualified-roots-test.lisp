(in-package #:aitools.integration.structure-test)
(describe "aitools structure test (dead code)"
  (it "distinguishes qualified references, roots, and imports"
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "unused-package-use-fails")
      (expect (not (null records)) :to-equal t)
      (expect violations
              :to-equal
              (list
               (format nil
                       "~A:2: unused :use ~A"
                       "packages/feature/fixture-use-consumer/src/domain/package.lisp"
                       "AITOOLS.FIXTURE.USE-LIBRARY"))))
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "referenced-package-use-passes")
      (expect (not (null records)) :to-equal t)
      (expect violations :to-equal nil)
      (expect
       (not
        (null
         (find-if
          (lambda (definition)
            (and (eq (getf definition :namespace) :function)
                 (string= (getf definition :package)
                          "AITOOLS.FIXTURE.USE-LIBRARY-LIVE")
                 (string= (getf definition :name)
                          "(SETF LIBRARY-SETF-PLACE)")))
          (%liveness-definitions records))))
       :to-equal t))
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "reference-roots")
      (expect (not (null records)) :to-equal t)
      (expect
       violations
       :to-equal
       (sort (list
 (format nil "~A:10: unused definition AITOOLS.FIXTURE.ROOTS:%STRING-KEY-TARGET"
         "packages/feature/fixture-roots/src/domain/calc.lisp")
 (format nil "~A:13: unused definition AITOOLS.FIXTURE.ROOTS:%CALC-FUNCTION-NEIGHBOR"
         "packages/feature/fixture-roots/src/domain/calc.lisp")
 (format nil "~A:17: unused definition AITOOLS.FIXTURE.ROOTS:same-name-target"
         "packages/feature/fixture-roots/src/domain/calc.lisp")
 (format nil
         "~A:3: unused definition AITOOLS.FIXTURE.ROOTS:%CALC-FUNCTION"
         "packages/feature/fixture-roots/src/domain/calc-shadow.lisp")
 (format nil "~A:12: unused definition AITOOLS.FIXTURE.ROOTS:%UNUSED-BUILDER"
         "packages/feature/fixture-roots/src/domain/commands.lisp")
 (format nil "~A:3: unused definition AITOOLS.FIXTURE.ROOTS:%UNUSED-MACRO"
         "packages/feature/fixture-roots/src/domain/definitions.lisp")
 (format nil "~A:6: unused definition AITOOLS.FIXTURE.ROOTS:*UNUSED-VARIABLE*"
         "packages/feature/fixture-roots/src/domain/definitions.lisp")
 (format nil "~A:9: unused definition AITOOLS.FIXTURE.ROOTS:%UNUSED-GENERIC"
         "packages/feature/fixture-roots/src/domain/definitions.lisp")
 (format nil "~A:37: unused definition AITOOLS.FIXTURE.ROOTS:%DOTTED-UNUSED"
         "packages/feature/fixture-roots/src/domain/definitions.lisp")
 (format nil "~A:40: unused definition AITOOLS.FIXTURE.ROOTS:%DOTTED-DESTRUCTURING-LOCAL"
         "packages/feature/fixture-roots/src/domain/definitions.lisp")
 (format nil "~A:43: unused definition AITOOLS.FIXTURE.ROOTS:%UNUSED-NEGATIVE-CLASS"
         "packages/feature/fixture-roots/src/domain/definitions.lisp")
 (format nil "~A:44: unused definition AITOOLS.FIXTURE.ROOTS:%UNUSED-NEGATIVE-CONDITION"
         "packages/feature/fixture-roots/src/domain/definitions.lisp")
 (format nil "~A:53: unused definition AITOOLS.FIXTURE.ROOTS:%UNUSED-GENERATED-MODEL"
         "packages/feature/fixture-roots/src/domain/definitions.lisp")
 (format nil "~A:55: unused definition AITOOLS.FIXTURE.ROOTS:%LOOP-GENERATED-MODEL-P"
         "packages/feature/fixture-roots/src/domain/definitions.lisp")
 (format nil "~A:10: unused definition AITOOLS.FIXTURE.ROOTS:%LOOKUP-TARGET-NEIGHBOR"
         "packages/feature/fixture-roots/src/domain/lookups.lisp")
 (format nil "~A:9: unused definition AITOOLS.FIXTURE.ROOTS:%UNUSED-METHOD-HELPER"
         "packages/feature/fixture-roots/src/domain/methods.lisp")
 (format nil
        "~A:11: unused definition AITOOLS.FIXTURE.ROOTS:USE-ERROR-VALUES"
        "packages/feature/fixture-roots/src/domain/macro-lambda-list-scope.lisp")
 (format nil
        "~A:3: unused definition AITOOLS.FIXTURE.ROOTS:ON-ERROR"
        "packages/feature/fixture-roots/src/domain/macro-lambda-list-scope.lisp")
 "packages/feature/fixture-roots/src/domain/dynamic-find-symbol.lisp:4: unresolved dynamic find-symbol"
 "packages/feature/fixture-roots/src/domain/commands.lisp:27: unresolved command builder"
 "packages/feature/fixture-roots/src/domain/dynamic-find-symbol.lisp:16: unresolved dynamic find-symbol"
 "packages/feature/fixture-roots/src/domain/dynamic-find-symbol.lisp:17: unresolved dynamic find-symbol"
 "packages/feature/fixture-roots/src/domain/dynamic-find-symbol.lisp:25: unused definition AITOOLS.FIXTURE.ROOTS.API:EXPLICIT-NIL-PACKAGE-TARGET"
 "packages/feature/fixture-roots/src/domain/dynamic-find-symbol.lisp:22: unused definition AITOOLS.FIXTURE.ROOTS.API:INACTIVE-BACKQUOTE-TARGET")
       (function string<)))
      (let* ((definitions (%liveness-definitions records))
       (calc-function
         (find-if (lambda (definition)
                    (and (eq (getf definition :namespace) :function)
                         (string= (getf definition :package)
                                  "AITOOLS.FIXTURE.ROOTS")
                         (string= (getf definition :file)
                                  "packages/feature/fixture-roots/src/domain/calc.lisp")
                         (string= (getf definition :name) "%CALC-FUNCTION")))
                  definitions))
       (string-key-target
         (find-if (lambda (definition)
                    (and (eq (getf definition :namespace) :function)
                         (string= (getf definition :package)
                                  "AITOOLS.FIXTURE.ROOTS")
                         (string= (getf definition :file)
                                  "packages/feature/fixture-roots/src/domain/calc.lisp")
                         (string= (getf definition :name) "%STRING-KEY-TARGET")))
                  definitions)))
  (expect (not (null calc-function)) :to-equal t)
  (expect (getf calc-function :root) :to-equal t)
  (expect (not (null string-key-target)) :to-equal t)
  (progn
    (expect (getf string-key-target :root) :to-equal nil)
    (flet ((api-target (name)
             (find-if
              (lambda (definition)
                (and (eq (getf definition :namespace) :function)
                     (string= (getf definition :package)
                              "AITOOLS.FIXTURE.ROOTS.API")
                     (string= (getf definition :file)
                              "packages/feature/fixture-roots/src/domain/dynamic-find-symbol.lisp")
                     (string= (getf definition :name) name)))
              definitions)))
      (let* ((inactive-backquote-target (api-target "INACTIVE-BACKQUOTE-TARGET"))
             (active-unquote-target (api-target "ACTIVE-UNQUOTE-TARGET"))
             (omitted-package-target (api-target "OMITTED-PACKAGE-TARGET"))
             (explicit-nil-package-target (api-target "EXPLICIT-NIL-PACKAGE-TARGET"))
             (references
              (nth-value 0
                         (%liveness-reference-index
                          records (%liveness-all-package-metadata records)))))
        (expect (not (null inactive-backquote-target)) :to-equal t)
        (expect (getf inactive-backquote-target :root) :to-equal nil)
        (expect (not (null active-unquote-target)) :to-equal t)
        (expect (getf active-unquote-target :root) :to-equal nil)
        (expect (gethash (list :production "AITOOLS.FIXTURE.ROOTS.API"
                               :function "ACTIVE-UNQUOTE-TARGET")
                         references)
                :to-equal t)
        (expect (not (null omitted-package-target)) :to-equal t)
        (expect (getf omitted-package-target :root) :to-equal nil)
        (expect (gethash (list :production "AITOOLS.FIXTURE.ROOTS.API"
                               :function "OMITTED-PACKAGE-TARGET")
                         references)
                :to-equal t)
        (expect (not (null explicit-nil-package-target)) :to-equal t)
        (expect (getf explicit-nil-package-target :root) :to-equal nil))))))
    (let* ((records (%liveness-files (repository-root)))
       (definitions (%liveness-definitions records))
       (unified-diff-api
         (find-if (lambda (definition)
                    (and (eq (getf definition :namespace) :function)
                         (string= (getf definition :package) "AITOOLS.KERNEL.DOMAIN")
                         (string= (getf definition :file)
                                  "packages/core/kernel/src/domain/unified-diff.lisp")
                         (string= (getf definition :name) "GENERATE-UNIFIED-DIFF")))
                  definitions))
       (diff-hunks-api
         (find-if (lambda (definition)
                    (and (eq (getf definition :namespace) :function)
                         (string= (getf definition :package) "AITOOLS.KERNEL.DOMAIN")
                         (string= (getf definition :file)
                                  "packages/core/kernel/src/domain/unified-diff.lisp")
                         (string= (getf definition :name) "GENERATE-DIFF-HUNKS")))
                  definitions))
       (generated
         (quote (("packages/feature/edit/src/application/yaml-flows.lisp"
                  "PREPARE-YAML-SET/K")
                 ("packages/feature/edit/src/application/yaml-flows.lisp"
                  "PREPARE-YAML-DELETE/K")
                 ("packages/feature/edit/src/application/yaml-flows.lisp"
                  "PREPARE-YAML-MERGE/K")
                 ("packages/feature/edit/src/application/yaml-flows.lisp"
                  "PREPARE-YAML-PATCH/K")
                 ("packages/feature/edit/src/application/yaml-flows.lisp"
                  "PREPARE-YAML-FMT/K")
                 ("packages/feature/edit/src/application/toml-flows.lisp"
                  "PREPARE-TOML-SET/K")
                 ("packages/feature/edit/src/application/toml-flows.lisp"
                  "PREPARE-TOML-DELETE/K")
                 ("packages/feature/edit/src/application/toml-flows.lisp"
                  "PREPARE-TOML-MERGE/K")
                 ("packages/feature/edit/src/application/toml-flows.lisp"
                  "PREPARE-TOML-PATCH/K")
                 ("packages/feature/edit/src/application/toml-flows.lisp"
                  "PREPARE-TOML-FMT/K")))))
  (dolist (entry generated)
    (expect
     (not
      (null
       (find-if
        (lambda (definition)
          (and (getf definition :root)
               (string= (getf definition :package) "AITOOLS.EDIT.APPLICATION")
               (string= (getf definition :file) (first entry))
               (string= (getf definition :name) (second entry))))
        definitions)))
     :to-equal t))
  (expect (not (null unified-diff-api)) :to-equal t)
  (expect (getf unified-diff-api :root) :to-equal t)
  (expect (not (null diff-hunks-api)) :to-equal t)
  (expect (getf diff-hunks-api :root) :to-equal nil))
    (progn
      (format *error-output* "~&DEAD_CODE_FIXTURE_SPEC_PASS: ~A~%" "distinguishes qualified references, roots, and imports")
      (finish-output *error-output*))))
