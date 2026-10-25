(in-package #:aitools.integration.structure-test)

(describe "aitools structure test (dead code)"
  (it "distinguishes qualified references, roots, and imports"
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "u05-dead-code-use-fails")
      (expect (not (null records)) :to-equal t)
      (expect violations
              :to-equal
              (list
               (format nil
                       "~A:2: unused :use ~A"
                       "packages/feature/fixture-use-consumer/src/domain/package.lisp"
                       "AITOOLS.FIXTURE.USE-LIBRARY"))))
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "u05-dead-code-use-passes")
      (expect (not (null records)) :to-equal t)
      (expect violations :to-equal nil))
    (multiple-value-bind (records violations)
        (dead-code-fixture-scan "u05-dead-code-roots")
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
 "packages/feature/fixture-roots/src/domain/dynamic-find-symbol.lisp:4: unresolved dynamic find-symbol"
 "packages/feature/fixture-roots/src/domain/commands.lisp:27: unresolved command builder"
 "packages/feature/fixture-roots/src/domain/dynamic-find-symbol.lisp:8: unresolved dynamic find-symbol")
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
  (expect (getf string-key-target :root) :to-equal nil)))
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
  (expect (getf diff-hunks-api :root) :to-equal nil))))
