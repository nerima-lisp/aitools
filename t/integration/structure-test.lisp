;;;; t/integration/structure-test.lisp
;;;;
;;;; The dependency-direction check (docs/src/reference/architecture.md):
;;;; scans every context's source for the
;;;; packages it depends on and fails on one the layer table does not allow.
;;;; A dependency is a package-qualified symbol in code, or a package
;;;; designator in a DEFPACKAGE's :USE, :IMPORT-FROM, :SHADOWING-IMPORT-FROM
;;;; or :LOCAL-NICKNAMES clause. Each layer has an allow-list: a package the
;;;; list does not name is a violation, so a new kit or host package has to be
;;;; added here with its reason rather than slipping through unnoticed.
;;;; Qualified symbols are found by a text scan after comments, strings and
;;;; character literals are blanked (see %STRIP-PROSE), so prose that merely
;;;; mentions a package is not counted; DEFPACKAGE and IN-PACKAGE forms are
;;;; read with the Lisp reader.
(in-package #:cl-user)

(defpackage #:aitools.integration.structure-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect))

(in-package #:aitools.integration.structure-test)

(defparameter *core-contexts* '("kernel" "protocol" "workspace" "text" "store"))
(defparameter *feature-contexts* '("search" "inspect" "edit" "journal" "process" "vcs" "org" "env" "util"))
(defparameter *all-contexts* (append *core-contexts* *feature-contexts*))

(defun context-package-name (context layer)
  (format nil "AITOOLS.~:@(~A~).~:@(~A~)" context layer))

(defparameter *always-allowed-packages*
  ;; AITOOLS.DATA holds every context's table data (data is kept separate
  ;; from logic); it is loaded before any library file and
  ;; contains no logic, so each layer may read its own tables from it.
  '("CL" "COMMON-LISP" "KEYWORD" "AITOOLS.DATA"))

(defparameter *effectful-host-packages*
  ;; An adapter's own host interface: SBCL's POSIX, alien, socket and
  ;; system packages and UIOP are what the effectful kits are built on, and
  ;; infrastructure is the layer that owns such effects.
  ;; SB-GRAY adapts caller-owned input streams without closing them; SB-UNIX
  ;; polls their descriptors so the process feeder can observe cancellation.
  ;; These host effects remain confined to infrastructure.
  '("HOST-KIT" "PROCESS-KIT" "VCS-KIT" "CL-CONCURRENT-KIT" "CL-BOUNDARY-KIT"
    "SB-POSIX" "SB-ALIEN" "SB-BSD-SOCKETS" "SB-EXT" "SB-INT" "SB-SYS"
    "SB-GRAY" "SB-UNIX" "UIOP"))

(defparameter *symbol-allowances*
  ;; (layers package symbol reason): single SBCL symbols a pure layer may
  ;; use although their package is not on its list.
  '(((:domain :application) "SB-EXT" "STRING-TO-OCTETS"
     "pure UTF-8 encoding; no portable equivalent in CL")
    ((:domain :application) "SB-EXT" "OCTETS-TO-STRING"
     "pure UTF-8 decoding; no portable equivalent in CL")
    ((:application) "SB-INT" "CHARACTER-DECODING-ERROR"
     "the condition OCTETS-TO-STRING signals on invalid UTF-8")
    ((:domain) "SB-UNICODE" "NORMALIZE-STRING"
     "pure Unicode normalization")))

(defvar *context-symbol-allowances*)

(defun allowed-package-names (context layer core-or-feature)
  "The packages a file at CONTEXT/LAYER may depend on, per the layer table in
docs/src/reference/architecture.md. CORE-OR-FEATURE is :CORE or :FEATURE, the file's own placement
under packages/core/ or packages/feature/ (a fixture's made-up context name
is in neither context list)."
  (let ((own (context-package-name context layer))
        (core-domains (mapcar (lambda (c) (context-package-name c :domain)) *core-contexts*)))
    (append
     *always-allowed-packages*
     (list own)
     (ecase layer
       (:domain
        (append core-domains
                ;; Parser APIs are pure and are used only by inspect's read adapters.
                (and (string= context "inspect") '("YAML-KIT" "CL-TOML-KIT"))
                '("CL-REGEX-KIT" "JSON-KIT"
                  "CL-CODEC-KIT" "CRYPTO-KIT" "CL-DATE-KIT" "DEFLATE-KIT")))
       (:application
        ;; A core context's application may reach only core contexts
        ;; ("core 文脈から feature 文脈への参照" is its own violation); a
        ;; feature context's may reach every context's application.
        (append (list (context-package-name context :domain))
                core-domains
                (mapcar (lambda (c) (context-package-name c :application))
                        (if (eq core-or-feature :core) *core-contexts* *all-contexts*))))
       (:infrastructure
        (append (list (context-package-name context :domain) (context-package-name context :application))
                *effectful-host-packages*
                ;; Documented deviation: the envelope writer serializes with
                ;; json-kit (packages/core/protocol/src/infrastructure/json-writer.lisp).
                (and (string= own "AITOOLS.PROTOCOL.INFRASTRUCTURE") '("JSON-KIT"))))
       (:presentation
        (list (context-package-name context :application)
              "AITOOLS.PROTOCOL.DOMAIN" "AITOOLS.PROTOCOL.APPLICATION" "CL-CLI"))))))

(defun symbol-allowed-p (context layer package symbol)
  (or (find-if (lambda (entry)
                 (destructuring-bind (layers entry-package entry-symbol reason) entry
                   (declare (ignore reason))
                   (and (member layer layers) (string= package entry-package)
                        (string= symbol entry-symbol))))
               *symbol-allowances*)
      (find-if (lambda (entry)
                 (destructuring-bind (entry-context layers entry-package entry-symbol reason) entry
                   (declare (ignore reason))
                   (and (string= context entry-context) (member layer layers)
                        (string= package entry-package) (string= symbol entry-symbol))))
               *context-symbol-allowances*)))

(defun %aitools-context (package-name)
  "The context segment of an AITOOLS.<CONTEXT>.<LAYER> package name, or NIL."
  (let ((parts (uiop:split-string package-name :separator ".")))
    (and (= (length parts) 3) (string= (first parts) "AITOOLS") (second parts))))

;;; ------------------------------------------------------- text scanning

;;; --------------------------------------------------------------- tests

(defun repository-root ()
  (asdf:system-source-directory "aitools"))

(defun fixture-violations (name)
  (scan-source-tree (merge-pathnames (format nil "t/integration/fixtures/~A/" name)
                                     (asdf:system-source-directory "aitools/test"))))

(describe "aitools structure test (dependency direction)"
  (it "finds no layer violations in the real source tree"
    (expect (scan-source-tree (repository-root)) :to-equal nil))

  (it "allows process stream adapters only in infrastructure"
    (let* ((relative "packages/feature/process/src/infrastructure/runner.lisp")
           (pathname (merge-pathnames relative (repository-root))))
      (expect (scan-file pathname relative "process" :infrastructure :feature nil)
              :to-equal nil)
      ;; Reclassify real effectful code: unrelated placement errors must not
      ;; substitute for the two host-package violations this policy requires.
      (dolist (layer '(:domain :application))
        (let ((violations (scan-file pathname relative "process" layer :feature nil)))
          (dolist (reference '("SB-GRAY:FUNDAMENTAL-CHARACTER-INPUT-STREAM"
                               "SB-UNIX:UNIX-SIMPLE-POLL"))
            (let ((expected (format nil "~A, which process/~(~A~) may not depend on"
                                    reference layer)))
              (expect (not (null (some (lambda (violation) (search expected violation))
                                      violations)))
                      :to-equal t)))))))

  (it "fails on a domain file that reaches an effectful kit"
    (expect (fixture-violations "violation-effectful-kit-from-domain")
            :to-equal '("packages/core/fixture/src/domain/bad.lisp:5: HOST-KIT:READ-FILE, which fixture/domain may not depend on")))

  (it "fails on a core file that reaches into a feature context"
    (expect (fixture-violations "violation-core-reaches-feature")
            :to-equal '("packages/core/fixture/src/application/bad.lisp:6: AITOOLS.SEARCH.APPLICATION:DO-SEARCH, which fixture/application may not depend on")))

  (it "fails on an application file that reaches another context's domain directly"
    (expect (fixture-violations "violation-application-reaches-domain")
            :to-equal '("packages/feature/fixture-feature/src/application/bad.lisp:7: AITOOLS.SEARCH.DOMAIN:SOMETHING, which fixture-feature/application may not depend on")))

  (it "fails on a package the layer's allow-list does not name"
    (expect (fixture-violations "violation-unknown-package")
            :to-equal '("packages/core/fixture/src/domain/bad.lisp:5: UIOP:READ-FILE-STRING, which fixture/domain may not depend on")))

  (it "fails on defpackage :use, :import-from, :shadowing-import-from and :local-nicknames designators"
    (expect (fixture-violations "violation-defpackage-clauses")
            :to-equal '("packages/core/fixture/src/domain/bad.lisp:10: HOST-KIT:READ-FILE, which fixture/domain may not depend on"
                        "packages/core/fixture/src/domain/package.lisp:3: :import-from PROCESS-KIT:RUN, which fixture/domain may not depend on"
                        "packages/core/fixture/src/domain/package.lisp:3: :local-nicknames HOST-KIT, which fixture/domain may not depend on"
                        "packages/core/fixture/src/domain/package.lisp:3: :shadowing-import-from VCS-KIT:STATUS, which fixture/domain may not depend on"
                        "packages/core/fixture/src/domain/package.lisp:3: :use CL-CLI, which fixture/domain may not depend on")))

  (it "fails on another context's internal symbol even where its package is allowed"
    (expect (fixture-violations "violation-internal-symbol")
            :to-equal '("packages/feature/fixture-feature/src/application/bad.lisp:5: AITOOLS.KERNEL.DOMAIN::HELPER is an internal symbol of another context")))

  (it "fails on an in-package that does not match the file's directory"
    (expect (fixture-violations "violation-in-package")
            :to-equal '("packages/core/fixture/src/domain/bad.lisp:3: in-package AITOOLS.FIXTURE.APPLICATION, but a file in fixture/domain must be in AITOOLS.FIXTURE.DOMAIN")))

  (it "allows JSON-KIT in protocol infrastructure only"
    (expect (fixture-violations "violation-json-kit-infrastructure")
            :to-equal '("packages/core/store/src/infrastructure/bad.lisp:5: JSON-KIT:PARSE, which store/infrastructure may not depend on")))

  (it "passes on a domain file using only what its layer allows"
    (expect (fixture-violations "clean-domain") :to-equal nil)))

(defun dead-code-fixture-scan (name)
  (let* ((root (merge-pathnames
                (format nil "t/integration/fixtures/~A/" name)
                (repository-root)))
         (records (%liveness-files root)))
    (values records (scan-dead-code-tree root))))

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
      (expect violations :to-equal nil)))

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
  (expect (getf diff-hunks-api :root) :to-equal nil)))
  (it "keeps reader-resolved roots in their source namespaces"
      (let* ((records (%liveness-files
                       (merge-pathnames "t/integration/fixtures/u05-dead-code-roots/"
                                        (repository-root))))
             (definitions (%liveness-definitions records))
             (packages (%liveness-all-package-metadata records)))
        (multiple-value-bind (references local-references diagnostics)
            (%liveness-reference-index records packages)
          (declare (ignore diagnostics))
          (labels ((definition (package name)
                               (find-if (lambda (entry)
                                          (and (eq (getf entry :namespace) :function)
                                               (string= (getf entry :package) package)
                                               (string= (getf entry :name) name)))
                                        definitions))
                   (reference-p (package namespace symbol)
                                (gethash (list :production package namespace symbol) references)))
            (progn
              (dolist (package (list "AITOOLS.FIXTURE.ROOTS" "AITOOLS.FIXTURE.OTHER"))
                (let ((target (definition package "duplicate-target")))
                  (expect (not (null target)) :to-equal t)
                  (expect (getf target :root) :to-equal t)))
              (let ((root-target (definition "AITOOLS.FIXTURE.ROOTS" "same-name-target"))
                    (other-target (definition "AITOOLS.FIXTURE.OTHER" "same-name-target")))
                (expect (not (null root-target)) :to-equal t)
                (expect (getf root-target :root) :to-equal nil)
                (expect (not (null other-target)) :to-equal t)
                (expect (getf other-target :root) :to-equal nil)
                (expect (reference-p "AITOOLS.FIXTURE.ROOTS" :function "same-name-target")
                        :to-equal nil)
                (expect (reference-p "AITOOLS.FIXTURE.OTHER" :function "same-name-target")
                        :to-equal t)
                (expect (getf (gethash "AITOOLS.FIXTURE.OTHER" packages) :uses)
                        :to-equal nil)
                (expect (getf (gethash "AITOOLS.FIXTURE.OTHER" packages) :uses-declared-p)
                        :to-equal nil)
                (expect (getf (gethash "AITOOLS.FIXTURE.DEFAULT-USE" packages) :uses)
                        :to-equal nil)
                (expect (getf (gethash "AITOOLS.FIXTURE.DEFAULT-USE" packages) :uses-declared-p)
                        :to-equal nil)
                (expect (getf (gethash "AITOOLS.FIXTURE.EMPTY-USE" packages) :uses)
                        :to-equal nil)
                (expect (getf (gethash "AITOOLS.FIXTURE.EMPTY-USE" packages) :uses-declared-p)
                        :to-equal t))
              (dolist (name (list "%COMMAND-BUILDER" "%INSPECT-BUILDER"))
                (let ((builder (definition "AITOOLS.FIXTURE.ROOTS" name)))
                  (expect (not (null builder)) :to-equal t)
                  (expect (getf builder :root) :to-equal nil)
                  (expect (and (reference-p "AITOOLS.FIXTURE.ROOTS" :function name)
                               (reference-p "AITOOLS.FIXTURE.ROOTS" :function "%QUASIQUOTE-REFERENCE-TARGET"))
                          :to-equal t)))
              (let ((entry-point (definition "AITOOLS/CLI" "IMAGE-ENTRY-POINT")))
                (expect (not (null entry-point)) :to-equal t)
                (expect (getf entry-point :root) :to-equal nil)
                (expect (reference-p "AITOOLS/CLI" :function "IMAGE-ENTRY-POINT") :to-equal t)
                (flet ((references-for-source (source-file)
                         (let ((selected-records
                                 (remove-if-not
                                  (lambda (record)
                                    (member (getf record :relative)
                                            (list
                                             "packages/feature/fixture-roots/src/domain/other-package.lisp"
                                             source-file)
                                            :test #'string=))
                                  records)))
                           (multiple-value-bind
                                 (selected-references local-references diagnostics)
                               (%liveness-reference-index selected-records packages)
                             (declare (ignore local-references diagnostics))
                             selected-references))))
                  (let ((default-references
                          (references-for-source
                           "packages/feature/fixture-roots/src/domain/default-use.lisp"))
                        (empty-references
                          (references-for-source
                           "packages/feature/fixture-roots/src/domain/empty-use.lisp")))
                    (dolist (symbol (quote ("CAR" "FIND-SYMBOL")))
                      (expect (gethash (list :production "COMMON-LISP" :function symbol)
                                       default-references)
                              :to-equal t)
                      (expect (gethash (list :production "AITOOLS.FIXTURE.DEFAULT-USE"
                                             :function symbol)
                                       default-references)
                              :to-equal nil)
                      (expect (gethash (list :production "AITOOLS.FIXTURE.EMPTY-USE"
                                             :function symbol)
                                       empty-references)
                              :to-equal t)
                      (expect (gethash (list :production "COMMON-LISP" :function symbol)
                                       empty-references)
                              :to-equal nil))))))
            (dolist (entry
                     '(("AITOOLS.FIXTURE.ROOTS" :function "duplicate-target")
                       ("AITOOLS.FIXTURE.ROOTS" :function "FIND-SYMBOL")
                       ("AITOOLS.FIXTURE.OTHER" :function "FIND-SYMBOL")
                       ("AITOOLS.FIXTURE.RELAY" :function "RELAY-TARGET")
                       ("AITOOLS.FIXTURE.OTHER" :function "RELAY-TARGET")
                       ("AITOOLS.FIXTURE.OTHER" :function "duplicate-target")
                       ("AITOOLS.FIXTURE.OTHER" :function "CAR")
                       ("COMMON-LISP" :function "CAR")
                       ("COMMON-LISP" :function "FIND-SYMBOL")
                       ("AITOOLS.FIXTURE.EMPTY-USE" :function "CAR")
                       ("AITOOLS.FIXTURE.EMPTY-USE" :function "FIND-SYMBOL")
                       ("AITOOLS.FIXTURE.ROOTS" :function "%LIVE-GENERIC")
                       ("AITOOLS.FIXTURE.ROOTS" :function "%LIVE-MACRO")
                       ("AITOOLS.FIXTURE.ROOTS" :value "*LIVE-VARIABLE*")
                       ("AITOOLS.FIXTURE.ROOTS" :type "%LIVE-CLASS")
                       ("AITOOLS.FIXTURE.ROOTS" :type "%UNUSED-CLASS")
                       ("AITOOLS.FIXTURE.ROOTS" :type "%SECONDARY-CLASS-PARENT")
                       ("AITOOLS.FIXTURE.ROOTS" :type "%DECLARATION-TYPE")
                       ("AITOOLS.FIXTURE.ROOTS" :type "%THE-ONLY-TYPE")
                       ("AITOOLS.FIXTURE.ROOTS" :type "%DECLAIM-ONLY-TYPE")
                       ("AITOOLS.FIXTURE.ROOTS" :type "%CHECK-TYPE-ONLY-TYPE")
                       ("AITOOLS.FIXTURE.ROOTS" :type "%LIVE-CONDITION")
                       ("AITOOLS.FIXTURE.ROOTS" :type "%UNUSED-CONDITION")
                       ("AITOOLS.FIXTURE.ROOTS" :type "%HANDLER-ONLY-CONDITION")
                       ("AITOOLS.FIXTURE.ROOTS.API" :function "EXTERNAL-GENERIC")
                       ("AITOOLS.FIXTURE.ROOTS" :function "%LOOP-BODY-HELPER")
                       ("AITOOLS.FIXTURE.ROOTS" :function "%CONSTRUCTOR-DEFAULT-FALLBACK")
                       ("AITOOLS.FIXTURE.ROOTS" :function "%MAKE-CONSTRUCTOR-DEFAULT-HOLDER")
                       ("AITOOLS.FIXTURE.ROOTS" :function "%CONSTRUCTOR-DEFAULT-HOLDER-VALUE")
                       ("AITOOLS.FIXTURE.ROOTS" :value "*FIXTURE-LOCK*")
                       ("AITOOLS/CLI" :function "IMAGE-ENTRY-POINT")))
              (destructuring-bind (package namespace symbol) entry
                (expect (reference-p package namespace symbol) :to-equal t)))
            (dolist (entry
                     (quote (("AITOOLS.FIXTURE.ROOTS" :type "%UNUSED-NEGATIVE-CLASS")
 ("AITOOLS.FIXTURE.ROOTS" :type "%UNUSED-NEGATIVE-CONDITION")
 ("AITOOLS.FIXTURE.ROOTS" :function "%LOOP-GENERATED-MODEL-P"))))
              (destructuring-bind (package namespace symbol) entry
                (expect (reference-p package namespace symbol) :to-equal nil)))))))
  (it "reports no production dead-code violations"
  (let* ((root (merge-pathnames "t/integration/fixtures/u05-dead-code-roots/"
                                (repository-root)))
         (records (%liveness-files root))
         (packages (%liveness-all-package-metadata records)))
    (multiple-value-bind (references local-references diagnostics)
        (%liveness-reference-index records packages)
      (declare (ignore local-references diagnostics))
      (dolist (symbol (quote ("%MAKE-LOOP-GENERATED-MODEL"
                              "%LOOP-GENERATED-MODEL-VALUE")))
        (expect (gethash (list :production "AITOOLS.FIXTURE.ROOTS" :function symbol)
                         references)
                :to-equal t))))
  (expect (scan-dead-code-tree (repository-root)) :to-equal nil)))
