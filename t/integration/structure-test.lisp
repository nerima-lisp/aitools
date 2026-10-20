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
(defparameter *feature-contexts* '("search" "inspect" "edit" "journal" "process" "vcs" "env" "util"))
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

(defparameter *context-symbol-allowances*
  ;; (context layers package symbol reason): a pure shared helper used by
  ;; only the named feature adapters.
  '(("edit" (:infrastructure) "AITOOLS.KERNEL.DOMAIN" "UNIVERSAL-TIME-TO-UNIX-SECONDS"
     "pure Unix-time conversion for the edit adapter")
    ("search" (:infrastructure) "AITOOLS.KERNEL.DOMAIN" "UNIVERSAL-TIME-TO-UNIX-SECONDS"
     "pure Unix-time conversion for the search adapter")))

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
                '("CL-REGEX-KIT" "JSON-KIT" "CL-CODEC-KIT" "CRYPTO-KIT" "CL-DATE-KIT")))
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
