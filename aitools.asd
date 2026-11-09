;;;; This form must precede defsystem: ASDF binds *package* to ASDF-USER
;;;; only when it reads this file itself. Other readers use the current
;;;; package, so unqualified defsystem can fail; see docs/src/reference/architecture.md.
(in-package #:asdf-user)
;; Context systems live in their own ASDF files under packages/.  ASDF's
;; source registry does not discover this repository's two-level layout, so
;; the aggregate system registers those definitions before referring to them.
(let ((root (make-pathname :name nil :type nil :defaults *load-truename*)))
  (dolist (pathname (directory (merge-pathnames "packages/*/*/*.asd" root)))
    (load pathname)))

(defsystem "aitools/data"
  :description "Static data tables shared by all library contexts."
  :pathname "data/"
  :components
  ((:file "package")
   (:file "table-macros" :depends-on ("package"))
   (:file "domain/edit/transform-tables-data" :depends-on ("table-macros"))
   (:file "domain/protocol/error-catalog-data")
   (:file "domain/protocol/redaction-patterns-data")
   (:file "domain/protocol/correspondence-table-data")
   (:file "domain/protocol/selector-options-data")
   (:file "domain/process/process-tables-data")
   (:file "domain/workspace/builtin-excludes-data")
   (:file "domain/workspace/git-metadata-data")
   (:file "domain/text/cp932-data")
   (:file "domain/text/euc-jp-data")
   (:file "domain/text/encoding-aliases-data")
   (:file "domain/text/mime-data")
   (:file "domain/text/language-data")
   (:file "domain/search/build-files-data")
   (:file "presentation/org/command-schema-data")
   (:file "presentation/org/write-command-schema-data")
   (:file "presentation/search/command-schema-data")
   (:file "presentation/inspect/command-schema-data")
   (:file "presentation/inspect/format-command-schema-data")
   (:file "presentation/inspect/archive-command-schema-data")
   (:file "presentation/journal/command-schema-data")
   (:file "application/edit/command-spec-data")
   (:file "application/edit/command-option-groups-data" :depends-on ("domain/protocol/selector-options-data"))
   (:file "presentation/process/command-schema-data")
   (:file "domain/env/tools-data")
   (:file "domain/util/util-tables-data" :depends-on ("table-macros"))
   (:file "domain/store/action-names-data")
   (:file "presentation/util/command-schema-data")
   (:file "presentation/env/command-schema-data")
   (:file "presentation/md/command-schema-data")
   (:file "presentation/edit/command-schema-data")
   (:file "presentation/edit/doc-command-schema-data")
   (:file "presentation/net/command-schema-data")))

(defsystem "aitools"
  :description "An AI-agent-oriented replacement for cat/grep/sed/find/jq/tar and friends: JSON-only output, crash-safe writes, and a built-in undo history."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.3.0"
  :homepage "https://github.com/nerima-lisp/aitools"
  :bug-tracker "https://github.com/nerima-lisp/aitools/issues"
  :source-control (:git "https://github.com/nerima-lisp/aitools.git")
  :depends-on ("aitools/data"
               "aitools/core/kernel"
               "aitools/core/protocol"
               "aitools/core/workspace"
               "aitools/core/text"
               "aitools/core/store"
               "aitools/feature/search"
               "aitools/feature/inspect"
               "aitools/feature/journal"
               "aitools/feature/edit"
               "aitools/feature/process"
               "aitools/feature/org"
               "aitools/feature/env"
               "aitools/feature/md"
               "aitools/feature/util"
               "aitools/feature/net")
  :pathname "."
  :around-compile (lambda (next)
                    (let ((*package* (or (find-package "AITOOLS") *package*)))
                      (funcall next)))
  :in-order-to ((test-op (test-op "aitools/test"))))

(defsystem "aitools/cli/foundation"
  :description "Shared CLI package and runtime foundation."
  :depends-on ("aitools" "cl-cli" "cl-observability-kit")
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "trace-context" :depends-on ("package"))
               (:file "trace-store-io" :depends-on ("trace-context"))
               (:file "registry" :depends-on ("package"))
               (:file "workspace-context" :depends-on ("package"))
               (:file "context-registration-definitions" :depends-on ("package"))))
(defsystem "aitools/cli"
  :description "Command-line executable for aitools."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.3.0"
  :homepage "https://github.com/nerima-lisp/aitools"
  :bug-tracker "https://github.com/nerima-lisp/aitools/issues"
  :source-control (:git "https://github.com/nerima-lisp/aitools.git")
  :depends-on ("aitools"
               "aitools/cli/foundation"
               "cl-cli"
               "cl-observability-kit"
               "aitools/feature/search/presentation"
               "aitools/feature/inspect/presentation"
               "aitools/feature/edit/presentation"
               "aitools/feature/journal/presentation"
               "aitools/feature/process/presentation"
               "aitools/feature/org/presentation"
               "aitools/feature/env/presentation"
               "aitools/feature/md/presentation"
               "aitools/feature/util/presentation"
               "aitools/feature/net/presentation")
  :pathname "."
  :components ((:module "src"
                :pathname "src/"
                :components ((:file "context-registration")
                             (:file "schema")
                             (:file "dispatch-names")
                             (:file "dispatch")
                             (:file "batch")
                             (:file "batch-atomic-flow")
                             (:file "batch-flow")
                             (:file "app")
                             (:file "entry-point"))))
  :build-operation "program-op"
  :build-pathname "aitools"
  :entry-point "aitools/cli::image-entry-point")
(load (merge-pathnames "aitools-test-system-base.lisp"
                      (make-pathname :name nil :type nil
                                     :defaults *load-truename*)))
(load (merge-pathnames "aitools-test-system.lisp"
                      (make-pathname :name nil :type nil
                                     :defaults *load-truename*)))

