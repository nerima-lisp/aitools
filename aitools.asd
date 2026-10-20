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
   (:file "domain/protocol/error-catalog-data")
   (:file "domain/protocol/redaction-patterns-data")
   (:file "domain/protocol/correspondence-table-data")
   (:file "domain/protocol/selector-options-data")
   (:file "domain/process/process-tables-data")
   (:file "domain/workspace/builtin-excludes-data")
   (:file "domain/text/cp932-data")
   (:file "domain/text/euc-jp-data")
   (:file "domain/text/encoding-aliases-data")
   (:file "domain/text/mime-data")
   (:file "domain/text/language-data")
   (:file "domain/search/build-files-data")
   (:file "presentation/search/command-schema-data")
   (:file "presentation/inspect/command-schema-data")
   (:file "presentation/inspect/format-command-schema-data")
   (:file "presentation/inspect/archive-command-schema-data")
   (:file "presentation/journal/command-schema-data")
   (:file "application/edit/command-spec-data")
   (:file "presentation/process/command-schema-data")
   (:file "presentation/vcs/command-schema-data")
   (:file "domain/env/tools-data")
   (:file "domain/util/util-tables-data" :depends-on ("table-macros"))
   (:file "presentation/util/command-schema-data")
   (:file "presentation/env/command-schema-data")
   (:file "presentation/edit/command-schema-data")))

(defsystem "aitools"
  :description "An AI-agent-oriented replacement for cat/grep/sed/find/jq/tar and friends: JSON-only output, crash-safe writes, and a built-in undo history."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.2.0"
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
               "aitools/feature/vcs"
               "aitools/feature/env"
               "aitools/feature/util")
  :pathname "."
  :around-compile (lambda (next)
                    (let ((*package* (or (find-package "AITOOLS") *package*)))
                      (funcall next)))
  :in-order-to ((test-op (test-op "aitools/test"))))

(defsystem "aitools/cli"
  :description "Command-line executable for aitools."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.2.0"
  :homepage "https://github.com/nerima-lisp/aitools"
  :bug-tracker "https://github.com/nerima-lisp/aitools/issues"
  :source-control (:git "https://github.com/nerima-lisp/aitools.git")
  :depends-on ("aitools" "cl-cli")
  :pathname "."
  ;; Each feature context's presentation layer (its cl-cli command
  ;; definitions), in module order, ahead of the CLI's own src/ module.
  :components ((:module "feature-search"
                :pathname "packages/feature/search/src/"
                :components ((:file "presentation/package")
                             (:file "presentation/commands")
                             (:file "presentation/command-registration")))
               (:module "feature-inspect"
                :pathname "packages/feature/inspect/src/"
                :components ((:file "presentation/package")
                             (:file "presentation/registry")
                             (:file "presentation/file-commands")
                             (:file "presentation/json-commands")
                             (:file "presentation/table-commands")
                             (:file "presentation/archive-commands")
                             (:file "presentation/snapshot-commands")))
               (:module "feature-journal"
                :pathname "packages/feature/journal/src/"
                :components ((:file "presentation/package")
                             (:file "presentation/commands")))
               (:module "feature-edit"
                :pathname "packages/feature/edit/src/"
                :components ((:file "presentation/package")
                             (:file "presentation/commands")))
               (:module "feature-process"
                :pathname "packages/feature/process/src/"
                :components ((:file "presentation/package")
                             (:file "presentation/commands")))
               (:module "feature-vcs"
                :pathname "packages/feature/vcs/src/"
                :components ((:file "presentation/package")
                             (:file "presentation/commands")))
               (:module "feature-env"
                :pathname "packages/feature/env/src/"
                :components ((:file "presentation/package")
                             (:file "presentation/env-command-support")
                             (:file "presentation/env-sys-command-builders")
                             (:file "presentation/env-time-command-builders")
                             (:file "presentation/env-commands")))
               (:module "feature-util"
                :pathname "packages/feature/util/src/"
                :components ((:file "presentation/package")
                             (:file "presentation/util-commands")))
               (:module "src"
                :pathname "src/"
                :components ((:file "package")
                             (:file "registry")
                             (:file "workspace-context")
                             (:file "context-registration")
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
(load (merge-pathnames "aitools-test-system.lisp"
                      (make-pathname :name nil :type nil
                                     :defaults *load-truename*)))

