;;;; This form comes FIRST, before any defsystem. ASDF binds *package* to
;;;; ASDF-USER only for a file it loads itself; read any other way -- a REPL
;;;; `load`, an editor evaluating the buffer, flake.nix parsing :version --
;;;; the file is read in whatever package happens to be current, and an
;;;; unqualified `defsystem` then fails to read at all. See
;;;; docs/src/reference/architecture.md for the system layout.
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
   (:file "domain/protocol/error-catalog-data")
   (:file "domain/protocol/redaction-patterns-data")
   (:file "domain/protocol/command-placement-data")
   (:file "domain/protocol/correspondence-table-data")
   (:file "domain/protocol/selector-options-data")
   (:file "domain/workspace/builtin-excludes-data")
   (:file "domain/text/cp932-data")
   (:file "domain/text/euc-jp-data")
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
   (:file "domain/util/util-tables-data")
   (:file "presentation/util/command-schema-data")
   (:file "presentation/env/command-schema-data")
   (:file "presentation/edit/command-schema-data")))

(defsystem "aitools"
  :description "An AI-agent-oriented replacement for cat/grep/sed/find/jq/tar and friends: JSON-only output, crash-safe writes, and a built-in undo history."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.2"
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
  :version "0.1.2"
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
                             (:file "presentation/commands")))
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
                             (:file "dispatch")
                             (:file "batch")
                             (:file "app")
                             (:file "entry-point"))))
  :build-operation "program-op"
  :build-pathname "aitools"
  :entry-point "aitools/cli::image-entry-point")

(defsystem "aitools/test"
  :description "Test system for aitools."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.2"
  :homepage "https://github.com/nerima-lisp/aitools"
  :bug-tracker "https://github.com/nerima-lisp/aitools/issues"
  :source-control (:git "https://github.com/nerima-lisp/aitools.git")
  ;; cl-weave is the org's only test framework. Do not
  ;; introduce FiveAM, parachute, rove, or prove.
  :depends-on ("aitools" "aitools/cli" "cl-weave")
  :pathname "."
  ;; t/package and t/support come first, then every context's :TESTS file in
  ;; module order, then the cross-context integration and e2e modules last.
  :components ((:module "t"
                :pathname "t/"
                :components ((:file "package")
                             (:module "support"
                              :pathname "support/"
                              :components ((:file "package")
                                           (:file "json-assertions")
                                           (:file "files")
                                           (:file "tools")
                                           (:file "workspace")
                                           (:file "cli")
                                           (:file "envelope-matchers")))
                             (:file "unit/kernel/package")
                             (:file "unit/kernel/path-test")
                             (:file "unit/kernel/selector-test")
                             (:file "unit/kernel/guard-test")
                             (:file "unit/kernel/duration-test")
                             (:file "unit/kernel/size-test")
                             (:file "unit/kernel/digest-test")
                             (:file "unit/kernel/token-estimate-test")
                             (:file "unit/kernel/unified-diff-test")
                             (:file "unit/kernel/json-test")
                             (:file "unit/protocol/package")
                             (:file "unit/protocol/envelope-test")
                             (:file "unit/protocol/error-catalog-test")
                             (:file "unit/protocol/redaction-test")
                             (:file "unit/protocol/command-placement-test")
                             (:file "unit/protocol/redaction-flow-test")
                             (:file "unit/protocol/schema-flow-test")
                             (:file "unit/protocol/command-result-test")
                             (:file "unit/protocol/json-writer-test")
                             (:file "unit/workspace/package")
                             (:file "unit/workspace/fake-host")
                             (:file "unit/workspace/wildmatch-test")
                             (:file "unit/workspace/gitignore-test")
                             (:file "unit/workspace/root-boundary-test")
                             (:file "unit/workspace/git-data-test")
                             (:file "unit/workspace/scan-test")
                             (:file "integration/workspace-test-support")
                             (:file "integration/workspace-gitignore-parity-test")
                             (:file "integration/workspace-host-test")
                             (:file "unit/text/package")
                             (:file "unit/text/layout-test")
                             (:file "unit/text/charset-test")
                             (:file "unit/text/guess-test")
                             (:file "unit/text/codec-test")
                             (:file "unit/text/archive-test")
                             (:file "unit/text/source-test")
                             (:file "integration/text-archive-fixtures")
                             (:file "integration/text-archive-interop-test")
                             (:file "integration/text-host-source-test")
                             (:file "support/store-fault-injection")
                             (:file "unit/store/package")
                             (:file "unit/store/domain-test")
                             (:file "integration/store-write-protocol-test")
                             (:file "integration/store-recovery-test")
                             (:file "integration/store-lock-test")
                             (:file "integration/store-tx-test")
                             (:file "integration/store-mtime-test")
                             (:file "integration/store-security-test")
                             (:file "unit/search/package")
                             (:file "unit/search/fakes")
                             (:file "unit/search/matcher-test")
                             (:file "unit/search/search-flow-test")
                             (:file "unit/search/search-flow-options-test")
                             (:file "unit/search/find-flow-test")
                             (:file "unit/search/code-flow-test")
                             (:file "unit/search/commands-test")
                             (:file "perf/search-allocation-test")
                             (:file "integration/search-host-test")
                             (:file "unit/inspect/package")
                             (:file "unit/inspect/support")
                             (:file "unit/inspect/read-test")
                             (:file "unit/inspect/json-query-test")
                             (:file "unit/inspect/table-test")
                             (:file "unit/inspect/table-edge-test")
                             (:file "unit/inspect/archive-test")
                             (:file "unit/inspect/snapshot-test")
                             (:file "integration/inspect-snapshot-test")
                             (:file "unit/inspect/info-check-diff-test")
                             (:file "unit/inspect/selection-test")
                             (:file "integration/inspect-cli-test")
                             (:file "integration/inspect-cli-commands-test")
                             (:file "integration/inspect-cli-store-test")
                             (:file "unit/journal/package")
                             (:file "unit/journal/domain-test")
                             (:file "unit/journal/changes-json-test")
                             (:file "integration/journal-flows-test")
                             (:file "integration/journal-tx-test")
                             (:file "integration/journal-tx-failure-test")
                             (:file "integration/journal-cli-test")
                             (:file "unit/edit/package")
                             (:file "unit/edit/domain-test")
                             (:file "unit/edit/domain-edge-test")
                             (:file "unit/edit/format-test")
                             (:file "unit/edit/format-edge-test")
                             (:file "integration/edit-text-test")
                             (:file "integration/edit-text-pipeline-test")
                             (:file "integration/edit-text-ports-test")
                             (:file "integration/edit-text-commands-test")
                             (:file "integration/edit-text-commands-inputs-test")
                             (:file "integration/edit-text-commands-paths-test")
                             (:file "integration/edit-files-test")
                             (:file "integration/edit-files-refusals-test")
                             (:file "integration/edit-tx-and-limits-test")
                             (:file "integration/edit-cli-test")
                             (:file "integration/edit-security-test")
                             (:file "unit/process/package")
                             (:file "unit/process/fakes")
                             (:file "unit/process/domain-test")
                             (:file "unit/process/domain-records-test")
                             (:file "unit/process/flows-test")
                             (:file "unit/process/flows-bg-control-test")
                             (:file "integration/process-test")
                             (:file "integration/process-bg-test")
                             (:file "integration/process-ports-test")
                             (:file "integration/process-ports-launch-test")
                             (:file "unit/vcs/package")
                             (:file "unit/vcs/domain-test")
                             (:file "unit/vcs/flows-test")
                             (:file "unit/vcs/flows-options-test")
                             (:file "integration/vcs-git-test")
                             (:file "unit/env/package")
                             (:file "unit/env/tzif-fixtures")
                             (:file "unit/env/support")
                             (:file "unit/env/civil-time-test")
                             (:file "unit/env/time-input-test")
                             (:file "unit/env/tzif-test")
                             (:file "unit/env/host-parsers-test")
                             (:file "unit/env/time-flows-test")
                             (:file "unit/env/sys-flows-test")
                             (:file "unit/env/env-commands-test")
                             (:file "integration/env-host-test")
                             (:file "unit/util/package")
                             (:file "unit/util/codec-test")
                             (:file "unit/util/calc-test")
                             (:file "unit/util/uuid-random-test")
                             (:file "unit/util/flows-test")
                             (:file "integration/util-cli-test")
                             (:module "integration"
                              :pathname "integration/"
                              :components ((:file "dispatch-test")
                                           (:file "dispatch-recovery-failure-test")
                                           (:file "dispatch-invocation-test")
                                           (:file "batch-test") (:file "batch-failure-test")
                                           (:file "cli-schema-drift-test") (:file "skill-contract-test")
                                           (:file "structure-test")))
                             ;; meta-test last: cl-weave runs specs in definition
                             ;; order, and it checks that every correspondence-table
                             ;; row registered a case above it.
                             (:module "e2e"
                              :pathname "e2e/"
                              :components ((:file "package") (:file "harness")
                                           (:file "harness-cases")
                                           (:file "correspondence-rows")
                                           (:file "read-search-cases") (:file "search-find-cases")
                                           (:file "edit-cases") (:file "transform-file-cases")
                                           (:file "format-cases") (:file "process-env-cases")
                                           (:file "meta-test"))))))
  :perform (test-op (op system)
             (declare (ignore op system))
             (unless (uiop:symbol-call :aitools/test :run-tests)
               (error "aitools self test suite failed."))))
