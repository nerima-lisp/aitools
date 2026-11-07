(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      '(:context "workspace"
        :complex-functions
        (
          ("AITOOLS.WORKSPACE.APPLICATION" "RESOLVE-REAL-PATH" 5 "packages/core/workspace/src/application/real-path.lisp:24"
               (("AITOOLS.WORKSPACE.APPLICATION" "packages/core/workspace/src/application/real-path.lisp" "RESOLVE-REAL-PATH")
                ("AITOOLS.WORKSPACE.APPLICATION" "packages/core/workspace/src/application/real-path.lisp" "%MEMOIZE-UNRESOLVABLE-REAL-PATH")))
          ("AITOOLS.WORKSPACE.APPLICATION" "CALL-WITH-WORKSPACE-SCAN/K" 5 "packages/core/workspace/src/application/scan.lisp:175"
               (("AITOOLS.WORKSPACE.APPLICATION" "packages/core/workspace/src/application/scan.lisp" "CALL-WITH-WORKSPACE-SCAN/K")
                ("AITOOLS.WORKSPACE.APPLICATION" "packages/core/workspace/src/application/scan-walk.lisp" "%SCAN-START-ENTRIES")
                ("AITOOLS.WORKSPACE.APPLICATION" "packages/core/workspace/src/application/scan-walk.lisp" "%RUN-SCAN")
                ("AITOOLS.WORKSPACE.APPLICATION" "packages/core/workspace/src/application/scan-walk.lisp" "%SCAN-RUN-STARTS")
                ("AITOOLS.WORKSPACE.APPLICATION" "packages/core/workspace/src/application/scan-walk.lisp" "%SCAN-WALK-ENTRY")
                ("AITOOLS.WORKSPACE.APPLICATION" "packages/core/workspace/src/application/scan-walk.lisp" "%SCAN-WALK-DIRECTORY")
                ("AITOOLS.WORKSPACE.APPLICATION" "packages/core/workspace/src/application/scan-walk.lisp" "%SCAN-OFFER")
                ("AITOOLS.WORKSPACE.APPLICATION" "packages/core/workspace/src/application/scan-walk.lisp" "%SCAN-FLUSH")
                ("AITOOLS.WORKSPACE.APPLICATION" "packages/core/workspace/src/application/scan-walk.lisp" "%SCAN-DELIVER")))
          ("AITOOLS.WORKSPACE.DOMAIN" "IGNORE-LIST-VERDICT" 5 "packages/core/workspace/src/domain/gitignore.lisp:106"
               (("AITOOLS.WORKSPACE.DOMAIN" "packages/core/workspace/src/domain/gitignore.lisp" "IGNORE-LIST-VERDICT")))
          ("AITOOLS.WORKSPACE.DOMAIN" "%DOWILD" 7 "packages/core/workspace/src/domain/wildmatch.lisp:42"
               (("AITOOLS.WORKSPACE.DOMAIN" "packages/core/workspace/src/domain/wildmatch.lisp" "%DOWILD")))
          ("AITOOLS.WORKSPACE.INFRASTRUCTURE" "READ-REGULAR-FILE-OCTETS" 5 "packages/core/workspace/src/infrastructure/host.lisp:48"
               (("AITOOLS.WORKSPACE.INFRASTRUCTURE" "packages/core/workspace/src/infrastructure/host.lisp" "READ-REGULAR-FILE-OCTETS")))
        )
        :generated-exemptions
        nil
))
