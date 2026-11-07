(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      '(:context "vcs"
        :complex-functions
        (
          ("AITOOLS.VCS.APPLICATION" "%SHOW-BLOB" 5 "packages/feature/vcs/src/application/blame-show-flows.lisp:168"
               (("AITOOLS.VCS.APPLICATION" "packages/feature/vcs/src/application/show-flow.lisp" "%SHOW-BLOB")
                ("AITOOLS.VCS.APPLICATION" "packages/feature/vcs/src/application/show-flow.lisp" "%SHOW-TEXT-LINES")
                ("AITOOLS.VCS.APPLICATION" "packages/feature/vcs/src/application/show-flow.lisp" "%TEXT-FIELDS")
                ("AITOOLS.VCS.APPLICATION" "packages/feature/vcs/src/application/flows.lisp" "%FINISH-FIELDS")))
          ("AITOOLS.VCS.DOMAIN" "DIFF-FILES/K" 6 "packages/feature/vcs/src/domain/diff.lisp:72"
               (("AITOOLS.VCS.DOMAIN" "packages/feature/vcs/src/domain/diff.lisp" "DIFF-FILES/K")
                ("AITOOLS.VCS.DOMAIN" "packages/feature/vcs/src/domain/diff.lisp" "%HUNK-FILE-DATA")
                ("AITOOLS.VCS.DOMAIN" "packages/feature/vcs/src/domain/diff.lisp" "%HUNK-FILE-STEP")
                ("AITOOLS.VCS.DOMAIN" "packages/feature/vcs/src/domain/diff.lisp" "%COLLECT-DIFF-HUNKS")))
        )
        :generated-exemptions
        nil
))
