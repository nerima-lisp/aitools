(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      '(:context "edit"
        :complex-functions
        (
          ("AITOOLS.EDIT.APPLICATION" "PARSE-RECORDED-ARGV" 5 "packages/feature/edit/src/application/command-spec.lisp:65"
               (("AITOOLS.EDIT.APPLICATION" "packages/feature/edit/src/application/command-spec.lisp" "PARSE-RECORDED-ARGV")
                ("AITOOLS.EDIT.APPLICATION" "packages/feature/edit/src/application/command-spec.lisp" "%PARSE-RECORDED-OPTION")))
          ("AITOOLS.EDIT.APPLICATION" "MKTEMP-FLOW" 5 "packages/feature/edit/src/application/mktemp.lisp:35"
               (("AITOOLS.EDIT.APPLICATION" "packages/feature/edit/src/application/mktemp.lisp" "MKTEMP-FLOW")
                ("AITOOLS.EDIT.APPLICATION" "packages/feature/edit/src/application/mktemp.lisp" "%MKTEMP-RESULT")))
          ("AITOOLS.EDIT.DOMAIN" "PARSE-MTIME" 6 "packages/feature/edit/src/domain/regex.lisp:151"
               (("AITOOLS.EDIT.DOMAIN" "packages/feature/edit/src/domain/mtime.lisp" "PARSE-MTIME")
                ("AITOOLS.EDIT.DOMAIN" "packages/feature/edit/src/domain/mtime.lisp" "%MTIME-ZONE-OFFSET-SECONDS")
                ("AITOOLS.EDIT.DOMAIN" "packages/feature/edit/src/domain/mtime.lisp" "%PARSE-ISO-MTIME-MATCH")))
          ("AITOOLS.EDIT.DOMAIN" "REPLACE-DOCUMENT" 5 "packages/feature/edit/src/domain/regex.lisp:105"
               (("AITOOLS.EDIT.DOMAIN" "packages/feature/edit/src/domain/regex.lisp" "REPLACE-DOCUMENT")
                ("AITOOLS.EDIT.DOMAIN" "packages/feature/edit/src/domain/regex.lisp" "%REPLACE-DOCUMENT-MULTILINE")
                ("AITOOLS.EDIT.DOMAIN" "packages/feature/edit/src/domain/regex.lisp" "%REPLACE-DOCUMENT-LINES")))
          ("AITOOLS.EDIT.DOMAIN" "%SCAN-RECORDS" 5 "packages/feature/edit/src/domain/table.lisp:9"
               (("AITOOLS.EDIT.DOMAIN" "packages/feature/edit/src/domain/table.lisp" "%SCAN-RECORDS")
                ("AITOOLS.EDIT.DOMAIN" "packages/feature/edit/src/domain/table.lisp" "%QUOTED-FIELD-END")
                ("AITOOLS.EDIT.DOMAIN" "packages/feature/edit/src/domain/table.lisp" "%FIELD-END")))
        )
        :generated-exemptions
        nil
))
