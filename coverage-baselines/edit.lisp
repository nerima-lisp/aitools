(:context :edit
          :state
          :measured
          :source
          :linux-ci
          :expression-covered
          13997
          :expression-total
          13997
          :branch-covered
          1500
          :branch-total
          1500
          :coverage-exclusions
          ((:kind :expression
                  :package
                  "AITOOLS.EDIT.APPLICATION"
                  :symbol
                  "RESOLVE-EXTRA-PATH/K"
                  :reason
                  "SB-COVER does not record DEFINE-FLOW keyword defaults; the edit boundary spec exercises the default :BASE :ROOT path and omits :FOLLOW.")
           (:kind :expression :package "AITOOLS.EDIT.APPLICATION" :symbol "WRITE-TARGET" :reason "SB-COVER does not record DEFSTRUCT keyword defaults; pipeline tests assert the default :FOLLOW T and :BASE :CWD values.")
           (:kind :expression :package "AITOOLS.EDIT.APPLICATION" :symbol "WRITE-PLAN" :reason "SB-COVER does not record generated DEFSTRUCT slot defaults; pipeline tests assert the default plan slots used by existing edit commands.")
           (:kind :expression
                  :package
                  "AITOOLS.EDIT.APPLICATION"
                  :symbol
                  "WRITE-CONTEXT"
                  :reason
                  "SB-COVER does not record generated DEFSTRUCT slot defaults; pipeline tests assert the empty :PATHS and :HASH-PATHS values used by existing edit commands.")
           (:kind :expression
                  :package
                  "AITOOLS.EDIT.APPLICATION"
                  :symbol
                  "%APPLY-PLAN"
                  :reason
                  "SB-COVER reports the :PATH keyword binding in the FLOW-CALL error continuation as an uncovered expression; the plan error spec asserts the missing-source refusal.")
           (:kind :expression
                  :package
                  "AITOOLS.EDIT.APPLICATION"
                  :symbol
                  "%EDIT-PLAN"
                  :reason
                  "SB-COVER reports the :PATH keyword binding in the FLOW-CALL error continuation as an uncovered expression; the plan error spec asserts the missing-source refusal.")
           (:kind :expression
                  :package
                  "AITOOLS.EDIT.APPLICATION"
                  :symbol
                  "%INSERT-PLAN"
                  :reason
                  "SB-COVER reports the :PATH keyword binding in the FLOW-CALL error continuation as an uncovered expression; the plan error spec asserts the missing-source refusal.")
           (:kind :expression
                  :package
                  "AITOOLS.EDIT.APPLICATION"
                  :symbol
                  "%JSON-PLAN"
                  :reason
                  "SB-COVER leaves the :PATH keyword binding in the explicit reader error callback lambda list unmarked; JSON repair specs assert the error and path-specific repair.")
           (:kind :expression
                  :package
                  "AITOOLS.EDIT.APPLICATION"
                  :symbol
                  "%TRANSFORM-PLAN-FROM-DOCUMENT/K"
                  :reason
                  "SB-COVER reports the candidate keyword binding in a FLOW-CALL continuation as an uncovered expression; transform specs assert candidate-bearing rejection behavior.")
           (:kind :expression
                  :package
                  "AITOOLS.EDIT.APPLICATION"
                  :symbol
                  "%RUN-TRANSFORM-PLAN/K"
                  :reason
                  "SB-COVER reports the candidate and diagnostic keyword bindings in FLOW-CALL continuations as uncovered expressions; transform specs assert both rejection forms.")
           (:kind :expression
                  :package
                  "AITOOLS.EDIT.APPLICATION"
                  :symbol
                  "%MOVE-LINES-PLAN"
                  :reason
                  "SB-COVER reports the :PATH keyword bindings in source and destination FLOW-CALL error continuations as uncovered expressions; plan specs assert missing-source and invalid-UTF-8 refusals.")
           (:kind :expression
                  :package
                  "AITOOLS.EDIT.APPLICATION"
                  :symbol
                  "%REPLACE-PLAN"
                  :reason
                  "SB-COVER reports the :PATH keyword binding in the FLOW-CALL error continuation as an uncovered expression; the plan error spec asserts the missing-path refusal.")
           (:kind :expression
                  :package
                  "AITOOLS.EDIT.APPLICATION"
                  :symbol
                  "%READ-FORMAT/K"
                  :reason
                  "SB-COVER reports the :PATH keyword binding in the FLOW-CALL error continuation as an uncovered expression; the format-read spec asserts the missing-file error.")
           (:kind :expression :package "AITOOLS.EDIT.APPLICATION" :symbol "*PREPARERS*" :reason "SB-COVER does not record the one-time top-level DEFVAR initializer; command-input specs exercise the initialized preparer registry.")
           (:kind :branch :package "AITOOLS.EDIT.APPLICATION" :symbol "WRITE-PLAN" :reason "SB-COVER does not attribute generated DEFSTRUCT slot type guards; command specs exercise valid and invalid precommit-check values.")
           (:kind :expression :package "AITOOLS.EDIT.DOMAIN" :symbol "JSON-ADD" :reason "SB-COVER does not attribute the array-mode keyword initializer; JSON specs omit :ARRAY-MODE and assert middle insertion and append behavior.")
           (:kind :expression :package "AITOOLS.EDIT.DOMAIN" :symbol "SIMILAR-WINDOWS" :reason "SB-COVER does not attribute the limit keyword initializer; similarity specs omit :LIMIT and assert the default three-candidate window.")
           (:kind :expression :package "AITOOLS.EDIT.DOMAIN" :symbol "TRANSFORM-LINES" :reason "SB-COVER does not attribute the columns keyword initializer; transform specs omit :COLUMNS and assert wrapping at the default 80-column width.")
           (:kind :branch :package "AITOOLS.EDIT.DOMAIN" :symbol "EXTRACT-STEP" :reason "SB-COVER does not attribute generated DEFSTRUCT slot type guards; archive-path specs exercise rejection of an invalid kind.")
           (:kind :branch :package "AITOOLS.EDIT.DOMAIN" :symbol "REPLACER" :reason "SB-COVER does not attribute generated DEFSTRUCT slot type guards; regex specs exercise rejection of an invalid seen count.")
           (:kind :branch :package "AITOOLS.EDIT.DOMAIN" :symbol "SPLIT-PIECE" :reason "SB-COVER does not attribute generated DEFSTRUCT slot type guards; split specs exercise invalid start and end values.")
           (:kind :expression :package "AITOOLS.EDIT.DOMAIN" :symbol "EDIT-JSON-NUMBER" :reason "SBCL 2.6.8's reader signals READER-ERROR for out-of-range numbers and yields an infinity with traps masked; the clause covers readers that signal ARITHMETIC-ERROR directly" :expected-uncovered-count 4)))
