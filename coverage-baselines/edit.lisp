(:context :edit
 :state :measured
 :source :linux-ci
 :expression-covered 13390
 :expression-total 13517
 :branch-covered 1418
 :branch-total 1456
 :coverage-exclusions
 ((:kind :expression
   :package "AITOOLS.EDIT.APPLICATION"
   :symbol "RESOLVE-EXTRA-PATH/K"
   :reason "SB-COVER does not record DEFINE-FLOW keyword defaults; the edit boundary spec exercises the default :BASE :ROOT path and omits :FOLLOW.")
  (:kind :expression
   :package "AITOOLS.EDIT.APPLICATION"
   :symbol "WRITE-TARGET"
   :reason "SB-COVER does not record DEFSTRUCT keyword defaults; pipeline tests assert the default :FOLLOW T and :BASE :CWD values.")
  (:kind :expression
   :package "AITOOLS.EDIT.APPLICATION"
   :symbol "WRITE-PLAN"
   :reason "SB-COVER does not record generated DEFSTRUCT slot defaults; pipeline tests assert the default plan slots used by existing edit commands.")
  (:kind :expression
   :package "AITOOLS.EDIT.APPLICATION"
   :symbol "WRITE-CONTEXT"
   :reason "SB-COVER does not record generated DEFSTRUCT slot defaults; pipeline tests assert the empty :PATHS and :HASH-PATHS values used by existing edit commands.")
  (:kind :expression :package "AITOOLS.EDIT.APPLICATION" :symbol "MAKE-EDIT-WRITE-BOUNDARIES" :reason "The macro body runs only during compile-time expansion; SB-COVER cannot record it as a runtime expression, and boundary contract tests execute and verify the generated two-key constructor call.")))
