(:context :util
 :state :measured
 :source :linux-ci
 :expression-covered 2733
 :expression-total 2733
 :branch-covered 242
 :branch-total 242
 :coverage-exclusions
 ((:kind :expression
   :package "AITOOLS.UTIL.DOMAIN"
   :symbol "%WITH-DEPTH"
   :reason "SB-COVER instruments runtime forms, but this macro body runs while expanding callers; parser nesting tests exercise the generated depth guard.")
  (:kind :expression
   :package "AITOOLS.UTIL.PRESENTATION"
   :symbol "%WITH-COMMAND-RESULT"
   :reason "SB-COVER instruments runtime forms, but this macro body runs while expanding command handlers; CLI integration tests exercise the generated command-result calls.")
  (:kind :branch
   :package "AITOOLS.UTIL.APPLICATION"
   :symbol "INPUT-REQUEST"
   :reason "SB-COVER attributes the generated DEFSTRUCT constructor slot type check for (OR NULL STRING) to declarative type metadata; util flow specs exercise NIL and string content values.")))
