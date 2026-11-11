(:context :util
 :state :measured
 :source :linux-ci
 :expression-covered 2733
 :expression-total 2733
 :branch-covered 244
 :branch-total 244
 :coverage-exclusions
 ((:kind :expression
   :package "AITOOLS.UTIL.DOMAIN"
   :symbol "%WITH-DEPTH"
   :reason "SB-COVER instruments runtime forms, but this macro body runs while expanding callers; parser nesting tests exercise the generated depth guard.")
  (:kind :expression
   :package "AITOOLS.UTIL.PRESENTATION"
   :symbol "%WITH-COMMAND-RESULT"
   :reason "SB-COVER instruments runtime forms, but this macro body runs while expanding command handlers; CLI integration tests exercise the generated command-result calls."))
)
