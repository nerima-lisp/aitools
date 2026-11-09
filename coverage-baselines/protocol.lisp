(:context :protocol
 :state :measured
 :source :linux-ci
 :expression-covered 1708
 :expression-total 1795
 :branch-covered 212
 :branch-total 246
 :coverage-exclusions
 ((:kind :expression :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "COMMAND-SCHEMA"
   :reason "SB-COVER does not mark keyword defaults in the generated DEFSTRUCT constructor; schema tests exercise the omitted defaults.")
  (:kind :expression :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "MAKE-OK-ENVELOPE"
   :reason "SB-COVER attributes the :STATUS default expression to the DEFUN lambda list rather than calls that omit it; protocol tests exercise the default.")
  (:kind :branch :package "AITOOLS.PROTOCOL.APPLICATION" :symbol "COMMAND-DECLARATION"
   :reason "The group slot OR type is declarative metadata; validation admits only NIL or a non-empty string.")
  (:kind :branch :package "AITOOLS.PROTOCOL.APPLICATION" :symbol "COMMAND-RESULT"
   :reason "The kind slot MEMBER type is declarative metadata; the CPS adapter constructs only the three valid kinds.")
  (:kind :branch :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "COMMAND-SCHEMA"
   :reason "SB-COVER attributes the generated DEFSTRUCT constructor slot type check for (OR NULL STRING) to declarative type metadata; schema tests cover NIL, string, and invalid values.")
  (:kind :branch :package "AITOOLS.PROTOCOL.APPLICATION" :symbol "%REDACT-VALUE"
   :reason "The final T clause matches every remaining value, so the fall-through path after it is unreachable.")))

