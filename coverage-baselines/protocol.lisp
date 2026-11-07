(:context :protocol
 :state :measured
 :source :linux-ci
 :expression-covered 1708
 :expression-total 1795
 :branch-covered 212
 :branch-total 246
 :coverage-exclusion-counts (:expression 20 :branch 3)
 :coverage-uncovered-counts (:expression 54 :branch 7)
 :coverage-exclusions
 ((:kind :expression :package "CL-USER" :symbol "IN-PACKAGE"
   :reason "Package declarations load before CL-Weave starts collecting coverage.")
  (:kind :expression :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "IN-PACKAGE"
   :reason "Package-switch forms load before CL-Weave starts collecting coverage.")
  (:kind :expression :package "AITOOLS.PROTOCOL.APPLICATION" :symbol "IN-PACKAGE"
   :reason "Package-switch forms load before CL-Weave starts collecting coverage.")
  (:kind :expression :package "AITOOLS.PROTOCOL.INFRASTRUCTURE" :symbol "IN-PACKAGE"
   :reason "Package-switch forms load before CL-Weave starts collecting coverage.")
  (:kind :expression :package "CL-USER" :symbol "DEFPACKAGE"
   :reason "Package definitions and exports load before CL-Weave starts collecting coverage.")
  (:kind :expression :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "COMMAND-SCHEMA"
   :reason "DEFSTRUCT constructor and slot declarations load before coverage collection.")
  (:kind :expression :package "AITOOLS.PROTOCOL.APPLICATION" :symbol "COMMAND-DECLARATION"
   :reason "DEFSTRUCT constructor and slot declarations load before coverage collection.")
  (:kind :expression :package "AITOOLS.PROTOCOL.APPLICATION" :symbol "COMMAND-REGISTRY"
   :reason "DEFSTRUCT constructor and slot declarations load before coverage collection.")
  (:kind :expression :package "AITOOLS.PROTOCOL.APPLICATION" :symbol "COMMAND-RESULT"
   :reason "DEFSTRUCT constructor and slot declarations load before coverage collection.")
  (:kind :expression :package "AITOOLS.PROTOCOL.INFRASTRUCTURE" :symbol "+ENVELOPE-MAX-LENGTH+"
   :reason "Constant initialization runs before coverage collection; output-limit behavior is tested.")
  (:kind :expression :package "AITOOLS.PROTOCOL.INFRASTRUCTURE" :symbol "ENVELOPE-TOO-LARGE"
   :reason "Condition definition runs before coverage collection; signaling behavior is tested.")
  (:kind :expression :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "*ERROR-CODES*"
   :reason "The data-backed error catalog is bound before coverage collection; catalog behavior is tested.")
  (:kind :expression :package "AITOOLS.PROTOCOL.APPLICATION" :symbol "*REDACTION-COUNT*"
   :reason "The special variable declaration runs before coverage collection; dynamic counting behavior is tested.")
  (:kind :expression :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "MAKE-OK-ENVELOPE"
   :reason "The top-level DEFUN form for MAKE-OK-ENVELOPE is evaluated while loading the system, before CL-Weave starts collecting coverage.")
  (:kind :expression :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "+REDACTED-SECRET+"
   :reason "The redaction marker is initialized before coverage collection; output use is tested.")
  (:kind :expression :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "DECLAIM"
   :reason "The inline compiler declaration has no runtime behavior and loads before coverage collection.")
  (:kind :expression :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "+LITERAL-CANDIDATE+"
   :reason "Candidate constants are initialized before coverage collection; redaction behavior is tested.")
  (:kind :expression :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "+AWS-CANDIDATE+"
   :reason "Candidate constants are initialized before coverage collection; redaction behavior is tested.")
  (:kind :expression :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "+BEARER-CANDIDATE+"
   :reason "Candidate constants are initialized before coverage collection; redaction behavior is tested.")
  (:kind :expression :package "AITOOLS.PROTOCOL.DOMAIN" :symbol "+ASSIGNMENT-CANDIDATE+"
   :reason "Candidate constants are initialized before coverage collection; redaction behavior is tested.")
  (:kind :branch :package "AITOOLS.PROTOCOL.APPLICATION" :symbol "COMMAND-DECLARATION"
   :reason "The group slot's OR type is declarative metadata; validation admits only NIL or a non-empty string.")
  (:kind :branch :package "AITOOLS.PROTOCOL.APPLICATION" :symbol "COMMAND-RESULT"
   :reason "The kind slot's MEMBER type is declarative metadata; the CPS adapter constructs only the three valid kinds.")
  (:kind :branch :package "AITOOLS.PROTOCOL.APPLICATION" :symbol "%REDACT-VALUE"
   :reason "The final T clause matches every remaining value, so the fall-through path after it is unreachable.")))

