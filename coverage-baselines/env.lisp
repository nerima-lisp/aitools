(:context :env
 :state :measured
 :source :linux-ci
 :expression-covered 4973
 :expression-total 4973
 :branch-covered 478
 :branch-total 478
 :coverage-exclusions
 ((:kind :expression :package "AITOOLS.ENV.PRESENTATION" :symbol "DEFINE-ENV-COMMAND"
   :reason "Runtime requests execute the expansion, not this macro body; no runtime input reaches the template. Command contract specs exercise each registered command.")
  (:kind :expression :package "AITOOLS.ENV.APPLICATION" :symbol "DEFINE-ENV-OPERATION"
   :reason "Flows execute expanded boundary functions; the macro body runs only during source expansion, so runtime input cannot reach the template.")
  (:kind :expression :package "AITOOLS.ENV.DOMAIN" :symbol "ASCII-DIGITS-P"
   :reason "SB-COVER does not attribute lambda-list initforms to calls omitting :START and :END; civil-time specs exercise the default range.")
  (:kind :expression :package "AITOOLS.ENV.APPLICATION" :symbol "SYS-NETWORK/K"
   :reason "SB-COVER does not attribute lambda-list initforms to calls omitting :LIMIT; network specs assert the default limit.")
  (:kind :expression :package "AITOOLS.ENV.APPLICATION" :symbol "SYS-PROCS/K"
   :reason "SB-COVER does not attribute lambda-list initforms to calls omitting :LIMIT; process specs exercise the default limit.")
  (:kind :expression :package "AITOOLS.ENV.APPLICATION" :symbol "SYS-METRICS/K"
   :reason "SB-COVER does not attribute lambda-list initforms to calls omitting :LIMIT; metrics specs exercise the default limit.")
  (:kind :expression :package "AITOOLS.ENV.APPLICATION" :symbol "SYS-SNAPSHOT/K"
   :reason "SB-COVER does not attribute lambda-list initforms to calls omitting the three limit keywords; snapshot specs assert all defaults.")
  (:kind :expression :package "AITOOLS.ENV.APPLICATION" :symbol "SYS-TOOLS/K"
   :reason "SB-COVER does not attribute the :TIMEOUT lambda-list initform to calls omitting it; tool specs assert the default timeout.")
  (:kind :expression :package "AITOOLS.ENV.APPLICATION" :symbol "TIME-CONVERT/K"
   :reason "SB-COVER does not attribute the :TO lambda-list initform to calls omitting it; time specs assert the default format.")
  (:kind :branch :package "AITOOLS.ENV.DOMAIN" :symbol "TIME-INPUT"
   :reason "The DEFSTRUCT-generated MEMBER slot checks are declarative metadata without source-mapped SB-COVER counters; command text cannot set the parser-internal kind, and parser specs cover the valid and rejected constructor kinds.")
  (:kind :branch :package "AITOOLS.ENV.DOMAIN" :symbol "%PARSE-KIT-OFFSET-SECONDS"
   :reason "The caller passes a suffix beginning at a found offset character, so the suffix is never empty and input cannot reach the zero-length path.")))
