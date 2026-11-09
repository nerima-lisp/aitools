(:context :process :state :measured :source :linux-ci
 :expression-covered 4908 :expression-total 4908
 :branch-covered 426 :branch-total 426
 :coverage-exclusions
 ((:kind :expression :package "AITOOLS.PROCESS.APPLICATION" :symbol "BG-LOGS/K"
   :reason "SB-COVER does not mark omitted keyword defaults in the DEFINE-FLOW lambda list; flow tests invoke the command without those options and assert the resulting defaults.")
  (:kind :expression :package "AITOOLS.PROCESS.APPLICATION" :symbol "BG-STOP/K"
   :reason "SB-COVER does not mark the omitted grace default in the DEFINE-FLOW lambda list; flow tests omit grace and assert the five-second behavior.")
  (:kind :expression :package "AITOOLS.PROCESS.APPLICATION" :symbol "%CALL-WITH-DURATION-MS"
   :reason "SB-COVER does not mark the omitted allow-zero default in the lambda list; duration tests omit the option and verify zero is accepted.")
  (:kind :expression :package "AITOOLS.PROCESS.APPLICATION" :symbol "RUN-REQUEST"
   :reason "SB-COVER does not mark omitted DEFSTRUCT constructor defaults; tests construct a request without options and assert every default value.")
  (:kind :branch :package "AITOOLS.PROCESS.APPLICATION" :symbol "RUN-REQUEST"
   :reason "SB-COVER does not instrument DEFSTRUCT-generated slot type checks; request tests exercise valid and invalid values for the declared slot types.")
  (:kind :expression :package "AITOOLS.PROCESS.APPLICATION" :symbol "SIGNAL-COMMAND/K"
   :reason "SB-COVER does not mark the omitted signal default in the DEFINE-FLOW lambda list; flow tests omit signal and verify TERM is sent.")
  (:kind :branch :package "AITOOLS.PROCESS.APPLICATION" :symbol "WAIT-REQUEST"
   :reason "SB-COVER does not instrument DEFSTRUCT-generated slot type checks; wait request tests exercise valid and invalid values for the declared slot types.")
  (:kind :expression :package "AITOOLS.PROCESS.APPLICATION" :symbol "WAIT-REQUEST"
   :reason "SB-COVER does not mark the omitted timeout default in the DEFSTRUCT constructor; tests construct a request without options and assert the default.")
  (:kind :branch :package "AITOOLS.PROCESS.DOMAIN" :symbol "BG-RECORD"
   :reason "SB-COVER does not instrument DEFSTRUCT-generated slot type checks; record tests exercise valid and invalid name and pid values.")
  (:kind :branch :package "AITOOLS.PROCESS.DOMAIN" :symbol "LOG-SLICE"
   :reason "SB-COVER does not instrument DEFSTRUCT-generated slot type checks; log-slice tests exercise valid and invalid offset and redaction values.")
  (:kind :branch :package "AITOOLS.PROCESS.DOMAIN" :symbol "OUTPUT-REPORT"
   :reason "SB-COVER does not instrument DEFSTRUCT-generated slot type checks; report tests exercise valid and invalid line-count and redaction values.")
  (:kind :branch :package "AITOOLS.PROCESS.DOMAIN" :symbol "PROCESS-OUTCOME"
   :reason "SB-COVER does not instrument DEFSTRUCT-generated slot type checks; outcome tests exercise valid and invalid exit-code, signal, and duration values.")
  (:kind :expression :package "AITOOLS.PROCESS.DOMAIN" :symbol "DECODE-OUTPUT-OCTETS"
   :reason "SB-COVER does not mark the omitted end default in the keyword lambda list; decoder tests omit the range and verify the whole octet vector is decoded.")
  (:kind :branch :package "AITOOLS.PROCESS.DOMAIN" :symbol "WAIT-CONDITION"
   :reason "SB-COVER does not instrument DEFSTRUCT-generated slot type checks; wait-condition tests exercise valid and invalid values for every declared slot type.")
  (:kind :expression :package "AITOOLS.PROCESS.INFRASTRUCTURE" :symbol "%OPEN-INPUT-FILE"
   :reason "SB-COVER does not mark the omitted if-does-not-exist default in the keyword lambda list; file tests omit it and verify missing files signal a port error.")
  (:kind :expression :package "AITOOLS.PROCESS.INFRASTRUCTURE" :symbol "*LAUNCHED-SUPERVISORS*"
   :reason "The hash table initializer runs once while loading the source, before coverage instrumentation begins; supervisor tracking tests exercise the initialized table.")
  (:kind :expression :package "AITOOLS.PROCESS.INFRASTRUCTURE" :symbol "*LAUNCHED-SUPERVISORS-LOCK*"
   :reason "The lock initializer runs once while loading the source, before coverage instrumentation begins; supervisor tracking tests exercise the initialized lock.")))
