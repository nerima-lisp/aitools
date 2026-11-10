(:context :store
 :state :measured
 :source :linux-ci
 :expression-covered 7607
 :expression-total 7836
 :branch-covered 685
 :branch-total 810
 :coverage-exclusions
 ((:kind :branch :package "AITOOLS.STORE.APPLICATION" :symbol "STORE-IO"
   :reason "The DEFSTRUCT-generated slot type checks are compiler-generated guards without source-mapped SB-COVER branches. Production adapters and COPY-STORE-IO pass only NIL or functions; specs assert invalid search-port values are rejected.")
  (:kind :branch :package "AITOOLS.STORE.APPLICATION" :symbol "%ARCHIVE-PREFIX-CURSOR"
   :reason "The DEFSTRUCT-generated slot type checks are compiler-generated guards without source-mapped SB-COVER branches. Production cursor constructors use only validated descriptors and identities; specs assert invalid slot values are rejected.")))
