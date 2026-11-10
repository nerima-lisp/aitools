(:context :store
 :state :measured
 :source :linux-ci
 :expression-covered 7607
 :expression-total 7836
 :branch-covered 685
 :branch-total 810
 :coverage-exclusions
 ((:kind :branch :package "AITOOLS.STORE.APPLICATION" :symbol "%ARCHIVE-PREFIX-CURSOR"
   :reason "These DEFSTRUCT guards are compiler-generated and have no source-mapped SB-COVER branches. No valid cursor state can contain an invalid descriptor or identity; specs verify malformed constructor values are rejected.")
  (:kind :branch :package "aitools.store.application" :symbol "tx-status"
   :reason "CLHS DEFSTRUCT: slot :TYPE is not evaluated; any type checking is implementation-dependent.")
  (:kind :branch :package "aitools.store.domain" :symbol "change-request"
   :reason "CLHS DEFSTRUCT: slot :TYPE is not evaluated; any type checking is implementation-dependent.")
  (:kind :branch :package "aitools.store.domain" :symbol "change-result"
   :reason "CLHS DEFSTRUCT: slot :TYPE is not evaluated; any type checking is implementation-dependent.")
  (:kind :branch :package "aitools.store.domain" :symbol "conflict"
   :reason "CLHS DEFSTRUCT: slot :TYPE is not evaluated; any type checking is implementation-dependent.")
  (:kind :branch :package "aitools.store.domain" :symbol "entry-state"
   :reason "CLHS DEFSTRUCT: slot :TYPE is not evaluated; any type checking is implementation-dependent.")
  (:kind :branch :package "aitools.store.domain" :symbol "intent-step"
   :reason "CLHS DEFSTRUCT: slot :TYPE is not evaluated; any type checking is implementation-dependent.")
  (:kind :branch :package "aitools.store.domain" :symbol "tx-index"
   :reason "CLHS DEFSTRUCT: slot :TYPE is not evaluated; any type checking is implementation-dependent.")
  (:kind :branch :package "aitools.store.domain" :symbol "tx-meta"
   :reason "CLHS DEFSTRUCT: slot :TYPE is not evaluated; any type checking is implementation-dependent.")
  (:kind :branch :package "aitools.store.domain" :symbol "tx-op-record"
   :reason "CLHS DEFSTRUCT: slot :TYPE is not evaluated; any type checking is implementation-dependent.")
  (:kind :branch :package "aitools.store.domain" :symbol "plan-changes/k"
   :reason "Empty paths are rejected before planning, and proper ancestors never include the root.")
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "%ARCHIVE-PREFIX-START"
   :reason "After cursor allocation, only non-failing local assignments remain before STARTED is set; no callback can enter this cleanup except asynchronous interruption.")
  (:kind :expression :package "aitools.store.domain" :symbol "plan-changes/k"
   :reason "Valid request paths must be nonempty and path-ancestors never yields the workspace root, so planner state for the empty path is unreachable.")
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "%DIRECTORY-FINALIZATION-STEPS"
   :reason "Every public completion path checkpoints a created directory identity before finalization; the checkpoint fails first if identity is unavailable.")
  (:kind :expression :package "AITOOLS.STORE.INFRASTRUCTURE" :symbol "%CREATE-FILE" :reason "The compiled SBCL FCHMOD syscall succeeds for a valid descriptor on the writable test filesystem; function-cell fault injection does not reach this inlined call, and a deterministic failure seam would require changing production code solely for the test.")))
