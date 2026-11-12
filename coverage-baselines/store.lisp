(:context :store
 :state :measured
 :source :linux-ci
 :expression-covered 8746
 :expression-total 8746
 :branch-covered 862
 :branch-total 862
 :coverage-exclusions
 ((:kind :branch :package "AITOOLS.STORE.APPLICATION" :symbol "%ARCHIVE-PREFIX-CURSOR"
   :reason "SB-COVER cannot source-map compiler-generated DEFSTRUCT :TYPE slot guards; invalid slot values reach only these generated checks, which have no source expression." :expected-uncovered-count 4)
  (:kind :branch :package "aitools.store.application" :symbol "tx-status"
   :reason "SB-COVER cannot source-map compiler-generated DEFSTRUCT :TYPE slot guards; invalid slot values reach only these generated checks, which have no source expression." :expected-uncovered-count 2)
  (:kind :branch :package "aitools.store.domain" :symbol "change-request"
   :reason "SB-COVER cannot source-map compiler-generated DEFSTRUCT :TYPE slot guards; invalid slot values reach only these generated checks, which have no source expression." :expected-uncovered-count 10)
  (:kind :branch :package "aitools.store.domain" :symbol "change-result"
   :reason "SB-COVER cannot source-map compiler-generated DEFSTRUCT :TYPE slot guards; invalid slot values reach only these generated checks, which have no source expression." :expected-uncovered-count 6)
  (:kind :branch :package "aitools.store.domain" :symbol "conflict"
   :reason "SB-COVER cannot source-map compiler-generated DEFSTRUCT :TYPE slot guards; invalid slot values reach only these generated checks, which have no source expression." :expected-uncovered-count 2)
  (:kind :branch :package "aitools.store.domain" :symbol "entry-state"
   :reason "SB-COVER cannot source-map compiler-generated DEFSTRUCT :TYPE slot guards; invalid slot values reach only these generated checks, which have no source expression." :expected-uncovered-count 6)
  (:kind :branch :package "aitools.store.domain" :symbol "intent-step"
   :reason "SB-COVER cannot source-map compiler-generated DEFSTRUCT :TYPE slot guards; invalid slot values reach only these generated checks, which have no source expression." :expected-uncovered-count 16)
  (:kind :branch :package "aitools.store.domain" :symbol "tx-index"
   :reason "SB-COVER cannot source-map compiler-generated DEFSTRUCT :TYPE slot guards; invalid slot values reach only these generated checks, which have no source expression." :expected-uncovered-count 2)
  (:kind :branch :package "aitools.store.domain" :symbol "tx-meta"
   :reason "SB-COVER cannot source-map compiler-generated DEFSTRUCT :TYPE slot guards; invalid slot values reach only these generated checks, which have no source expression." :expected-uncovered-count 2)
  (:kind :branch :package "aitools.store.domain" :symbol "tx-op-record"
   :reason "SB-COVER cannot source-map compiler-generated DEFSTRUCT :TYPE slot guards; invalid slot values reach only these generated checks, which have no source expression." :expected-uncovered-count 2)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "COLLECT-GARBAGE"
   :reason "SB-COVER cannot source-map the omitted &key initializer (journal :reread); SBCL evaluates it in generated argument binding rather than the function body." :expected-uncovered-count 1)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "CALL-WITH-TEMP-DIR"
   :reason "SB-COVER cannot source-map the omitted &key initializer (mode #o700); SBCL evaluates it in generated argument binding rather than CALL-WITH-TEMP-DIR's body." :expected-uncovered-count 1)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "RECOVER/K"
   :reason "SB-COVER cannot source-map recover/k's omitted &key initforms for lock-timeout-ms and on-failed; SBCL evaluates them in generated argument binding." :expected-uncovered-count 1)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "TX-COMMIT/K"
   :reason "SB-COVER cannot source-map tx-commit/k's omitted &key initializer (lock-timeout-ms +default-lock-timeout-ms+); SBCL evaluates it in generated argument binding." :expected-uncovered-count 1)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "TX-DROP/K"
   :reason "SB-COVER cannot source-map tx-drop/k's omitted &key initializer (lock-timeout-ms +default-lock-timeout-ms+); SBCL evaluates it in generated argument binding." :expected-uncovered-count 1)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "TX-REBASE/K"
   :reason "SB-COVER cannot source-map tx-rebase/k's omitted &key initializer (lock-timeout-ms +default-lock-timeout-ms+); SBCL evaluates it in generated argument binding." :expected-uncovered-count 1)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "TX-STAGE/K"
   :reason "SB-COVER cannot source-map tx-stage/k's omitted &key initializer (lock-timeout-ms +default-lock-timeout-ms+) or the rejection lambda's candidates, diagnostics, conflicts, repairs, and path defaults; SBCL evaluates them in generated argument binding." :expected-uncovered-count 6)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "%RUN-UNDER-TX-LOCKS"
   :reason "SB-COVER cannot source-map the omitted &key initializer (workspace t); SBCL evaluates it in generated argument binding rather than %RUN-UNDER-TX-LOCKS's body." :expected-uncovered-count 1)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "TX-BEGIN/K"
   :reason "SB-COVER cannot source-map tx-begin/k's omitted &key initializer (lock-timeout-ms +default-lock-timeout-ms+); SBCL evaluates it in generated argument binding." :expected-uncovered-count 1)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "TX-RECORD-READ/K"
   :reason "SB-COVER cannot source-map tx-record-read/k's omitted &key initializer (lock-timeout-ms +default-lock-timeout-ms+); SBCL evaluates it in generated argument binding." :expected-uncovered-count 1)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "TX-ABORT/K"
   :reason "SB-COVER cannot source-map tx-abort/k's omitted &key initializer (lock-timeout-ms +default-lock-timeout-ms+); SBCL evaluates it in generated argument binding." :expected-uncovered-count 1)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "UNDO-OP/K"
   :reason "SB-COVER cannot source-map undo-op/k's omitted &key initforms for lock-timeout-ms and rejection continuation fields; SBCL evaluates them in generated argument binding." :expected-uncovered-count 6)
  (:kind :expression :package "AITOOLS.STORE.APPLICATION" :symbol "COMMIT-CHANGES/K"
   :reason "SB-COVER cannot source-map commit-changes/k's omitted &key initializer (lock-timeout-ms +default-lock-timeout-ms+) or the rejection lambda's candidates, diagnostics, conflicts, repairs, and path defaults; SBCL evaluates them in generated argument binding." :expected-uncovered-count 6)
  (:kind :expression :package "aitools.store.domain" :symbol "CHANGES->JSON"
   :reason "SB-COVER cannot source-map the omitted &key initializer (max-diff-lines +default-max-diff-lines+); SBCL evaluates it in generated argument binding rather than CHANGES->JSON's body." :expected-uncovered-count 1)
  (:kind :expression :package "aitools.store.domain" :symbol "WRITE-RESULT-FIELDS"
   :reason "SB-COVER cannot source-map the omitted &key initializer (max-diff-lines +default-max-diff-lines+); SBCL evaluates it in generated argument binding rather than WRITE-RESULT-FIELDS's body." :expected-uncovered-count 1)
  (:kind :expression :package "aitools.store.domain" :symbol "%JSON-FIELD"
   :reason "SB-COVER cannot source-map %JSON-FIELD's omitted &key initforms (type t) and (required t); SBCL evaluates them in generated argument binding." :expected-uncovered-count 2)
  (:kind :expression :package "aitools.store.domain" :symbol "%JSON-LIST"
   :reason "SB-COVER cannot source-map %JSON-LIST's omitted &key initializer (required t); SBCL evaluates it in generated argument binding." :expected-uncovered-count 1)
  (:kind :expression :package "aitools.store.domain" :symbol "RETENTION-REMOVALS"
   :reason "SB-COVER cannot source-map the omitted &key initializer (generations +retained-generations+); SBCL evaluates it in generated argument binding." :expected-uncovered-count 1)
  (:kind :expression :package "AITOOLS.STORE.INFRASTRUCTURE" :symbol "MAKE-POSIX-STORE-IO"
   :reason "SB-COVER cannot source-map the :mkdir lambda's omitted &key initializer (mode #o777); SBCL evaluates it in generated argument binding." :expected-uncovered-count 1)
  (:kind :expression :package "AITOOLS.STORE.INFRASTRUCTURE" :symbol "%OPEN-DIRECTORY-AT"
   :reason "SB-COVER cannot source-map %OPEN-DIRECTORY-AT's omitted &key initializer (follow nil); SBCL evaluates it in generated argument binding." :expected-uncovered-count 1)))
