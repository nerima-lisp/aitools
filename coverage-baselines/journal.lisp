(:context :journal
 :state :measured
 :source :linux-ci
 :expression-covered 1426
 :expression-total 1426
 :branch-covered 60
 :branch-total 60
 :coverage-exclusions
 ((:kind :expression
   :package "AITOOLS.JOURNAL.APPLICATION"
   :symbol "HISTORY-FLOW"
   :reason "SB-COVER reports the top-level DEFINE-FLOW declaration form; runtime specs exercise the generated history flow through history and dispatch.")
  (:kind :expression
   :package "AITOOLS.JOURNAL.APPLICATION"
   :symbol "*TX-REPLAYERS*"
   :reason "SB-COVER reports the one-time DEFVAR registry declaration; runtime registry specs exercise registration and lookup, and tx rebase specs exercise its consumers.")
  (:kind :expression
   :package "AITOOLS.JOURNAL.APPLICATION"
   :symbol "TX-COMMIT-FLOW"
   :reason "SB-COVER reports the top-level DEFINE-FLOW declaration form; runtime specs exercise the generated commit flow through tx commit and dispatch.")
  (:kind :expression
   :package "AITOOLS.JOURNAL.APPLICATION"
   :symbol "TX-DIFF-FLOW"
   :reason "SB-COVER reports the top-level DEFINE-FLOW declaration form; runtime specs exercise the generated diff flow through tx diff and dispatch.")
  (:kind :expression
   :package "AITOOLS.JOURNAL.APPLICATION"
   :symbol "UNDO-FLOW"
   :reason "SB-COVER reports the top-level DEFINE-FLOW declaration form; runtime specs exercise the generated undo flow through undo and dispatch.")
  (:kind :branch
   :package "AITOOLS.JOURNAL.APPLICATION"
   :symbol "JOURNAL-CONTEXT"
   :reason "SB-COVER attributes JOURNAL-CONTEXT's generated ROOT and LOCK-TIMEOUT type-check branches to DEFSTRUCT without source-mapped counters; specs cover omitted defaults, supplied strings, and rejected values for both options.")))
