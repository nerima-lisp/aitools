(:context :search
 :state :measured
 :source :linux-ci
 :expression-covered 5358
 :expression-total 5358
 :branch-covered 537
 :branch-total 537
 :coverage-exclusions
 ((:kind :expression :package "AITOOLS.SEARCH.APPLICATION" :symbol "CODE-OUTLINE/K"
   :reason "SB-COVER leaves DEFINE-FLOW lambda-list defaults uncovered when callers omit the keyword; specs omit :LIMIT and assert the 200-item cap.")
  (:kind :expression :package "AITOOLS.SEARCH.APPLICATION" :symbol "CODE-DEFS/K"
   :reason "SB-COVER leaves DEFINE-FLOW lambda-list defaults uncovered when callers omit the keyword; specs omit :LIMIT and assert the 50-definition cap.")
  (:kind :expression :package "AITOOLS.SEARCH.APPLICATION" :symbol "CODE-REFS/K"
   :reason "SB-COVER leaves DEFINE-FLOW lambda-list defaults uncovered when callers omit the keyword; specs omit :LIMIT and assert the 50-reference cap.")
  (:kind :expression :package "AITOOLS.SEARCH.APPLICATION" :symbol "FIND/K"
   :reason "SB-COVER leaves DEFINE-FLOW lambda-list defaults uncovered when callers omit the keyword; specs omit sort and limit overrides, verify path sorting against size and mtime, assert flat items, and check the 50-item cap.")
  (:kind :expression :package "AITOOLS.SEARCH.APPLICATION" :symbol "OVERVIEW/K"
   :reason "SB-COVER leaves DEFINE-FLOW lambda-list defaults uncovered when callers omit the keyword; specs omit :LIMIT and return all 12 supported language rows, which cannot distinguish the default of 30 from a larger limit.")
  (:kind :expression :package "AITOOLS.SEARCH.APPLICATION" :symbol "SEARCH/K"
   :reason "SB-COVER leaves DEFINE-FLOW lambda-list defaults uncovered when callers omit the keyword; specs omit overrides and assert two context lines, block output, and the 15-result cap.")
  (:kind :branch :package "AITOOLS.SEARCH.DOMAIN" :symbol "FOUND-ENTRY"
   :reason "SB-COVER reports generated DEFSTRUCT slot type-check branches as source-mapped gaps; constructor specs pass invalid values and verify TYPE-ERROR.")
  (:kind :branch :package "AITOOLS.SEARCH.DOMAIN" :symbol "PROGRAM"
   :reason "SB-COVER reports generated DEFSTRUCT slot type-check branches as source-mapped gaps; constructor specs pass invalid values and verify TYPE-ERROR.")
  (:kind :branch :package "AITOOLS.SEARCH.DOMAIN" :symbol "FILE-OUTCOME"
   :reason "SB-COVER reports generated DEFSTRUCT slot type-check branches as source-mapped gaps; constructor specs pass invalid values and verify TYPE-ERROR.")))
