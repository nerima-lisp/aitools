(:context :kernel
 :state :measured :source :linux-ci
 :expression-covered 3083 :expression-total 3094
 :branch-covered 394 :branch-total 418
 :coverage-exclusions
 ((:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "*FLOW-SIGNATURES*"
   :reason "The DEFVAR initializer runs during initial system loading before counters reset; flow registration behavior is covered by flow specs.")
  (:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "JSON-MODEL-ADD"
   :reason "The ARRAY-MODE keyword initializer is exercised by calls that omit the keyword; SB-COVER does not attribute lambda-list initialization forms.")
  (:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "GENERATE-DIFF-HUNKS"
   :reason "The CONTEXT and FINAL-NEWLINE-A/B defaults are exercised by default-option specs; SB-COVER does not attribute lambda-list initialization forms.")
  (:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "GENERATE-UNIFIED-DIFF"
   :reason "The CONTEXT and FINAL-NEWLINE-A/B defaults are exercised by default-option specs; SB-COVER does not attribute lambda-list initialization forms.")
  (:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "PARSE-UNIFIED-DIFF"
   :reason "The STRIP default is exercised by parser specs; SB-COVER does not attribute lambda-list initialization forms.")
  (:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "APPLY-HUNKS/K"
   :reason "The FUZZ default is exercised by patch specs; SB-COVER does not attribute lambda-list initialization forms.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "EXPECT-HASH-ENTRY"
   :reason "The DEFSTRUCT-generated PATH slot type-check branch has no source-mapped SB-COVER counter; constructor specs cover valid and invalid PATH values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "SELECTOR"
   :reason "The DEFSTRUCT-generated slot type-check branches have no source-mapped SB-COVER counters; selector constructor specs cover valid and invalid slot values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "SELECTOR-CATALOG-ENTRY"
   :reason "The DEFSTRUCT-generated BASIS slot type-check branch has no source-mapped SB-COVER counter; selector catalog specs cover valid and invalid BASIS values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "FILE-PATCH"
   :reason "The DEFSTRUCT-generated OLD-PATH slot type-check branch has no source-mapped SB-COVER counter; file patch constructor specs cover valid and invalid OLD-PATH values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "DIFF-LINE"
   :reason "The DEFSTRUCT-generated OP slot type-check branch has no source-mapped SB-COVER counter; diff-line constructor specs cover valid and invalid OP values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "DIFF-HUNK"
   :reason "The DEFSTRUCT-generated OLD-COUNT slot type-check branch has no source-mapped SB-COVER counter; diff-hunk constructor specs cover valid and invalid OLD-COUNT values.")))

