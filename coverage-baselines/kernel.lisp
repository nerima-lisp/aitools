(:context :kernel
 :state :measured :source :linux-ci
 :expression-covered 3083 :expression-total 3094
 :branch-covered 394 :branch-total 418
 :coverage-exclusion-counts (:expression 7 :branch 12)
 :coverage-uncovered-counts (:expression 11 :branch 24)
 :coverage-exclusions
 ((:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "*FLOW-SIGNATURES*"
   :file "packages/core/kernel/src/domain/flow.lisp" :lines (3)
   :reason "The DEFVAR initializer runs during initial system loading before counters reset; flow registration behavior is covered by flow specs.")
  (:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "JSON-MODEL-ADD"
   :file "packages/core/kernel/src/domain/json-model.lisp" :lines (110)
   :reason "The ARRAY-MODE keyword initializer is exercised by calls that omit the keyword; SB-COVER does not attribute lambda-list initialization forms.")
  (:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "DEFINE-SELECTOR-CONSTRUCTOR"
   :file "packages/core/kernel/src/domain/selector.lisp" :lines (65 66)
   :reason "The macro template is expanded during source loading before runtime counters reset; generated public constructors are covered by selector specs.")
  (:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "GENERATE-DIFF-HUNKS"
   :file "packages/core/kernel/src/domain/unified-diff.lisp" :lines (135 137)
   :reason "The CONTEXT and FINAL-NEWLINE-A/B defaults are exercised by default-option specs; SB-COVER does not attribute lambda-list initialization forms.")
  (:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "GENERATE-UNIFIED-DIFF"
   :file "packages/core/kernel/src/domain/unified-diff.lisp" :lines (214 215)
   :reason "The CONTEXT and FINAL-NEWLINE-A/B defaults are exercised by default-option specs; SB-COVER does not attribute lambda-list initialization forms.")
  (:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "PARSE-UNIFIED-DIFF"
   :file "packages/core/kernel/src/domain/unified-diff-patch.lisp" :lines (58)
   :reason "The STRIP default is exercised by parser specs; SB-COVER does not attribute lambda-list initialization forms.")
  (:kind :expression :package "AITOOLS.KERNEL.DOMAIN" :symbol "APPLY-HUNKS/K"
   :file "packages/core/kernel/src/domain/unified-diff-patch.lisp" :lines (159)
   :reason "The FUZZ default is exercised by patch specs; SB-COVER does not attribute lambda-list initialization forms.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "EXPECT-HASH-ENTRY.PATH"
   :file "packages/core/kernel/src/domain/guard.lisp" :lines (15)
   :reason "This DEFSTRUCT-generated slot type-check branch has no source-mapped SB-COVER counter; constructor specs cover valid and invalid PATH values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "SELECTOR.KIND"
   :file "packages/core/kernel/src/domain/selector.lisp" :lines (18)
   :reason "This DEFSTRUCT-generated slot type-check branch has no source-mapped SB-COVER counter; selector constructor specs cover valid and invalid KIND values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "SELECTOR.RANGE-START"
   :file "packages/core/kernel/src/domain/selector.lisp" :lines (19)
   :reason "This DEFSTRUCT-generated slot type-check branch has no source-mapped SB-COVER counter; selector constructor specs cover valid and invalid RANGE-START values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "SELECTOR.RANGE-END"
   :file "packages/core/kernel/src/domain/selector.lisp" :lines (20)
   :reason "This DEFSTRUCT-generated slot type-check branch has no source-mapped SB-COVER counter; selector constructor specs cover valid and invalid RANGE-END values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "SELECTOR.SYMBOL-NAME"
   :file "packages/core/kernel/src/domain/selector.lisp" :lines (21)
   :reason "This DEFSTRUCT-generated slot type-check branch has no source-mapped SB-COVER counter; selector constructor specs cover valid and invalid SYMBOL-NAME values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "SELECTOR.SYMBOL-KIND"
   :file "packages/core/kernel/src/domain/selector.lisp" :lines (22)
   :reason "This DEFSTRUCT-generated slot type-check branch has no source-mapped SB-COVER counter; selector constructor specs cover valid and invalid SYMBOL-KIND values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "SELECTOR.BETWEEN-START"
   :file "packages/core/kernel/src/domain/selector.lisp" :lines (23)
   :reason "This DEFSTRUCT-generated slot type-check branch has no source-mapped SB-COVER counter; selector constructor specs cover valid and invalid BETWEEN-START values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "SELECTOR.BETWEEN-END"
   :file "packages/core/kernel/src/domain/selector.lisp" :lines (24)
   :reason "This DEFSTRUCT-generated slot type-check branch has no source-mapped SB-COVER counter; selector constructor specs cover valid and invalid BETWEEN-END values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "SELECTOR-CATALOG-ENTRY.BASIS"
   :file "packages/core/kernel/src/domain/selector.lisp" :lines (88)
   :reason "This DEFSTRUCT-generated slot type-check branch has no source-mapped SB-COVER counter; selector catalog specs cover valid and invalid BASIS values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "FILE-PATCH.OLD-PATH"
   :file "packages/core/kernel/src/domain/unified-diff-patch.lisp" :lines (13)
   :reason "This DEFSTRUCT-generated slot type-check branch has no source-mapped SB-COVER counter; file patch constructor specs cover valid and invalid OLD-PATH values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "DIFF-LINE.OP"
   :file "packages/core/kernel/src/domain/unified-diff.lisp" :lines (25)
   :reason "This DEFSTRUCT-generated slot type-check branch has no source-mapped SB-COVER counter; diff-line constructor specs cover valid and invalid OP values.")
  (:kind :branch :package "AITOOLS.KERNEL.DOMAIN" :symbol "DIFF-HUNK.OLD-COUNT"
   :file "packages/core/kernel/src/domain/unified-diff.lisp" :lines (36)
   :reason "This DEFSTRUCT-generated slot type-check branch has no source-mapped SB-COVER counter; diff-hunk constructor specs cover valid and invalid OLD-COUNT values.")))

