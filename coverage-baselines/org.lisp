(:context :org :state :measured :source :linux-ci
 :expression-covered 3884 :expression-total 4281
 :branch-covered 253 :branch-total 324
 :coverage-exclusions
 ((:kind :expression
   :package "AITOOLS.ORG.DOMAIN"
   :symbol "ORG-QUERY-MATCH"
   :reason "SB-COVER does not attribute generated DEFSTRUCT typed-slot constructor guard expressions; query-match specs reject invalid :tags and :heading-path values.")
  (:kind :branch
   :package "AITOOLS.ORG.DOMAIN"
   :symbol "ORG-QUERY-MATCH"
   :reason "SB-COVER does not attribute the generated DEFSTRUCT typed-slot constructor guard for :todo; query-match specs reject a non-string value.")
  (:kind :branch
   :package "AITOOLS.ORG.DOMAIN"
   :symbol "ORG-FAILURE"
   :reason "SB-COVER does not attribute the generated DEFSTRUCT typed-slot constructor guard for :line; failure specs reject a non-integer value.")))
