(:context :org :state :measured :source :linux-ci
 :expression-covered 4305 :expression-total 4305
 :branch-covered 308 :branch-total 308
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
