(:context :workspace :state :measured :source :linux-ci
 :expression-covered 3759 :expression-total 3963
 :branch-covered 500 :branch-total 578
 :coverage-exclusions
 ((:kind :expression
   :package "AITOOLS.WORKSPACE.APPLICATION"
   :symbol "IGNORE-CONTEXT"
   :reason "This is the BASE-STACK DEFSTRUCT initform. The default constructor is exercised, but SB-COVER does not mark generated DEFSTRUCT declaration-site defaults.")
  (:kind :expression
   :package "AITOOLS.WORKSPACE.APPLICATION"
   :symbol "CALL-WITH-WORKSPACE-SCAN/K"
   :reason "This is the SKIP-LARGER-THAN keyword default, exercised by omitting the keyword; SB-COVER does not mark default expressions in the function lambda list.")
  (:kind :expression
   :package "AITOOLS.WORKSPACE.DOMAIN"
   :symbol "PARSE-IGNORE-LINES"
   :reason "This is the BASE keyword default, exercised by omitting the keyword; SB-COVER does not mark default expressions in the function lambda list.")
  (:kind :expression
   :package "AITOOLS.WORKSPACE.DOMAIN"
   :symbol "PARSE-IGNORE-OCTETS"
   :reason "This is the BASE keyword default, exercised by omitting the keyword; SB-COVER does not mark default expressions in the function lambda list.")
  (:kind :expression
   :package "AITOOLS.WORKSPACE.DOMAIN"
   :symbol "PARSE-GIT-INDEX-PATHS"
   :reason "The HASH-SIZE default is exercised by omitting the keyword, but SB-COVER does not mark default expressions in the function lambda list.")
  (:kind :branch
   :package "AITOOLS.WORKSPACE.APPLICATION"
   :symbol "IGNORE-CONTEXT"
   :reason "The spec passes an invalid SOURCE and observes TYPE-ERROR, but SB-COVER does not mark generated DEFSTRUCT slot type-check branches.")
  (:kind :branch
   :package "AITOOLS.WORKSPACE.APPLICATION"
   :symbol "WORKSPACE-OVERLAY"
   :reason "The spec passes an invalid LIST-DIRECTORY and observes TYPE-ERROR, but SB-COVER does not mark generated DEFSTRUCT slot type-check branches.")
  (:kind :branch
   :package "AITOOLS.WORKSPACE.DOMAIN"
   :symbol "WORKSPACE-ENTRY"
   :reason "Specs pass invalid KIND, SIZE, and MODE values and observe TYPE-ERROR, but SB-COVER does not mark generated DEFSTRUCT slot type-check branches.")
  (:kind :branch
   :package "AITOOLS.WORKSPACE.DOMAIN"
   :symbol "WORKSPACE-ROOT"
   :reason "The spec passes an invalid SOURCE and observes TYPE-ERROR, but SB-COVER does not mark generated DEFSTRUCT slot type-check branches.")))

