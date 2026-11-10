(:context :store
 :state :measured
 :source :linux-ci
 :expression-covered 7607
 :expression-total 7836
 :branch-covered 685
 :branch-total 810
 :coverage-exclusions
 ((:kind :branch :package "AITOOLS.STORE.APPLICATION" :symbol "STORE-IO"
   :reason "These DEFSTRUCT guards are compiler-generated and have no source-mapped SB-COVER branches. No valid store input can supply a non-callable port; specs verify invalid values are rejected.")
  (:kind :branch :package "AITOOLS.STORE.APPLICATION" :symbol "%ARCHIVE-PREFIX-CURSOR"
   :reason "These DEFSTRUCT guards are compiler-generated and have no source-mapped SB-COVER branches. No valid cursor state can contain an invalid descriptor or identity; specs verify malformed constructor values are rejected.")
  (:kind :branch :package "AITOOLS.STORE.INFRASTRUCTURE" :symbol "%OPEN-SEARCH-ROOT"
   :reason "Linux uses O_DIRECTORY when opening the workspace root, so a successful descriptor is always a directory. No path input can reach the non-directory arm.")))
