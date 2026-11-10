(:context :text
 :state :measured
 :source :linux-ci
 :expression-covered 4314
 :expression-total 4384
 :branch-covered 476
 :branch-total 516
 :coverage-exclusions
 ((:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "%ARCHIVE-PATH-OCTETS-PROBLEM"
   :reason "The :TOO-DEEP returns are unreachable after the 4095-octet path cap: 2049 nonempty, non-dot one-byte components require at least 4097 octets. Specs accept 2048 components at the limit and reject the path that exceeds both limits.")
  (:kind :branch
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "%ARCHIVE-PATH-OCTETS-PROBLEM"
   :reason "The depth-limit branches are unreachable: a 2049th nonempty, non-dot one-byte component needs at least 4097 path octets, beyond the 4095-octet cap.")
  (:kind :branch
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "ARCHIVE-ENTRY"
   :reason "SB-COVER reports generated slot type-check branches for the private constructor; archive readers construct entries only from validated header metadata.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "LANGUAGE"
   :reason "SB-COVER reports generated defaults for the private constructor; the curated language table supplies every slot explicitly at load time.")
  (:kind :branch
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "LANGUAGE"
   :reason "SB-COVER reports generated slot type-check branches for the private constructor; the curated language table supplies only valid values at load time.")
  (:kind :branch
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "TEXT-LAYOUT"
   :reason "The private constructor line-ending type check receives only detector output; callers cannot supply arbitrary values through the public API.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "WRITE-TAR"
   :reason "The PAX size path requires a simple octet array of at least 8^11 bytes, which is impractical to materialize in the test environment.")
  (:kind :branch
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "WRITE-TAR"
   :reason "The PAX size threshold requires a simple octet array of at least 8^11 bytes, which is impractical to materialize in the test environment.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "WRITE-ZIP"
   :reason "The ZIP32 size paths require in-memory data or an archive of at least #xFFFFFFFF bytes, which is impractical to materialize in the test environment.")
  (:kind :branch
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "WRITE-ZIP"
   :reason "The ZIP32 size thresholds require in-memory data or an archive of at least #xFFFFFFFF bytes, which is impractical to materialize in the test environment.")))
