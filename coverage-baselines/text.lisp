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
  (:kind :branch
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "ARCHIVE-MEMBER"
   :reason "SB-COVER reports generated slot type-check branches for this public defstruct constructor; the constructor spec checks valid and invalid kind, mode, and mtime values.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "BINARY-OCTETS-P"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "BUILD-LINE-INDEX"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "COUNT-LINES"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "CRC32"
   :reason "SB-COVER leaves both lambda-list default initializers uncovered; runtime specs cover the default checksum and incremental :CRC behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "DECODE-OCTETS/K"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "DECODE-UTF8"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "DECODE-UTF8-STRICT/K"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "DEFLATE"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "ENCODE-UTF8"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "FINAL-NEWLINE-P"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "GZIP-COMPRESS"
   :reason "SB-COVER leaves the lambda-list default initializer uncovered; runtime specs exercise compression with default MTIME and named members.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "GZIP-MEMBER-HEADER"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "GUESS-ENCODING"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "INFLATE"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "LANGUAGE"
   :reason "SB-COVER reports generated defaults for the private constructor; the curated language table supplies every slot explicitly at load time.")
  (:kind :branch
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "LANGUAGE"
   :reason "SB-COVER reports generated slot type-check branches for the private constructor; the curated language table supplies only valid values at load time.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "LINE-ENDING-STYLE"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "MAP-LINES"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
  (:kind :branch
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "%PARSE-PAX-RECORD"
   :reason "The false arm of the mtime clause cannot be reached through production pax-key: earlier clauses consume path, linkpath, and size, and unknown keys are filtered to NIL. The true arm is exercised by the fractional-mtime spec.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "%PARSE-PAX-RECORD"
   :reason "The :TOO-DEEP path arm requires at least 4097 octets for 2049 components, beyond the 4095-octet path cap checked first.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "%TAR-STRING"
   :reason "The :TOO-DEEP path arm requires at least 4097 octets for 2049 components, beyond the 4095-octet path cap checked first.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "READ-ZIP-ENTRIES"
   :reason "The :TOO-DEEP path arm requires 4097 octets, beyond the 4095-octet cap; ZIP symlink targets are capped at 4095 bytes before path validation, so :PATH-TOO-LONG is unreachable.")
  (:kind :branch
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "TEXT-LAYOUT"
   :reason "The private constructor line-ending type check receives only detector output; callers cannot supply arbitrary values through the public API.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "UTF8-VALID-P"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")
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
