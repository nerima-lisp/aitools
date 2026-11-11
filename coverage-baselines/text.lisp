(:context :text
 :state :measured
 :source :linux-ci
 :expression-covered 4695
 :expression-total 4695
 :branch-covered 556
 :branch-total 556
 :coverage-exclusions
 ((:kind :branch
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "ARCHIVE-ENTRY"
   :reason "SB-COVER cannot map compiler-generated DEFSTRUCT slot type guards for this private constructor; t/unit/text/archive-safety-test.lisp exercises the :SIZE guard with a negative archive-entry size.")
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
   :reason "SB-COVER cannot map generated DEFSTRUCT default initializers for this private constructor; the omitted slots are exercised by the LANGUAGE spec in t/integration/edit-text-test.lisp lines 211-214.")
  (:kind :branch
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "LANGUAGE"
   :reason "SB-COVER cannot map compiler-generated DEFSTRUCT slot type guards for this private constructor; t/unit/text/language-test.lisp exercises the :EXTENT guard with :BAD.")
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
   :symbol "TEXT-LAYOUT"
   :reason "SB-COVER cannot map compiler-generated DEFSTRUCT slot type guards; t/unit/text/layout-test.lisp exercises the :LINE-ENDING guard with :BAD.")
  (:kind :expression
   :package "AITOOLS.TEXT.DOMAIN"
   :symbol "UTF8-VALID-P"
   :reason "SB-COVER leaves this lambda-list default initializer uncovered; runtime specs exercise this function default behavior.")))
