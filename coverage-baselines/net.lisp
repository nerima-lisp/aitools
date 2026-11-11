(:context :net
 :state :measured
 :source :linux-ci
 :expression-covered 2243
 :expression-total 2243
 :branch-covered 280
 :branch-total 280
 :coverage-exclusions
 ((:kind :expression :package "AITOOLS.NET.APPLICATION" :symbol "GET/K"
   :reason "SB-COVER does not attribute lambda-list initialization forms; the default GET timeout is exercised by returns a 5xx status and decodes UTF-8 lines after a GET, and default max-lines by uses the default 80-line window and parses open-ended ranges.")
  (:kind :expression :package "AITOOLS.NET.APPLICATION" :symbol "HEAD/K"
   :reason "SB-COVER does not attribute lambda-list initialization forms; the default HEAD timeout is exercised by uses the default timeout for HEAD requests.")
  (:kind :expression :package "AITOOLS.NET.APPLICATION" :symbol "RESOLVE/K"
   :reason "SB-COVER does not attribute lambda-list initialization forms; the default resolver timeout is exercised by returns empty records for NODATA and uses the default timeout.")
  (:kind :expression :package "AITOOLS.NET.INFRASTRUCTURE" :symbol "*HTTP-CLIENT-SEND-FUNCTION*"
   :reason "The DEFVAR initializer runs on first system load before coverage counters reset, and DEFVAR does not reinitialize the bound variable on later loads; requester behavior through the sender seam is covered by uses one finite deadline and bounded, unauthenticated decoding.")))
