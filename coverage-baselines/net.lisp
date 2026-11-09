(:context :net
 :state :measured
 :source :linux-ci
 :expression-covered 2243
 :expression-total 2243
 :branch-covered 280
 :branch-total 280
 :coverage-exclusions
 ((:kind :expression :package "AITOOLS.NET.APPLICATION" :symbol "%WITH-NET-VALIDATION"
   :reason "The macro template is consumed during ASDF source compilation, so SB-COVER cannot record expansion-time evaluation as a runtime call; generated checks are covered by rejects invalid GET URLs, timeouts, and line limits before sending and rejects invalid hosts and NUL before calling the resolver.")
  (:kind :expression :package "AITOOLS.NET.APPLICATION" :symbol "%WITH-NET-ERROR-CONTINUATION"
   :reason "The macro template is consumed during ASDF source compilation, so SB-COVER cannot record expansion-time evaluation as a runtime call; generated error mapping is covered by preserves net-error code and detail for GET and HEAD and hides generic sender errors for GET and HEAD.")
  (:kind :expression :package "AITOOLS.NET.APPLICATION" :symbol "GET/K"
   :reason "SB-COVER does not attribute lambda-list initialization forms; the default GET timeout is exercised by returns a 5xx status and decodes UTF-8 lines after a GET, and default max-lines by uses the default 80-line window and parses open-ended ranges.")
  (:kind :expression :package "AITOOLS.NET.APPLICATION" :symbol "HEAD/K"
   :reason "SB-COVER does not attribute lambda-list initialization forms; the default HEAD timeout is exercised by uses the default timeout for HEAD requests.")
  (:kind :expression :package "AITOOLS.NET.APPLICATION" :symbol "RESOLVE/K"
   :reason "SB-COVER does not attribute lambda-list initialization forms; the default resolver timeout is exercised by returns empty records for NODATA and uses the default timeout.")
  (:kind :expression :package "AITOOLS.NET.INFRASTRUCTURE" :symbol "*HTTP-CLIENT-SEND-FUNCTION*"
   :reason "The DEFVAR initializer runs on first system load before coverage counters reset, and DEFVAR does not reinitialize the bound variable on later loads; requester behavior through the sender seam is covered by uses one finite deadline and bounded, unauthenticated decoding.")))
