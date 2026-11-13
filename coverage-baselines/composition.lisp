(:context :composition
 :state :measured
 :source :linux-ci
 :expression-covered 2356
 :expression-total 2356
 :branch-covered 154
 :branch-total 154
 :coverage-exclusions
 ((:kind :expression
   :package "AITOOLS/CLI"
   :symbol "%WRITE-ERROR"
   :reason "SB-COVER attributes the :TRACE-P default to the DEFUN lambda list; the error-envelope spec calls %WRITE-ERROR without :TRACE-P, but that omitted-key default is not marked at the lambda-list source site.")))
