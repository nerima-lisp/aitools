(defpackage #:aitools.fixture.import-consumer-live
  (:use #:cl)
  (:import-from #:aitools.fixture.import-library-live
    #:library-call)
  (:export #:consumer-entry))

(defun consumer-entry ()
  (cond ((library-call) t))
  (cl:find-symbol "CONSUMER-ENTRY" "AITOOLS.FIXTURE.IMPORT-CONSUMER-LIVE"))
