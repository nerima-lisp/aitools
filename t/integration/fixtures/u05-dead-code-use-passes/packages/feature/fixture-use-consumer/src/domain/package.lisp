(defpackage #:aitools.fixture.use-consumer-live
  (:use #:cl
        #:aitools.fixture.use-library-live)
  (:export #:consumer-entry))

(defun consumer-entry ()
  (library-call)
  (cl:find-symbol "CONSUMER-ENTRY" "AITOOLS.FIXTURE.USE-CONSUMER-LIVE"))
