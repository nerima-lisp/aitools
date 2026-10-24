(defpackage #:aitools.fixture.use-consumer
  (:use #:cl
        #:aitools.fixture.use-library)
  (:export #:consumer-entry))

(defun consumer-entry ()
  (aitools.fixture.use-library:library-call)
  (cl:find-symbol "CONSUMER-ENTRY" "AITOOLS.FIXTURE.USE-CONSUMER"))
