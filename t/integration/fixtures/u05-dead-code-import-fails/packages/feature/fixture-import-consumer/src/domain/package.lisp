(defpackage #:aitools.fixture.import-consumer
  (:use #:cl)
  (:shadowing-import-from
   #:aitools.fixture.import-library
   #:library-shadowed-call)
  (:import-from
   #:aitools.fixture.import-library
   #:library-call)
  (:export #:consumer-entry))
(defun consumer-entry ()
  (aitools.fixture.import-library:library-call)
  (aitools.fixture.import-library:library-shadowed-call)
  (cl:find-symbol "CONSUMER-ENTRY" "AITOOLS.FIXTURE.IMPORT-CONSUMER"))
