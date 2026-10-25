(defpackage #:aitools.fixture.import-consumer-live
  (:use #:cl)
  (:import-from #:aitools.fixture.import-library-live
    #:library-call
    #:library-setf-place)
  (:export #:consumer-entry))
(in-package #:aitools.fixture.import-consumer-live)

(defun consumer-entry ()
  (cond ((library-call) t))
  (funcall #'(setf library-setf-place) nil :imported-place)
  (cl:find-symbol "CONSUMER-ENTRY" "AITOOLS.FIXTURE.IMPORT-CONSUMER-LIVE"))
