(defpackage #:aitools.fixture.use-consumer-live
  (:use #:cl
        #:aitools.fixture.use-library-live)
  (:export #:consumer-entry))
(in-package #:aitools.fixture.use-consumer-live)

(defun consumer-entry ()
  (funcall #'(setf library-setf-place) nil :used-place)
  (cl:find-symbol "CONSUMER-ENTRY" "AITOOLS.FIXTURE.USE-CONSUMER-LIVE"))
