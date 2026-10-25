(defpackage #:aitools.fixture.live (:use #:cl) (:export #:entry))

(defun entry ()
  (helper)
  (cl:find-symbol "ENTRY" "AITOOLS.FIXTURE.LIVE"))

(defun helper () nil)
