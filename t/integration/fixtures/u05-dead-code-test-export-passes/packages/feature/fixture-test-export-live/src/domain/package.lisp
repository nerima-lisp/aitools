(defpackage #:aitools.fixture.test-export-live
  (:use #:cl)
  (:export #:public-entry #:production-user))

(defun public-entry () nil)

(defun production-user ()
  (public-entry)
  (cl:find-symbol "PRODUCTION-USER" "AITOOLS.FIXTURE.TEST-EXPORT-LIVE"))
