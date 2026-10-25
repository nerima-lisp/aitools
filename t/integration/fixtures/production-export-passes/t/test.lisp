(defpackage #:aitools.fixture.test-export-live.tests
  (:use #:cl)
  (:import-from #:aitools.fixture.test-export-live
    #:public-entry))

(defun test-entry () (public-entry))
