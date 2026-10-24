(defpackage #:aitools.fixture.test-export.tests (:use #:cl) (:import-from #:aitools.fixture.test-export #:public-entry))

(defun test-entry () (public-entry))
