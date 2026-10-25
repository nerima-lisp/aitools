(defpackage #:aitools.fixture.test-export.tests
  (:use #:cl)
  (:import-from #:aitools.fixture.test-export
                 #:public-entry
                 #:test-setf-place))

(in-package #:aitools.fixture.test-export.tests)

(defun test-entry ()
  (public-entry)
  (funcall #'(setf test-setf-place) :test-value nil))
