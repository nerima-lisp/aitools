(defpackage #:aitools.fixture.import-library-live
  (:use #:cl)
  (:export #:library-call #:library-setf-place))
(in-package #:aitools.fixture.import-library-live)

(defun library-call () nil)
(defun (setf library-setf-place) (new-value place)
  (declare (ignore place))
  new-value)
