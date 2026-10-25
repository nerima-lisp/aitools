(defpackage #:aitools.fixture.use-library-live
  (:use #:cl)
  (:export #:library-call #:library-setf-place))
(in-package #:aitools.fixture.use-library-live)

(defun library-call () nil)
(defun (setf library-setf-place) (new-value place)
  (declare (ignore place))
  new-value)
