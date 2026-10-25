(defpackage #:aitools.fixture.setf-fail (:use #:cl))

(in-package #:aitools.fixture.setf-fail)

(defun (setf %dead-setf-place) (new-value place)
  (declare (ignore place))
  new-value)
