(defpackage #:aitools.fixture.test-export (:use #:cl) (:export #:public-entry #:test-setf-place))

(in-package #:aitools.fixture.test-export)

(defun public-entry () nil)

(defun (setf test-setf-place) (value place)
  (declare (ignore place))
  value)
