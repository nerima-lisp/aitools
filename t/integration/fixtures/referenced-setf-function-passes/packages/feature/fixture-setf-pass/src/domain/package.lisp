(defpackage #:aitools.fixture.setf-pass (:use #:cl) (:export #:entry))

(in-package #:aitools.fixture.setf-pass)

(defun (setf %live-setf-place) (new-value place)
  (declare (ignore place))
  new-value)

(defun %live-setf-place (place)
  (declare (ignore place))
  nil)

(defun entry (place) (%live-setf-place place) (setf (%live-setf-place place) :updated) (cl:find-symbol "ENTRY" "AITOOLS.FIXTURE.SETF-PASS"))
