(in-package #:aitools.fixture.roots)

(defmethod external-generic ((value t))
  (%method-helper value))

(defun %method-helper (value)
  value)

(defun %unused-method-helper ()
  nil)
