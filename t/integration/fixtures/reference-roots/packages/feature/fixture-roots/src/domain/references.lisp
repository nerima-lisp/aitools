(in-package #:aitools.fixture.roots)

(defvar *fixture-lock* nil)

(defun %loop-body-helper (model)
  (declare (ignore model))
  nil)

(defun %constructor-default-fallback ()
  :fallback)

(defstruct (%constructor-default-holder
            (:constructor %make-constructor-default-holder
                (&optional (value #'%constructor-default-fallback)))
            (:copier nil)
            (:predicate nil))
  value)
