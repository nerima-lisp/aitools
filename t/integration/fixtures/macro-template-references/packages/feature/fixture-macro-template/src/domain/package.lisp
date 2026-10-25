(defpackage #:aitools.fixture.macro-template (:use #:cl) (:export #:entry))

(defmacro entry ()
  `(progn
     (%template-helper)
     #'%template-function-designator
     (setf (template-record-value nil) nil)
     '(#'(setf template-record-designator))
     `(,(%nested-template-helper))))

(defun %template-helper () nil)
(defun %nested-template-helper () nil)
(defun %unused-template-helper () nil)
(defun %template-function-designator () nil)
(defstruct (template-record (:constructor nil) (:copier nil) (:predicate nil)) value designator)
