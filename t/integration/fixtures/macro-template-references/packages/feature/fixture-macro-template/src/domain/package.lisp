(defpackage #:aitools.fixture.macro-template (:use #:cl) (:export #:entry))

(defmacro entry ()
  `(progn
     (%template-helper)
     `(,(%nested-template-helper))))

(defun %template-helper () nil)
(defun %nested-template-helper () nil)
(defun %unused-template-helper () nil)
