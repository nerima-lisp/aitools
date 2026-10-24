(defpackage #:aitools.fixture.import-library
  (:use #:cl)
  (:export #:library-call #:library-shadowed-call))

(defun library-call () nil)
(defun library-shadowed-call () nil)
