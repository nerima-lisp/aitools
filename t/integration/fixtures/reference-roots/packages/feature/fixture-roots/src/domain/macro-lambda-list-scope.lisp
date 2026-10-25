(in-package #:aitools.fixture.roots)

(defvar on-error nil)

(defmacro with-error-values ((on-ok on-error) &body body)
  `(progn
     ,on-ok
     ,on-error
     ,@body))

(defun use-error-values ()
  (with-error-values (nil nil) nil))
