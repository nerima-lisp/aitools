(in-package #:cl-user)

(defpackage #:aitools.integration.cps-contract-test (:use #:cl) (:shadowing-import-from #:cl-weave #:describe) (:import-from #:cl-weave #:it #:expect))

(in-package #:aitools.integration.cps-contract-test)

(defvar *reader-package* (or (find-package "AITOOLS.CPS.CONTRACT.READER") (make-package "AITOOLS.CPS.CONTRACT.READER" :use '(#:cl))))

(defun %reader-conditional-form (stream sub-character argument)
  (declare (ignore sub-character argument))
  (let ((*read-eval* nil))
    (read stream t nil t)
    (read stream t nil t)))

(defun %reader-eval-form (stream sub-character argument)
  (declare (ignore sub-character argument))
  (let ((*read-eval* nil))
    (read stream t nil t)))

(defun %source-forms (text)
  (with-standard-io-syntax
    (let ((*package* *reader-package*)
          (*read-eval* nil)
          (*readtable* (copy-readtable nil)))
      (set-dispatch-macro-character #\# #\+ (function %reader-conditional-form))
      (set-dispatch-macro-character #\# #\- (function %reader-conditional-form))
      (set-dispatch-macro-character #\# #\. (function %reader-eval-form))
      (with-input-from-string (stream text)
        (loop for form = (read stream nil stream)
              until (eq form stream)
              collect form
              do (when (%operator-name-p form "IN-PACKAGE")
                   (setf *package* (or (find-package (string (second form))) *reader-package*))))))))

(defun %operator-name-p (form name)
  (and (consp form)
       (symbolp (first form))
       (string-equal (symbol-name (first form)) name)
       (or (not (member name (list "IN-PACKAGE" "DEFUN")
                        :test (function string-equal)))
           (eq (symbol-package (first form))
               (find-package "COMMON-LISP")))))
