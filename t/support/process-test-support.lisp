(in-package #:cl-user)

(defpackage #:aitools.process.integration-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect #:describe-skip-if)
  (:import-from #:aitools.test.support #:with-temporary-directory))

(in-package #:aitools.process.integration-test)

(defun call-with-workspace (function)
  "Call FUNCTION with (ROOT STATE): a workspace directory (the working
directory the workspace host reports) and a sibling state directory."
  (with-temporary-directory (directory)
    (let ((root (merge-pathnames "ws/" directory))
          (state (merge-pathnames "state/" directory)))
      (ensure-directories-exist root)
      (ensure-directories-exist (merge-pathnames "tmp/" state))
      (funcall function root state))))

(defmacro with-process-workspace ((root state) &body body)
  `(call-with-workspace (lambda (,root ,state) (declare (ignorable ,state)) ,@body)))
