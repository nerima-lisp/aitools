;;;; t/support/envelope-matchers.lisp
(in-package #:aitools.test.support)

(defun %envelope-mismatch (actual expected)
  (let ((version (envelope-value actual "schema_version")))
    (unless (eql version 2)
      (return-from %envelope-mismatch
        (list :key "schema_version" :expected 2 :actual version))))
  (loop for (key value) on expected by #'cddr
        for actual-value = (envelope-value actual key)
        unless (equalp actual-value value)
          return (list :key key :expected value :actual actual-value)))

(cl-weave:defmatcher :to-match-envelope (actual expected)
  "Passes when ACTUAL is a version 2 envelope and EXPECTED fields match."
  (let ((mismatch (%envelope-mismatch actual expected)))
    (values (null mismatch)
            (or mismatch (list :fields expected)))))
