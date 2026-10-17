;;;; t/support/envelope-matchers.lisp
(in-package #:aitools.test.support)

(defun %envelope-mismatch (actual expected)
  (loop for (key value) on expected by #'cddr
        for actual-value = (envelope-value actual key)
        unless (equalp actual-value value)
          return (list :key key :expected value :actual actual-value)))

(cl-weave:defmatcher :to-match-envelope (actual expected)
  "Passes when EXPECTED's envelope fields match ACTUAL."
  (let ((mismatch (%envelope-mismatch actual expected)))
    (values (null mismatch)
            (or mismatch (list :fields expected)))))
