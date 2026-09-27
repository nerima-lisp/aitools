;;;; Fixture: an application layer must go through another context's
;;;; application boundary, never straight into its domain -- and never into
;;;; a feature context's domain at all, even from another feature context.
(in-package #:aitools.fixture-feature.application)

(defun bad-call ()
  (aitools.search.domain:something))
