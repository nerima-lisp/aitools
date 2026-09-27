;;;; Fixture: `::` into another context bypasses its public boundary.
(in-package #:aitools.fixture-feature.application)

(defun bad-call ()
  (aitools.kernel.domain::helper))
