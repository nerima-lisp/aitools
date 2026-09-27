;;;; Fixture: a core context's application must never reach into a feature
;;;; context.
(in-package #:aitools.fixture.application)

(defun bad-call ()
  (aitools.search.application:do-search "x"))
