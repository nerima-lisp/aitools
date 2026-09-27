;;;; Fixture: a domain file that puts its definitions in another layer.
;;;; Its qualified references are fine; only the in-package is wrong.
(in-package #:aitools.fixture.application)

(defun f () (aitools.kernel.domain:sha256-hex #()))
