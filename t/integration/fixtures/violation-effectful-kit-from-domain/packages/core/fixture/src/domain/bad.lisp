;;;; Fixture: a domain file must never call an effectful kit directly.
(in-package #:aitools.fixture.domain)

(defun bad-read (path)
  (host-kit:read-file path))
