;;;; Fixture: a package no layer list names is a violation, not ignored.
(in-package #:aitools.fixture.domain)

(defun bad-read (path)
  (uiop:read-file-string path))
