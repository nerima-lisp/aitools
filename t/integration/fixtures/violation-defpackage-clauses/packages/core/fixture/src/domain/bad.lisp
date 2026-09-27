;;;; Fixture: a local nickname resolves to the package it names, so a
;;;; reference through it is checked against that package.
(in-package #:aitools.fixture.domain)

(defun ok ()
  (sha256-hex #()))

(defun bad-read (path)
  (list
   (hk:read-file path)))
