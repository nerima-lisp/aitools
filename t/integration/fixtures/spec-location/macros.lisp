(defpackage #:aitools.test.spec-location-fixture
  (:use #:cl)
  (:export #:define-spec))

(in-package #:aitools.test.spec-location-fixture)

(defmacro inner-spec (name)
  `(cl-weave:it ,name (values)))

(defmacro define-spec (name)
  `(inner-spec ,name))
