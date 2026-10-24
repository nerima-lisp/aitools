(in-package #:aitools.fixture.roots)

(defun %calc-function ()
  (%live-variable-user)
  (%live-macro-user)
  (%live-class-user)
  (%live-condition-user)
  (%check-type-type-user nil))

(defun %string-key-target ()
  nil)

(defun %calc-function-neighbor ()
  nil)
(defun |duplicate-target| ()
  nil)
(defun |same-name-target| ()
  nil)
