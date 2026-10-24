(in-package #:aitools.fixture.roots)

(defun %command-helper ()
  (flet ((%unused-builder () (quote %string-key-target))) (%unused-builder)))

(defun %command-builder ()
  (%command-helper))

(defun %inspect-builder ()
  nil)

(defun %unused-builder ()
  nil)

(define-command "literal.command"
                (:command-builder #'%command-builder :schema nil))

(define-inspect-command "literal.inspect" nil '%inspect-builder nil)
