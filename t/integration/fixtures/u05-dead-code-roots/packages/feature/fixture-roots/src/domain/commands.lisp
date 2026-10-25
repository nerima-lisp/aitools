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

(progn
  (define-inspect-command "literal.inspect" nil (quote %inspect-builder) nil)
  (quote
   (define-command "quoted.command"
     (:command-builder (%quoted-command-builder) :schema nil)))
  (quote
   (|define-command| "escaped.quoted"
     (:command-builder (%escaped-command-builder) :schema nil)))
  (when t
    (aitools.protocol.application:define-command
     "dynamic.command"
     (:command-builder (%dynamic-command-builder)
      :schema nil))))
