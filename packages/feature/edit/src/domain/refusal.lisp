;;;; packages/feature/edit/src/domain/refusal.lisp
;;;;
;;;; EDIT-REFUSAL is how a pure edit computation deep inside a value
;;;; transformation (a JSON Patch operation, a table cell, a template
;;;; filter's input) says "this write must not happen" together with the
;;;; spec error.code the flow reports. Flows catch it at their boundary and
;;;; call their error continuation; nothing has been written at that point.
(in-package #:aitools.edit.domain)

(define-condition edit-refusal (error)
  ((code :initarg :code :reader edit-refusal-code)
   (detail :initarg :detail :reader edit-refusal-detail))
  (:report (lambda (condition stream) (write-string (edit-refusal-detail condition) stream))))

(defun refuse (code control &rest arguments)
  (error 'edit-refusal :code code :detail (apply #'format nil control arguments)))
