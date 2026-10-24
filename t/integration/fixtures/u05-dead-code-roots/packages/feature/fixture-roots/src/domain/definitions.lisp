(in-package #:aitools.fixture.roots)

(defmacro %unused-macro () `(progn ,(%quasiquote-reference-target) `(ignore ,%unused-generic)))
(defmacro %live-macro (name lambda-list &body body) `(defun ,name ,lambda-list ,@body))

(defvar *unused-variable* nil)
(defvar *live-variable* nil)

(defgeneric %unused-generic (value))
(defgeneric %live-generic (value))
(defmethod %live-generic ((value t)) value)

(defclass %unused-class () ())
(defclass %secondary-class-parent () ())
(defclass %declaration-type () ())
(defclass %the-only-type () ())
(defclass %live-class (%unused-class %secondary-class-parent) ())

(define-condition %unused-condition (condition) ())
(define-condition %live-condition (condition %unused-condition) ())
(define-condition %handler-only-condition (condition) ())

(defun %live-variable-user ()
  (declare (type (cons %unused-class %declaration-type) *live-variable*))
  (the %the-only-type *live-variable*)
  (multiple-value-bind (*live-variable* *unused-variable* local-second)
      (values *live-variable* nil nil)
    *unused-variable*))
(defun %live-macro-user () (%live-macro %generated-function () nil))
(defun %live-class-user () (make-instance '%live-class))
(defun %live-condition-user ()
  (handler-case
      (error '%live-condition)
    (%handler-only-condition (caught)
      (declare (ignorable caught))
      (if caught nil nil))))
(defun %dotted-unused () nil)
(defun find-symbol (name package)
  (values name package))
(defun %dotted-destructuring-local (pair)
  (destructuring-bind (%dotted-unused . tail) pair
    %dotted-unused))
(defclass %unused-negative-class () ())
(define-condition %unused-negative-condition (condition) ())
(defclass %declaim-only-type () ())
(defclass %check-type-only-type () ())
(declaim (type %declaim-only-type *declaim-target*))
(defun %check-type-type-user (object)
  (check-type object %check-type-only-type))
(defun %quasiquote-reference-target ()
  nil)

(defstruct %unused-generated-model)

(defstruct (%loop-generated-model
            (:constructor %make-loop-generated-model ())
            (:copier nil))
  (value nil :read-only t))

(defun %loop-generated-model-user ()
  (cl-concurrent-kit:with-lock-held (*fixture-lock*)
    (funcall (%constructor-default-holder-value
              (%make-constructor-default-holder)))
    (loop for model = (%make-loop-generated-model)
          do (%loop-generated-model-value model)
             (%loop-body-helper model))))
