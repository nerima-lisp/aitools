(in-package #:aitools.integration.structure-test)

(defun %liveness-reference-setf-function-name-p (designator)
  (and (consp designator)
       (null (cddr designator))
       (symbolp (first designator))
       (string-equal (%liveness-symbol-name (first designator)) "SETF")
       (symbolp (second designator))
       (not (keywordp (second designator)))))

(defun %liveness-reference-function-name (designator)
  (cond
    ((%liveness-reference-setf-function-name-p designator)
     (format nil "(SETF ~A)"
             (%liveness-reference-name-of (second designator))))
    ((symbolp designator)
     (%liveness-reference-name-of designator))))

(defun %liveness-reference-function-symbol (designator)
  (cond
    ((%liveness-reference-setf-function-name-p designator)
     (second designator))
    ((and (symbolp designator) (not (keywordp designator))) designator)))

(defun %liveness-emit-function-reference (designator)
  (let ((name (%liveness-reference-function-name designator))
        (symbol (%liveness-reference-function-symbol designator)))
    (when (and name symbol)
      (if (%liveness-reference-setf-function-name-p designator)
          (push (list :function name (%liveness-reference-package-of symbol))
                *liveness-reference-collector*)
          (%liveness-emit-reference :function symbol)))))

(defun %liveness-reference-function-bound-p (designator bindings)
  (let ((name (%liveness-reference-function-name designator))
        (symbol (%liveness-reference-function-symbol designator)))
    (and name symbol
         (eq (%liveness-reference-package-of symbol) :local)
         (member name bindings :test #'equal))))

(defun %liveness-walk-definition-form (object operator values functions)
  (cond
    ((equal operator "LAMBDA")
     (let ((locals (%liveness-walk-lambda-vars (second object) values
                                                functions nil)))
       (%liveness-walk-sequence (cddr object) (append values locals)
                                functions))
     t)
    ((member operator '("DEFUN" "DEFMACRO" "DEFGENERIC") :test #'equal)
     (let* ((function-name (second object))
            (locals (%liveness-walk-lambda-vars (third object) values
                                                functions nil)))
       (%liveness-walk-sequence (cdddr object) (append values locals)
                                (cons (%liveness-reference-function-name
                                       function-name)
                                      functions)))
     t)
    ((equal operator "DEFMETHOD")
     (let* ((generic (second object))
            (lambda-list (first (cddr object)))
            (locals (%liveness-walk-lambda-vars lambda-list values
                                                functions t)))
       (%liveness-emit-function-reference generic)
       (%liveness-walk-sequence (cdddr object) (append values locals) functions))
     t)
    ((member operator '("DEFCLASS" "DEFINE-CONDITION") :test #'equal)
     (%liveness-walk-class object values functions)
     t)
    ((equal operator "DEFSTRUCT")
     (%liveness-walk-struct object values functions)
     t)
    ((equal operator "DEFINE-FLOW")
     (%liveness-emit-reference :function (first object))
     (let* ((signature (second object))
            (function-name (and (consp signature) (first signature)))
            (locals (%liveness-walk-lambda-vars (rest signature) values
                                                functions nil)))
       (%liveness-walk-sequence (cddr object) (append values locals)
                                (cons (%liveness-reference-function-name
                                       function-name)
                                      functions)))
     t)
    ((equal operator "DEFINE-WRITE-COMMAND")
     (%liveness-emit-reference :function (first object))
     (let ((locals (%liveness-walk-lambda-vars (third object) values
                                               functions nil)))
       (%liveness-walk-sequence (cdddr object) (append values locals)
                                functions))
     t)
    ((equal operator "DEFINE-INSPECT-COMMAND")
     (%liveness-emit-reference :function (first object))
     (let ((builder (fourth object)))
       (when (and (consp builder)
                  (equal (%liveness-reference-name-of (first builder)) "QUOTE"))
         (%liveness-emit-function-reference (second builder)))
       (%liveness-walk-sequence (rest object) values functions))
     t)
    (t nil)))

