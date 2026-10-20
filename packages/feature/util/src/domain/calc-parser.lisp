(in-package #:aitools.util.domain)

(defstruct (%parser (:constructor %make-parser (tokens)) (:copier nil))
  (tokens #() :type simple-vector :read-only t)
  (position 0 :type fixnum)
  (depth 0 :type fixnum))

(defun %peek (parser)
  (svref (%parser-tokens parser) (%parser-position parser)))

(defun %advance (parser)
  (prog1 (%peek parser) (incf (%parser-position parser))))

(defun %peek-op-p (parser &rest ops)
  (let ((token (%peek parser)))
    (and (eq (%token-kind token) :op) (member (%token-value token) ops :test #'string=))))

(defun %expect (parser kind reason)
  (let ((token (%peek parser)))
    (unless (eq (%token-kind token) kind) (%calc-fail (%token-offset token) reason))
    (%advance parser)))

(defmacro %with-depth ((parser offset) &body body)
  `(progn
     (when (> (incf (%parser-depth ,parser)) +calc-max-depth+)
       (%calc-fail ,offset (format nil "nesting exceeds ~D levels" +calc-max-depth+)))
     (multiple-value-prog1 (progn ,@body) (decf (%parser-depth ,parser)))))

(defun %parse-additive (parser)
  (let ((value (%parse-multiplicative parser)))
    (loop while (%peek-op-p parser "+" "-")
          do (let ((token (%advance parser)))
               (setf value (%apply-binary (%token-value token) value (%parse-multiplicative parser)
                                          (%token-offset token)))))
    value))

(defun %parse-multiplicative (parser)
  (let ((value (%parse-unary parser)))
    (loop while (%peek-op-p parser "*" "/" "%")
          do (let ((token (%advance parser)))
               (setf value (%apply-binary (%token-value token) value (%parse-unary parser)
                                          (%token-offset token)))))
    value))

(defun %parse-unary (parser)
  (if (%peek-op-p parser "+" "-")
      (let ((token (%advance parser)))
        (%with-depth (parser (%token-offset token))
          (let ((operand (%parse-unary parser)))
            (if (string= (%token-value token) "-") (- operand) operand))))
      (%parse-power parser)))

(defun %parse-power (parser)
  (let ((base (%parse-primary parser)))
    (if (%peek-op-p parser "**")
        (let ((token (%advance parser)))
          (%with-depth (parser (%token-offset token))
            (%apply-binary "**" base (%parse-unary parser) (%token-offset token))))
        base)))

(defun %parse-call (parser token)
  (destructuring-bind (arity function) (rest (assoc (%token-value token) +calc-functions+ :test #'string=))
    (%expect parser :open "a function name must be followed by '('")
    (let ((arguments (list (%parse-additive parser))))
      (loop while (eq (%token-kind (%peek parser)) :comma)
            do (%advance parser)
               (push (%parse-additive parser) arguments))
      (%expect parser :close "expected ')' after function arguments")
      (when (and (integerp arity) (/= arity (length arguments)))
        (%calc-fail (%token-offset token) (format nil "~A takes exactly ~D argument" (%token-value token) arity)))
      (%checked (apply (if (eq function :round-half-away-from-zero) #'%round-half-away function)
                       (nreverse arguments))
                (%token-offset token)))))

(defun %parse-primary (parser)
  (let ((token (%advance parser)))
    (ecase (%token-kind token)
      (:number (%checked (%token-value token) (%token-offset token)))
      (:name (%with-depth (parser (%token-offset token)) (%parse-call parser token)))
      (:open
       (%with-depth (parser (%token-offset token))
                    (prog1 (%parse-additive parser)
                      (%expect parser :close "expected ')'"))))
      ((:op :close :comma :end)
       (%calc-fail (%token-offset token)
                   (if (eq (%token-kind token) :end) "unexpected end of expression"
                     "expected a number, '(' or a function"))))))

(aitools.kernel.domain:define-flow
    (evaluate-expression text &key on-value on-error)
  "Evaluate the arithmetic expression TEXT. Calls ON-VALUE with the exact
rational result, or ON-ERROR with (OFFSET REASON) for a syntax error,
division by zero, or an exceeded budget; OFFSET is a character index into
TEXT."
  (declare (type function on-value on-error))
  (multiple-value-bind (success-p value offset reason)
      (handler-case
          (values
           t
           (progn
             (when (> (length text) +calc-max-input-length+)
               (%calc-fail +calc-max-input-length+
                           (format nil "expression exceeds ~D characters"
                                   +calc-max-input-length+)))
             (let ((parser (%make-parser (%tokenize text))))
               (when (eq (%token-kind (%peek parser)) :end)
                 (%calc-fail 0 "empty expression"))
               (prog1 (%parse-additive parser)
                 (let ((rest (%peek parser)))
                   (unless (eq (%token-kind rest) :end)
                     (%calc-fail (%token-offset rest) "unexpected trailing input"))))))
           nil
           nil)
        (%calc-error (condition)
          (values nil nil (%calc-error-offset condition)
                  (%calc-error-reason condition))))
    (if success-p
        (aitools.kernel.domain:flow-continue :on-value value)
        (aitools.kernel.domain:flow-continue :on-error offset reason))))
