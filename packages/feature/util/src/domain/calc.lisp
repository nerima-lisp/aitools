;;;; packages/feature/util/src/domain/calc.lisp
;;;;
;;;; `util calc`: an arithmetic-only expression language (it has
;;;; arithmetic expressions and nothing else). The expression is
;;;; untrusted input, so it is never handed to the Lisp reader or evaluator:
;;;; a hand-written tokenizer accepts only ASCII digits, `.`, the operators
;;;; `+ - * / % **`, parentheses, commas, ASCII whitespace, and the six
;;;; function names below, and a recursive-descent parser calls fixed Lisp
;;;; functions directly. There are no variables, no assignment, and no way to
;;;; define or name any other function.
;;;;
;;;; Budgets, each enforced before the work it bounds:
;;;; - input length, before tokenizing;
;;;; - nesting depth (parentheses and chained unary signs), during parsing,
;;;;   so recursion cannot exhaust the control stack;
;;;; - result size: every intermediate value's numerator and denominator
;;;;   must fit +CALC-MAX-BITS+. For `**`, |exponent| * (bits(base) - 1) is a
;;;;   lower bound on the result size; EXPT runs only when that bound fits,
;;;;   because a single `9**9**9` would otherwise allocate gigabytes before
;;;;   any check could see it. A result that passes the bound is at most
;;;;   twice the budget, as is the result of any other operator on
;;;;   in-budget operands, so checking results afterwards stays bounded.
;;;;
;;;; Semantics: integers are arbitrary precision; decimal literals and `/`
;;;; produce exact rationals; `%` is MOD (the result takes the divisor's
;;;; sign); `**` binds tighter than unary minus and associates to the right
;;;; (`-2**2` is -4, `2**3**2` is 512) and requires an integer exponent;
;;;; `round` rounds half away from zero.
(in-package #:aitools.util.domain)

(defconstant +calc-max-input-length+ 4096)
(defconstant +calc-max-depth+ 64)
(defconstant +calc-max-bits+ 65536)
(defconstant +calc-max-decimals+ 1000)

(defparameter +calc-functions+ aitools.data:*util-calc-functions*)

(define-condition %calc-error (error)
  ((offset :initarg :offset :reader %calc-error-offset)
   (reason :initarg :reason :reader %calc-error-reason)))

(defun %calc-fail (offset reason)
  (error '%calc-error :offset offset :reason reason))

(declaim (inline %ascii-digit-p %ascii-letter-p))

(defun %ascii-digit-p (char)
  (char<= #\0 char #\9))

(defun %ascii-letter-p (char)
  (or (char<= #\a char #\z) (char<= #\A char #\Z) (char= char #\_)))

;;; ------------------------------------------------------------- tokenizer

(defstruct (%token (:constructor %make-token (kind value offset)) (:copier nil))
  (kind nil :type keyword :read-only t)
  (value nil :read-only t)
  (offset 0 :type fixnum :read-only t))

(defun %scan-number (text start)
  "Return (VALUES RATIONAL END) for the decimal literal at START."
  (let* ((length (length text))
         (int-end (or (position-if-not #'%ascii-digit-p text :start start) length))
         (value (parse-integer text :start start :end int-end)))
    (if (and (< int-end length) (char= (char text int-end) #\.))
        (let ((frac-end (or (position-if-not #'%ascii-digit-p text :start (1+ int-end)) length)))
          (when (= frac-end (1+ int-end))
            (%calc-fail int-end "a decimal point must be followed by digits"))
          (values (+ value (/ (parse-integer text :start (1+ int-end) :end frac-end)
                              (expt 10 (- frac-end int-end 1))))
                  frac-end))
        (values value int-end))))

(defun %tokenize (text)
  (let ((tokens '()) (index 0) (length (length text)))
    (loop while (< index length)
          do (let ((char (char text index)))
               (cond
                 ((member char '(#\Space #\Tab #\Newline #\Return)) (incf index))
                 ((%ascii-digit-p char)
                  (multiple-value-bind (value end) (%scan-number text index)
                    (push (%make-token :number value index) tokens)
                    (setf index end)))
                 ((%ascii-letter-p char)
                  (let* ((end (or (position-if-not (lambda (c) (or (%ascii-letter-p c) (%ascii-digit-p c)))
                                                   text :start index)
                                  length))
                         (name (string-downcase (subseq text index end))))
                    (unless (assoc name +calc-functions+ :test #'string=)
                      (%calc-fail index "unknown name; only min, max, abs, floor, ceil, and round are available (no variables)"))
                    (push (%make-token :name name index) tokens)
                    (setf index end)))
                 ((and (char= char #\*) (< (1+ index) length) (char= (char text (1+ index)) #\*))
                  (push (%make-token :op "**" index) tokens)
                  (incf index 2))
                 ((find char "+-*/%")
                  (push (%make-token :op (string char) index) tokens)
                  (incf index))
                 ((char= char #\() (push (%make-token :open nil index) tokens) (incf index))
                 ((char= char #\)) (push (%make-token :close nil index) tokens) (incf index))
                 ((char= char #\,) (push (%make-token :comma nil index) tokens) (incf index))
                 ((char= char #\.) (%calc-fail index "a decimal literal must start with a digit"))
                 ((char= char #\=) (%calc-fail index "assignment is not supported"))
                 (t (%calc-fail index "unexpected character")))))
    (push (%make-token :end nil length) tokens)
    (coerce (nreverse tokens) 'simple-vector)))

;;; ---------------------------------------------------------- arithmetic

(defun %value-bits (value)
  (max (integer-length (abs (numerator value))) (integer-length (denominator value))))

(defun %checked (value offset)
  (when (> (%value-bits value) +calc-max-bits+)
    (%calc-fail offset (format nil "result exceeds ~D bits" +calc-max-bits+)))
  value)

(defun %round-half-away (value)
  (if (minusp value)
      (- (floor (+ (- value) 1/2)))
      (floor (+ value 1/2))))

(defun %power (base exponent offset)
  (unless (integerp exponent)
    (%calc-fail offset "the exponent of ** must be an integer"))
  (cond
    ((and (zerop base) (minusp exponent)) (%calc-fail offset "division by zero"))
    ((member base '(0 1 -1)) (expt base exponent))
    ((> (* (abs exponent) (1- (%value-bits base))) +calc-max-bits+)
     (%calc-fail offset (format nil "result exceeds ~D bits" +calc-max-bits+)))
    (t (expt base exponent))))

(defun %apply-binary (op left right offset)
  (%checked
   (cond ((string= op "+") (+ left right))
         ((string= op "-") (- left right))
         ((string= op "*") (* left right))
         ((string= op "/")
          (when (zerop right) (%calc-fail offset "division by zero"))
          (/ left right))
         ((string= op "%")
          (when (zerop right) (%calc-fail offset "division by zero"))
          (mod left right))
         ((string= op "**") (%power left right offset)))
   offset))

;;; -------------------------------------------------------------- parser

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
                   (if (eq (%token-kind token) :end) "unexpected end of expression" "expected a number, '(' or a function"))))))

(defun evaluate-expression/k (text &key on-value on-error)
  "Evaluate the arithmetic expression TEXT. Calls ON-VALUE with the exact
rational result, or ON-ERROR with (OFFSET REASON) for a syntax error,
division by zero, or an exceeded budget; OFFSET is a character index into
TEXT."
  (declare (type function on-value on-error))
  (let ((value (handler-case
                   (progn
                     (when (> (length text) +calc-max-input-length+)
                       (%calc-fail +calc-max-input-length+
                                   (format nil "expression exceeds ~D characters" +calc-max-input-length+)))
                     (let ((parser (%make-parser (%tokenize text))))
                       (when (eq (%token-kind (%peek parser)) :end)
                         (%calc-fail 0 "empty expression"))
                       (prog1 (%parse-additive parser)
                         (let ((rest (%peek parser)))
                           (unless (eq (%token-kind rest) :end)
                             (%calc-fail (%token-offset rest) "unexpected trailing input"))))))
                 (%calc-error (condition)
                   (return-from evaluate-expression/k
                     (funcall on-error (%calc-error-offset condition) (%calc-error-reason condition)))))))
    (funcall on-value value)))

;;; ------------------------------------------------------------ rendering

(defun format-decimal (value decimals)
  "Render the rational VALUE in decimal, rounded half away from zero to
DECIMALS fractional digits, with trailing zeros removed."
  (declare (type (integer 0) decimals))
  (let* ((scale (expt 10 decimals))
         (scaled (%round-half-away (* value scale))))
    (multiple-value-bind (whole fraction) (truncate (abs scaled) scale)
      (let ((digits (if (zerop fraction)
                        ""
                        (string-right-trim "0" (format nil "~V,'0D" decimals fraction)))))
        (format nil "~:[~;-~]~D~:[.~A~;~*~]" (minusp scaled) whole (zerop fraction) digits)))))

(defun format-exact (value)
  "\"N/D\" for a non-integer rational VALUE, NIL for an integer."
  (unless (integerp value)
    (format nil "~D/~D" (numerator value) (denominator value))))
