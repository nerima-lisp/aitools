;;;; packages/feature/util/src/domain/calc.lisp
;;;;
;;;; `util calc`: an arithmetic-only expression language (it has
;;;; arithmetic expressions and nothing else). The expression is
;;;; untrusted input, so it is never handed to the Lisp reader or evaluator:
;;;; a hand-written tokenizer accepts only ASCII digits, `.`, the operators
;;;; `+ - * / % **`, parentheses, commas, ASCII whitespace, and the six
;;;; function names in `data/domain/util/util-tables-data.lisp`. The parser
;;;; calls fixed Lisp functions directly. It has no variables or assignment,
;;;; and users cannot define or name another function.
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
                     (%calc-fail
                      index
                      (concatenate 'string
                                   "unknown name; only min, max, abs, floor, ceil, and round are "
                                   "available (no variables)")))
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
