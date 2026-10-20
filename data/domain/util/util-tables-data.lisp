(in-package #:aitools.data)

(define-data-table *util-codec-schemes* :strings ("base64" "url" "hex"))

(define-data-table *util-base64-alphabet* :alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(define-data-table *util-random-alphabets* :string-pairs
  (("hex" . "0123456789abcdef")
   ("alnum" . "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
   ("base64url" . "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"))
  "(NAME . CHARACTERS), in `--alphabet` choice order; the first is the default.")

;;;; Arithmetic operators are implemented directly by the parser.
(define-data-table *util-calc-functions* :calc-functions
  (("min" :variadic cl:min)
   ("max" :variadic cl:max)
   ("abs" 1 cl:abs)
   ("floor" 1 cl:floor)
   ("ceil" 1 cl:ceiling)
   ("round" 1 :round-half-away-from-zero))
  "(NAME ARITY FUNCTION). ARITY sets the parser's argument-count constraint: a
positive integer or :variadic (one or more arguments).
FUNCTION must be a bound COMMON-LISP function, not a macro or special
operator, so calc cannot dispatch to functions from other packages. The
rounding marker is :round-half-away-from-zero.")

(export '(*util-codec-schemes* *util-base64-alphabet* *util-random-alphabets* *util-calc-functions*))
