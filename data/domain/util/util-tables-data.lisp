;;;; data/domain/util/util-tables-data.lisp
;;;;
;;;; The closed vocabularies of `util`: codec scheme
;;;; names, `util random` alphabets, and the only function names `util calc`
;;;; accepts. The calc entries name the Lisp function each call dispatches
;;;; to, so the set of code an expression can reach is fixed here.
(in-package #:aitools.data)

(defparameter *util-codec-schemes* '("base64" "url" "hex"))

(defparameter *util-random-alphabets*
  '(("hex" . "0123456789abcdef")
    ("alnum" . "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
    ("base64url" . "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"))
  "(NAME . CHARACTERS), in `--alphabet` choice order; the first is the default.")

(defparameter *util-calc-functions*
  '(("min" :variadic cl:min)
    ("max" :variadic cl:max)
    ("abs" 1 cl:abs)
    ("floor" 1 cl:floor)
    ("ceil" 1 cl:ceiling)
    ("round" 1 :round-half-away-from-zero))
  "(NAME ARITY FUNCTION). FUNCTION is a CL function symbol, or a keyword the
calc evaluator maps to its own implementation.")

(export '(*util-codec-schemes* *util-random-alphabets* *util-calc-functions*))
