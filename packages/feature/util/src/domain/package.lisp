;;;; packages/feature/util/src/domain/package.lisp
;;;;
;;;; Pure value computations behind `util`: byte codecs,
;;;; text statistics, the arithmetic-only calculator, UUID layouts, and
;;;; alphabet sampling. Randomness and time arrive as arguments (bytes, a
;;;; byte supplier, a millisecond timestamp); nothing here performs I/O.
(in-package #:cl-user)

(defpackage #:aitools.util.domain
  (:use #:cl)
  (:export
   ;; codec.lisp
   #:+codec-schemes+
   #:codec-scheme-p
   #:encode-octets
   #:decode-text/k
   #:utf-8-octets
   #:octets->utf-8/k
   #:octets->lenient-text
   #:octets->hex
   ;; text-stats.lisp
   #:text-statistics
   ;; calc.lisp
   #:+calc-max-input-length+
   #:+calc-max-depth+
   #:+calc-max-bits+
   #:+calc-max-decimals+
   #:evaluate-expression/k
   #:format-decimal
   #:format-exact
   ;; uuid.lisp
   #:uuid-v4-from-octets
   #:uuid-octets->string
   #:make-uuid-v7-state
   #:uuid-v7-state-p
   #:next-uuid-v7
   #:+uuid-v7-random-octets+
   ;; random-string.lisp
   #:+random-alphabets+
   #:random-alphabet-p
   #:random-alphabet-string))
