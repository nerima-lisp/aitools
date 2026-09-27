;;;; t/unit/text/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.text.test
  (:use #:cl #:aitools.text.domain #:aitools.text.application)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect #:expect-not #:signals #:fail)
  (:import-from #:aitools.test.support #:string-bytes))

(in-package #:aitools.text.test)

(defun octets (&rest bytes)
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun hex-octets (hex)
  (let ((result (make-array (floor (length hex) 2) :element-type '(unsigned-byte 8))))
    (dotimes (i (length result) result)
      (setf (aref result i) (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun join-octets (&rest parts)
  (apply #'concatenate '(simple-array (unsigned-byte 8) (*)) parts))

(defun pseudo-random-octets (count seed)
  "Deterministic bytes from a linear congruential generator."
  (let ((result (make-array count :element-type '(unsigned-byte 8))) (state seed))
    (dotimes (i count result)
      (setf state (mod (+ (* state 1103515245) 12345) 2147483648))
      (setf (aref result i) (ldb (byte 8 16) state)))))
