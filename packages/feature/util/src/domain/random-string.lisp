;;;; packages/feature/util/src/domain/random-string.lisp
;;;;
;;;; `util random` alphabet sampling. Each character is chosen by rejection
;;;; sampling over random octets: an octet at or above the largest multiple
;;;; of the alphabet size that fits in 256 is discarded, so every character
;;;; is equally likely (plain `octet mod 62` would favor the first 8 alnum
;;;; characters).
(in-package #:aitools.util.domain)

(defparameter +random-alphabets+ aitools.data:*util-random-alphabets*)

(defun random-alphabet-p (name)
  (and (assoc name +random-alphabets+ :test #'string=) t))

(defun random-alphabet-string (alphabet-name length next-octet)
  "A string of LENGTH characters drawn uniformly from ALPHABET-NAME's
characters. NEXT-OCTET is called with no arguments for each random octet."
  (declare (type function next-octet))
  (let* ((alphabet (cdr (assoc alphabet-name +random-alphabets+ :test #'string=)))
         (size (length alphabet))
         (limit (* size (floor 256 size)))
         (out (make-string length)))
    (dotimes (index length out)
      (setf (char out index)
            (loop for octet = (funcall next-octet)
                  when (< octet limit) return (char alphabet (mod octet size)))))))
