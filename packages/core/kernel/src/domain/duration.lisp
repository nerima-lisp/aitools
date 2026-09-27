;;;; packages/core/kernel/src/domain/duration.lisp
;;;;
;;;; Durations (docs/src/reference/commands.md): `<number>ms|s|m|h|d`. Negative durations are rejected
;;;; by construction -- a leading `-` can never parse as the numeric prefix
;;;; below: a value starting with `-` cannot be told apart from a flag, so
;;;; negative values are not accepted, and a negative duration is expressed
;;;; only via `time convert --sub`.
(in-package #:aitools.kernel.domain)

(define-condition invalid-duration-error (error)
  ((text :initarg :text :reader invalid-duration-error-text))
  (:report (lambda (condition stream)
             (format stream "not a duration: ~S" (invalid-duration-error-text condition)))))

(defstruct (duration
            (:constructor %make-duration (milliseconds))
            (:copier nil))
  (milliseconds nil :type (integer 0) :read-only t))

(defparameter *duration-unit-milliseconds*
  '(("ms" . 1) ("s" . 1000) ("m" . 60000) ("h" . 3600000) ("d" . 86400000))
  "Longest suffix candidates are tried first in PARSE-DURATION so that `ms`
does not get misread as the unit `m` plus a dangling `s`.")

(defun %parse-decimal-number (text)
  "Parse TEXT as a nonnegative decimal number (`123`, `1.5`) and return an
exact RATIONAL, or NIL if TEXT is not one. No sign is accepted."
  (when (plusp (length text))
    (let ((dot (position #\. text)))
      (if dot
          (let ((integer-part (subseq text 0 dot))
                (fraction-part (subseq text (1+ dot))))
            (when (and (or (plusp (length integer-part)) (plusp (length fraction-part)))
                       (every #'%ascii-digit-char-p integer-part)
                       (plusp (length fraction-part))
                       (every #'%ascii-digit-char-p fraction-part))
              (+ (if (plusp (length integer-part)) (parse-integer integer-part) 0)
                 (/ (parse-integer fraction-part) (expt 10 (length fraction-part))))))
          (when (every #'%ascii-digit-char-p text)
            (parse-integer text))))))

(defun parse-duration (text)
  "Parse a `<number>ms|s|m|h|d` duration into a DURATION rounded to the
nearest whole millisecond. Signals INVALID-DURATION-ERROR on any other text,
including an empty string, a bare number with no unit, or a leading `-`."
  (dolist (entry (sort (copy-list *duration-unit-milliseconds*) #'>
                       :key (lambda (entry) (length (car entry)))))
    (destructuring-bind (unit . unit-milliseconds) entry
      (when (and (> (length text) (length unit))
                 (string= text unit :start1 (- (length text) (length unit))))
        (let ((number (%parse-decimal-number (subseq text 0 (- (length text) (length unit))))))
          (when number
            (return-from parse-duration
              (%make-duration (round (* number unit-milliseconds)))))))))
  (error 'invalid-duration-error :text text))
