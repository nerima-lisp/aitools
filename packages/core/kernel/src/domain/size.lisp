;;;; packages/core/kernel/src/domain/size.lisp
;;;;
;;;; Sizes (docs/src/reference/commands.md): a bare `<number>` is bytes; `KiB`/`MiB`/`GiB` are the
;;;; binary (1024-based) units. Fractional inputs (`1.5MiB`) are floored to a
;;;; whole byte count, since a size is always used as an integer boundary
;;;; (`--max-bytes`, `--skip-larger-than`).
(in-package #:aitools.kernel.domain)

(define-condition invalid-size-error (error)
  ((text :initarg :text :reader invalid-size-error-text))
  (:report (lambda (condition stream)
             (format stream "not a size: ~S" (invalid-size-error-text condition)))))

(defstruct (size
            (:constructor %make-size (bytes))
            (:copier nil))
  (bytes nil :type (integer 0) :read-only t))

(defparameter *size-unit-multipliers*
  '(("KiB" . 1024) ("MiB" . 1048576) ("GiB" . 1073741824)))

(defun parse-size (text)
  "Parse `<number>`, `<number>KiB`, `<number>MiB`, or `<number>GiB` into a
SIZE. Signals INVALID-SIZE-ERROR on any other text, including a leading `-`."
  (loop for (unit . multiplier) in *size-unit-multipliers*
        when (and (> (length text) (length unit))
                  (string= text unit :start1 (- (length text) (length unit))))
          do (let ((number (%parse-decimal-number (subseq text 0 (- (length text) (length unit))))))
               (when number
                 (return-from parse-size (%make-size (floor (* number multiplier))))))
        finally (let ((number (%parse-decimal-number text)))
                  (if number
                      (return-from parse-size (%make-size (floor number)))
                      (error 'invalid-size-error :text text)))))
