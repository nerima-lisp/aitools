;;;; packages/feature/process/src/domain/line-pattern.lisp
;;;;
;;;; `run --grep`, `bg logs --grep`, and `wait --pattern` all match one line
;;;; at a time (without `--multiline` a pattern never spans a
;;;; newline), so a compiled pattern is only ever applied to a single line.
(in-package #:aitools.process.domain)

(define-condition invalid-line-pattern (error)
  ((message :initarg :message :reader invalid-line-pattern-message))
  (:report (lambda (condition stream)
             (write-string (invalid-line-pattern-message condition) stream))))

(defun compile-line-pattern (source)
  "Compile SOURCE with cl-regex-kit. Signals INVALID-LINE-PATTERN (never the
kit's own condition) on a syntax error, so callers map exactly one condition
to `input.syntax-error`."
  (handler-case (cl-regex-kit:compile-regex source)
    (cl-regex-kit:regex-syntax-error (condition)
      (error 'invalid-line-pattern :message (princ-to-string condition)))))

(defun line-pattern-matches-p (pattern line)
  "True when PATTERN matches anywhere in LINE. A pattern that exhausts the
advanced executor's step budget on LINE signals INVALID-LINE-PATTERN rather
than counting as a non-match, which would silently drop a line."
  (handler-case (cl-regex-kit:is-match-p pattern line)
    (cl-regex-kit:cl-regex-kit-error (condition)
      (error 'invalid-line-pattern :message (princ-to-string condition)))))
