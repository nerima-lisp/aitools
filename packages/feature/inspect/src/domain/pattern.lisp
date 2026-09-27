;;;; packages/feature/inspect/src/domain/pattern.lisp
;;;;
;;;; Regular expressions as the inspect context uses them: compiled
;;;; once with cl-regex-kit and matched against one line string at a time
;;;; (selectors, `--where ~`). A syntax error is reported as data so the
;;;; caller can answer `input.syntax-error`.
(in-package #:aitools.inspect.domain)

(defun compile-pattern/k (pattern &key ignore-case on-compiled on-error)
  "Compile PATTERN and call ON-COMPILED (regex) or ON-ERROR (message)."
  (declare (type function on-compiled on-error))
  (let ((regex (handler-case (cl-regex-kit:compile-regex pattern :case-insensitive (and ignore-case t))
                 (cl-regex-kit:regex-syntax-error (condition)
                   (return-from compile-pattern/k
                     (funcall on-error
                              (format nil "invalid regular expression ~S at offset ~D: ~A"
                                      pattern
                                      (cl-regex-kit:regex-syntax-error-position condition)
                                      (cl-regex-kit:regex-syntax-error-reason condition))))))))
    (funcall on-compiled regex)))

(defun call-with-regex-limit/k (thunk on-limit)
  "Run THUNK; if a pattern on cl-regex-kit's bounded advanced executor
(backreferences, lookaround, ...) exhausts its step budget while matching,
call ON-LIMIT (message) rather than letting the condition escape as
internal.unexpected: read --match and json select --where must answer a
user error, as replace already does."
  (declare (type function thunk on-limit))
  (handler-case (funcall thunk)
    (cl-regex-kit:advanced-regex-limit-error (condition)
      (funcall on-limit (princ-to-string condition)))))

(defun pattern-matches-p (regex text)
  (and (cl-regex-kit:scan regex text) t))

(defun pattern-group-string (regex text group-name)
  "The text captured by GROUP-NAME in the first match of REGEX in TEXT, or
NIL when there is no match or the group did not participate."
  (let ((match (cl-regex-kit:scan regex text)))
    (and match
         (cl-regex-kit:match-group-string match (cl-regex-kit:regex-group-index regex group-name) text))))

