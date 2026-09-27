;;;; packages/core/protocol/src/domain/error-catalog.lisp
;;;;
;;;; Logic over the data table in data/domain/protocol/error-catalog-data.lisp.
(in-package #:aitools.protocol.domain)

(defparameter *error-codes* aitools.data:*protocol-error-codes*)

(defun %error-code-entry (code)
  (find code *error-codes* :key (lambda (entry) (getf entry :code)) :test #'string=))

(defun error-code-known-p (code)
  "True when CODE (a string like \"argument.invalid\") is one of the
error.code values in docs/src/reference/errors.md."
  (and (%error-code-entry code) t))

(defun error-code-exit-code (code)
  "The exit code for error.code CODE. Signals a SIMPLE-ERROR for an
unknown code -- every error site in aitools uses a literal string from this
catalog, so an unknown code here is a bug in aitools, not a runtime input."
  (let ((entry (%error-code-entry code)))
    (unless entry
      (error "unknown error.code ~S; add it to *protocol-error-codes* first" code))
    (getf entry :exit-code)))
