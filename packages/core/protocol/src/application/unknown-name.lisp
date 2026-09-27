;;;; packages/core/protocol/src/application/unknown-name.lisp
;;;;
;;;; Repairing a call made with a known foreign name
;;;; (docs/src/reference/errors.md, Unknown command names): composes
;;;; AITOOLS.PROTOCOL.DOMAIN's placement data into the three values dispatch
;;;; needs to build an ARGUMENT.INVALID envelope for a name that dispatched
;;;; to nothing.
(in-package #:aitools.protocol.application)

(defun unknown-command-error (attempted-name)
  "Return (VALUES CODE MESSAGE REPAIRS) for an unrecognized dispatch NAME.
CODE is always \"argument.invalid\"; REPAIRS draws on the correspondence
table (AITOOLS.PROTOCOL.DOMAIN:REPAIRS-FOR-UNKNOWN-NAME), which also covers a
bare group subcommand name such as `uuid`."
  (values "argument.invalid"
          (format nil "unknown command ~A" attempted-name)
          (aitools.protocol.domain:repairs-for-unknown-name attempted-name)))
