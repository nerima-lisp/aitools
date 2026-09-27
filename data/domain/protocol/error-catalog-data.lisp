;;;; data/domain/protocol/error-catalog-data.lisp
;;;;
;;;; The error.code table (docs/src/reference/errors.md). AITOOLS.PROTOCOL.
;;;; DOMAIN:ERROR-CODE-EXIT-CODE and :ERROR-CODE-KNOWN-P read this list; it is
;;;; the single place a new error code or a change to an exit code is added.
(in-package #:aitools.data)

(defparameter *protocol-error-codes*
  '((:code "argument.invalid" :exit-code 1)
    (:code "input.not-found" :exit-code 1)
    (:code "input.not-utf8" :exit-code 1)
    (:code "input.unsupported-format" :exit-code 1)
    (:code "input.unsupported-language" :exit-code 1)
    (:code "input.syntax-error" :exit-code 1)
    (:code "selection.no-match" :exit-code 2)
    (:code "selection.ambiguous" :exit-code 2)
    (:code "selection.count-mismatch" :exit-code 2)
    (:code "refusal.target-changed" :exit-code 2)
    (:code "refusal.redacted-input" :exit-code 1)
    (:code "refusal.outside-workspace" :exit-code 1)
    (:code "refusal.exists" :exit-code 1)
    (:code "refusal.not-a-file" :exit-code 1)
    (:code "refusal.too-large" :exit-code 1)
    (:code "environment.io" :exit-code 1)
    (:code "environment.busy" :exit-code 1)
    (:code "environment.timeout" :exit-code 1)
    (:code "environment.unavailable" :exit-code 1)
    (:code "internal.unexpected" :exit-code 1))
  "One entry per error.code.")

(export '(*protocol-error-codes*))
