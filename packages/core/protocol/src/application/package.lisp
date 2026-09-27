;;;; packages/core/protocol/src/application/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.protocol.application
  (:use #:cl)
  (:export
   ;; command-result.lisp
   #:command-result
   #:command-result-p
   #:command-result-kind
   #:command-result-fields
   #:call-with-command-result/k
   ;; command-registry.lisp
   #:command-registry
   #:make-command-registry
   #:command-registry-top-level
   #:command-registry-group-commands
   #:register-command
   #:find-command-schema
   #:all-command-schemas
   ;; redaction-flow.lisp
   #:redact-json-value
   ;; schema-flow.lisp
   #:render-command-summary
   #:render-command-detail
   ;; unknown-name.lisp
   #:unknown-command-error))
