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
   #:normalize-command-continuations
   #:call-with-command-result/k
   ;; command-registry.lisp
   #:command-declaration
   #:make-command-declaration
   #:command-declaration-p
   #:command-declaration-name
   #:command-declaration-group
   #:command-declaration-command-builder
   #:command-declaration-schema
   #:define-command
   #:command-registry
   #:make-command-registry
   #:command-registry-top-level
   #:command-registry-group-commands
   #:register-command
   #:register-command-declaration
   #:register-command-declarations
   #:find-command-schema
   #:all-command-schemas
   ;; redaction-flow.lisp
   #:redact-json-value
   ;; schema-flow.lisp
   #:render-command-summary
   #:render-command-detail
   ;; unknown-name.lisp
   #:unknown-command-error))
