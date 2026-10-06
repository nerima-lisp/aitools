;;;; packages/core/protocol/src/domain/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.protocol.domain
  (:use #:cl)
  (:export
   ;; envelope.lisp
   #:json-object-from-alist
   #:json-object
   #:json-null
   #:json-or-null
   #:json-boolean
   #:json-object-p
   #:json-object-members
   #:make-ok-envelope
   #:make-error-envelope
   ;; error-catalog.lisp
   #:error-code-exit-code
   #:error-code-known-p
   #:*error-codes*
   ;; redaction.lisp
   #:redact-secrets
   #:redact-secret-sequence
   #:secret-key-name-p
   ;; shell-words.lisp
   #:shell-quote
   #:command-line
   ;; envelope.lisp
   #:repair
   #:schema-repair
   ;; command-placement.lisp
   #:correspondence-name-p
   #:top-level-command-p
   #:group-command-p
   #:*top-level-commands*
   #:*command-groups*
   #:repairs-for-unknown-name
   ;; schema-model.lisp
   #:make-command-schema
   #:command-schema
   #:command-schema-p
   #:command-schema-name
   #:command-schema-summary
   #:command-schema-description
   #:command-schema-args
   #:command-schema-output-fields
   #:command-schema-error-codes))
