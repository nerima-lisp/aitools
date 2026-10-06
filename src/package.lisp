;;;; src/package.lisp -- the aitools composition root.
;;;;
;;;; This package assembles every context's presentation layer into one
;;;; cl-cli app, dispatches parsed argv to a command's handler, and owns the
;;;; process entry point. It is the only package allowed to depend on cl-cli
;;;; (the "aitools" system never depends on cl-cli) and the only
;;;; one allowed to reference every context's presentation package.
(in-package #:cl-user)

(defpackage #:aitools/cli
  (:use #:cl)
  (:import-from #:cl-cli
                #:make-app #:make-command #:make-option #:make-positional
                #:parse-argv #:current-process-argv
                #:invocation-action #:invocation-command #:invocation-command-path #:invocation-app
                #:invocation-positionals #:invocation-global-options #:invocation-command-options
                #:option-value #:positional-value
                #:app-version #:app-handler #:command-handler #:command-name
                #:cli-error #:cli-error-message
                #:cli-usage-error #:cli-usage-error-command
                #:cli-unknown-command #:cli-unknown-command-name)
  (:import-from #:aitools.protocol.application
                #:command-registry #:make-command-registry
                #:command-registry-top-level #:command-registry-group-commands
                #:register-command #:find-command-schema #:all-command-schemas)
  (:import-from #:aitools.protocol.domain
                #:repair #:schema-repair #:repairs-for-unknown-name)
  (:export
   ;; registry.lisp
   #:finalize-app-commands
   ;; context-registration.lisp
   #:register-all-context-commands
   ;; app.lisp
   #:build-app
   ;; dispatch.lisp
   #:dispatch
   ;; entry-point.lisp
   #:main
   #:image-entry-point))
