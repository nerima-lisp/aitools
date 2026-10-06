;;;; packages/feature/edit/src/application/package.lisp
;;;;
;;;; The edit context's public boundary. RUN-WRITE-COMMAND/K is the shared
;;;; write pipeline (also used by `util decode --to`); RUN-EDIT-COMMAND runs
;;;; one of this context's commands from its parsed positionals and options.
(in-package #:cl-user)

(defpackage #:aitools.edit.application
  (:use #:cl #:aitools.edit.domain)
  (:import-from #:aitools.protocol.domain #:json-object #:json-null #:repair)
  (:export
   ;; ports.lisp
   #:edit-ports
   #:edit-ports-p
   #:make-edit-ports
   #:make-write-edit-ports
   #:+max-input-bytes+
   ;; command-spec.lisp
   #:edit-command-specs
   #:find-command-spec
   #:command-words
   #:command-display-name
   #:options-argv
   #:parse-recorded-argv
   ;; write-plan.lisp, write-guards.lisp, pipeline.lisp
   #:write-target
   #:make-write-target
   #:write-plan
   #:make-write-plan
   #:write-context
   #:write-context-view
   #:write-context-paths
   #:context-path
   #:run-write-command/k
   #:run-plan
   #:check-expect-count/k
   #:require-expect-hash/k
   #:resolve-extra-path/k
   #:view-hash
   #:command-line
   #:repair
   #:default-repairs
   ;; commands.lisp
   #:run-edit-command
   #:replay-edit-op))
