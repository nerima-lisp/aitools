;;;; packages/feature/journal/src/domain/package.lisp
;;;;
;;;; Pure rendering for `history`, `undo` and the `tx` group: journal entries
;;;; and tx states as the JSON shapes of docs/src/reference/commands.md and
;;;; docs/src/reference/transactions.md, the
;;;; command lines placed in `repairs[].command` and `next_commands`, and the
;;;; commit-conflict repairs. The store context owns every record read here.
(in-package #:cl-user)

(defpackage #:aitools.journal.domain
  (:use #:cl)
  (:import-from #:aitools.protocol.domain #:json-object #:repair)
  (:export
   ;; command-line.lisp
   #:command-line
   #:argv-command-line
   ;; render.lisp
   #:+default-history-limit+
   #:history-item
   #:tx-path-action
   #:state-hash
   #:repair
   #:commit-conflict-repairs))
