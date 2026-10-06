;;;; packages/feature/journal/src/application/package.lisp
;;;;
;;;; The journal context's public boundary: the flows behind `history`,
;;;; `undo` and the `tx` group (thin over the store context's journal, undo
;;;; and tx API), their ports, and the registry through which write-command
;;;; owners supply `tx rebase` replays.
(in-package #:cl-user)

(defpackage #:aitools.journal.application
  (:use #:cl #:aitools.journal.domain)
  (:import-from #:aitools.protocol.domain
                #:json-object #:json-null #:json-boolean #:repair #:schema-repair)
  (:export
   ;; ports.lisp
   #:journal-ports
   #:make-journal-ports
   #:journal-ports-p
   #:journal-context
   #:make-journal-context
   #:journal-context-p
   ;; replayers.lisp
   #:register-tx-replayer
   #:find-tx-replayer
   ;; history-flow.lisp
   #:history-flow
   ;; undo-flow.lisp
   #:undo-flow
   ;; tx-flows.lisp
   #:tx-begin-flow
   #:tx-status-flow
   #:tx-diff-flow
   #:tx-drop-flow
   #:tx-rebase-flow
   #:tx-commit-flow
   #:tx-abort-flow))
