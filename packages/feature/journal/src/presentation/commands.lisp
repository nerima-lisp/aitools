;;;;
;;;; cl-cli commands for `history`, `undo` and the `tx` group. Each handler
;;;; turns the parsed invocation (including the global `--root` and
;;;; `--lock-timeout`) into one AITOOLS.JOURNAL.APPLICATION flow call and
;;;; returns the COMMAND-RESULT that call produced.
(in-package #:aitools.journal.presentation)

(defun %schema (name)
  (let ((entry (find name aitools.data:*journal-command-schemas*
                     :key (lambda (entry) (getf entry :name)) :test #'string=)))
    (aitools.protocol.domain:make-command-schema
     name (getf entry :summary)
     :description (getf entry :description)
     :args (getf entry :args)
     :output-fields (getf entry :output-fields)
     :error-codes (getf entry :error-codes))))

(defun %summary (name)
  (aitools.protocol.domain:command-schema-summary (%schema name)))

(defun %context (invocation)
  (aitools.journal.application:make-journal-context
   :root (option-value invocation :root)
   :lock-timeout (option-value invocation :lock-timeout)))

(defun %run-flow (flow &rest arguments)
  "Call FLOW with ARGUMENTS followed by the three command-result
continuations, returning the resulting COMMAND-RESULT."
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&rest continuations)
     (apply flow (append arguments continuations)))))

(defun %tx-positional ()
  (make-positional :key :tx :name "tx" :required-p t))

(defun %max-diff-lines-option ()
  (make-option :name "max-diff-lines" :kind :value :type :integer :min 0 :default 200
               :description "Diff lines shown per change."))

(defun %history-command (ports)
  (make-command
   :name "history" :description (%summary "history")
   :positionals (list (make-positional :key :path :name "path" :required-p nil))
   :options (list (make-option :name "limit" :kind :value :type :integer :min 1 :default 50
                               :description "Ops returned."))
   :handler (lambda (invocation)
              (%run-flow #'aitools.journal.application:history-flow ports (%context invocation)
                         :path (positional-value invocation :path)
                         :limit (option-value invocation :limit)))))

(defun %undo-command (ports)
  (make-command
   :name "undo" :description (%summary "undo")
   ;; Optional for cl-cli so that a missing op_id reaches the flow, which
   ;; answers with `aitools history` as the repair.
   :positionals (list (make-positional :key :op-id :name "op_id" :required-p nil))
   :options (list (make-option :name "dry-run" :kind :flag :description "Validate without writing.")
                  (%max-diff-lines-option))
   :handler (lambda (invocation)
              (%run-flow #'aitools.journal.application:undo-flow ports (%context invocation)
                         (positional-value invocation :op-id)
                         :dry-run (and (option-value invocation :dry-run) t)
                         :max-diff-lines (option-value invocation :max-diff-lines)))))

(defun %tx-begin-command (ports)
  (make-command
   :name "begin" :description (%summary "tx.begin")
   :options (list (make-option :name "name" :kind :value :value-name "NAME" :description "Label for tx status."))
   :handler (lambda (invocation)
              (%run-flow #'aitools.journal.application:tx-begin-flow ports (%context invocation)
                         :name (option-value invocation :name)))))

(defun %tx-status-command (ports)
  (make-command
   :name "status" :description (%summary "tx.status")
   :positionals (list (make-positional :key :tx :name "tx" :required-p nil))
   :handler (lambda (invocation)
              (%run-flow #'aitools.journal.application:tx-status-flow ports (%context invocation)
                         (positional-value invocation :tx)))))

(defun %tx-diff-command (ports)
  (make-command
   :name "diff" :description (%summary "tx.diff")
   :positionals (list (%tx-positional))
   :options (list (%max-diff-lines-option))
   :handler (lambda (invocation)
              (%run-flow #'aitools.journal.application:tx-diff-flow ports (%context invocation)
                         (positional-value invocation :tx)
                         :max-diff-lines (option-value invocation :max-diff-lines)))))

(defun %tx-drop-command (ports)
  (make-command
   :name "drop" :description (%summary "tx.drop")
   :positionals (list (%tx-positional) (make-positional :key :tx-op :name "tx_op" :required-p t))
   :handler (lambda (invocation)
              (%run-flow #'aitools.journal.application:tx-drop-flow ports (%context invocation)
                         (positional-value invocation :tx)
                         (positional-value invocation :tx-op)))))

(defun %tx-rebase-command (ports)
  (make-command
   :name "rebase" :description (%summary "tx.rebase")
   :positionals (list (%tx-positional))
   :handler (lambda (invocation)
              (%run-flow #'aitools.journal.application:tx-rebase-flow ports (%context invocation)
                         (positional-value invocation :tx)))))

(defun %tx-commit-command (ports)
  (make-command
   :name "commit" :description (%summary "tx.commit")
   :positionals (list (%tx-positional))
   :options (list (make-option :name "ignore-stale-reads" :kind :flag
                               :description "Commit despite read conflicts.")
                  (%max-diff-lines-option))
   :handler (lambda (invocation)
              (%run-flow #'aitools.journal.application:tx-commit-flow ports (%context invocation)
                         (positional-value invocation :tx)
                         :ignore-stale-reads (and (option-value invocation :ignore-stale-reads) t)
                         :max-diff-lines (option-value invocation :max-diff-lines)))))

(defun %tx-abort-command (ports)
  (make-command
   :name "abort" :description (%summary "tx.abort")
   :positionals (list (%tx-positional))
   :handler (lambda (invocation)
              (%run-flow #'aitools.journal.application:tx-abort-flow ports (%context invocation)
                         (positional-value invocation :tx)))))

(defun register-journal-commands (registry ports)
  "Register `history`, `undo` and `tx begin|status|diff|drop|rebase|commit|
abort` on REGISTRY, each handler running against PORTS (an
AITOOLS.JOURNAL.APPLICATION:JOURNAL-PORTS)."
  (aitools.protocol.application:register-command-declarations
   registry
   (list
    (aitools.protocol.application:define-command
        "history"
      (:command-builder #'%history-command :schema (%schema "history")))
    (aitools.protocol.application:define-command
        "undo"
      (:command-builder #'%undo-command :schema (%schema "undo")))
    (aitools.protocol.application:define-command
        "tx.begin"
      (:group "tx" :command-builder #'%tx-begin-command :schema (%schema "tx.begin")))
    (aitools.protocol.application:define-command
        "tx.status"
      (:group "tx" :command-builder #'%tx-status-command :schema (%schema "tx.status")))
    (aitools.protocol.application:define-command
        "tx.diff"
      (:group "tx" :command-builder #'%tx-diff-command :schema (%schema "tx.diff")))
    (aitools.protocol.application:define-command
        "tx.drop"
      (:group "tx" :command-builder #'%tx-drop-command :schema (%schema "tx.drop")))
    (aitools.protocol.application:define-command
        "tx.rebase"
      (:group "tx" :command-builder #'%tx-rebase-command :schema (%schema "tx.rebase")))
    (aitools.protocol.application:define-command
        "tx.commit"
      (:group "tx" :command-builder #'%tx-commit-command :schema (%schema "tx.commit")))
    (aitools.protocol.application:define-command
        "tx.abort"
      (:group "tx" :command-builder #'%tx-abort-command :schema (%schema "tx.abort"))))
   ports)
  registry)
