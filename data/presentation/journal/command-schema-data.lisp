;;;; data/presentation/journal/command-schema-data.lisp
;;;;
;;;; Schema text for `history`, `undo` and the `tx` group
;;;; (docs/src/reference/transactions.md), one entry per command. AITOOLS.JOURNAL.PRESENTATION
;;;; builds each COMMAND-SCHEMA from this table; the cl-cli option
;;;; definitions live beside the handlers.
(in-package #:aitools.data)

(defparameter *journal-command-schemas*
  '((:name "history"
     :summary "List journaled operations (op_id, command, paths, time), newest first."
     :description "Every committed write, tx commit and undo is one op. With a path, only ops that touched it or anything under it (a move's source counts). More ops than --limit make the result partial (exit 3) with the command that lists them all in next_commands. Reads take no lock."
     :args ((:name "path" :kind "positional" :type "string" :description "Only ops touching this path (absolute or relative to the working directory).")
            (:name "--limit" :type "integer" :default 50 :description "Ops returned."))
     :output-fields ((:name "items" :description "[{op_id, command, paths, time, undoes?}], newest first; undoes names the op an undo reverted.")
                     (:name "total" :description "Ops matching, before --limit.")
                     (:name "truncated" :description "True when --limit cut the list."))
     :error-codes ("argument.invalid" "environment.io"))
    (:name "undo"
     :summary "Revert one journaled op, checking first that none of its paths changed since."
     :description "op_id is required (see aitools history). If any path of the op is no longer in the state the op left it in, nothing is written and the changed paths come back as conflicts (exit 2). The undo is itself a new op with undoes = op_id, so undoing it redoes the original. Takes the workspace lock (--lock-timeout)."
     :args ((:name "op_id" :kind "positional" :type "string" :required t :description "The op to revert, from aitools history.")
            (:name "--dry-run" :type "flag" :description "Validate and compute the changes without writing or journaling.")
            (:name "--max-diff-lines" :type "integer" :default 200 :description "Diff lines shown per change; longer diffs are cut and marked diff_truncated."))
     :output-fields ((:name "changes" :description "[{path, action, from?, hash_before, hash_after, diff?, diff_truncated?}], as every write reports.")
                     (:name "op_id" :description "The new op recording the undo (absent with --dry-run).")
                     (:name "dry_run" :description "True with --dry-run.")
                     (:name "undoes" :description "The reverted op_id."))
     :error-codes ("argument.invalid" "input.not-found" "refusal.target-changed" "environment.busy" "environment.io"))
    (:name "tx.begin"
     :summary "Open a transaction: --tx writes stack up in it without touching the working tree."
     :description "Writes given --tx <tx> are validated against the tx state and recorded in it; reads given --tx see it. Nothing reaches the files until tx commit. Open transactions never expire."
     :args ((:name "--name" :type "string" :description "Label shown by tx status."))
     :output-fields ((:name "tx" :description "The tx id for --tx and the other tx commands.")
                     (:name "name" :description "The --name label, or null."))
     :error-codes ("environment.busy" "environment.io"))
    (:name "tx.status"
     :summary "List open transactions, or show one tx's operations, paths, drift and stale reads."
     :description "drift lists written paths whose file changed outside the tx since the tx first touched it (commit would conflict; see tx rebase); stale_reads lists files read through the tx that changed since (commit would conflict unless re-read or --ignore-stale-reads). A path's base and staged are content hashes, null for a missing file, directory or symlink."
     :args ((:name "tx" :kind "positional" :type "string" :description "The tx to detail; omit to list all."))
     :output-fields ((:name "items" :description "Without tx: [{tx, name, created, ops, paths, drift, stale_reads}], ops and paths as counts.")
                     (:name "total" :description "Without tx: number of open transactions.")
                     (:name "ops" :description "With tx: [{tx_op, command, paths}].")
                     (:name "paths" :description "With tx: [{path, action, base, staged}].")
                     (:name "drift" :description "Written paths changed outside the tx.")
                     (:name "stale_reads" :description "Paths read through the tx and changed since."))
     :error-codes ("input.not-found" "environment.io"))
    (:name "tx.diff"
     :summary "Show what committing a tx would change, as write-output changes without an op_id."
     :description "Compares each path's base (its state when the tx first touched it) with its staged state. A cut diff is marked diff_truncated and next_commands repeats this command with a limit that fits."
     :args ((:name "tx" :kind "positional" :type "string" :required t :description "The tx.")
            (:name "--max-diff-lines" :type "integer" :default 200 :description "Diff lines shown per change."))
     :output-fields ((:name "tx" :description "The tx.")
                     (:name "changes" :description "[{path, action, from?, hash_before, hash_after, diff?, diff_truncated?}]."))
     :error-codes ("input.not-found" "environment.io"))
    (:name "tx.drop"
     :summary "Undo one tx operation and every later one, restoring the tx state before it."
     :description "Later operations build on earlier ones, so dropping one drops everything after it too."
     :args ((:name "tx" :kind "positional" :type "string" :required t :description "The tx.")
            (:name "tx_op" :kind "positional" :type "integer" :required t :description "The first operation to drop, from tx status."))
     :output-fields ((:name "tx" :description "The tx.")
                     (:name "dropped" :description "The dropped tx_op numbers, ascending."))
     :error-codes ("argument.invalid" "input.not-found" "environment.busy" "environment.io"))
    (:name "tx.rebase"
     :summary "Re-apply a tx's operations onto files that changed outside it, moving its base forward."
     :description "Only operations touching drifted paths (or paths an earlier re-applied operation rewrote) are re-run, from their recorded arguments. Content-based operations (--old, --between, --match, replace, apply, json writes) search again; position-based ones (--range, --symbol, --expect-hash) are not re-run and make the rebase a conflict. On any conflict the tx is left unchanged (exit 2)."
     :args ((:name "tx" :kind "positional" :type "string" :required t :description "The tx."))
     :output-fields ((:name "tx" :description "The tx.")
                     (:name "rebased" :description "The drifted paths whose base moved to the current file."))
     :error-codes ("input.not-found" "refusal.target-changed" "environment.busy" "environment.io"))
    (:name "tx.commit"
     :summary "Write a whole tx to the working tree as one journaled op, or report its conflicts."
     :description "Atomically write every staged path. A written path whose file changed since the tx first touched it is a write conflict (repair: tx rebase); a file read through the tx that changed since is a read conflict (repair: read it again with --tx, or --ignore-stale-reads). Any conflict writes nothing (exit 2). The op_id undoes the whole tx."
     :args ((:name "tx" :kind "positional" :type "string" :required t :description "The tx.")
            (:name "--ignore-stale-reads" :type "flag" :description "Commit despite read conflicts.")
            (:name "--max-diff-lines" :type "integer" :default 200 :description "Diff lines shown per change."))
     :output-fields ((:name "changes" :description "[{path, action, from?, hash_before, hash_after, diff?, diff_truncated?}], as every write reports.")
                     (:name "op_id" :description "The op recording the commit; null for an empty tx."))
     :error-codes ("input.not-found" "refusal.target-changed" "environment.busy" "environment.io"))
    (:name "tx.abort"
     :summary "Discard a tx without touching the working tree."
     :args ((:name "tx" :kind "positional" :type "string" :required t :description "The tx."))
     :output-fields ((:name "tx" :description "The tx.")
                     (:name "discarded" :description "The paths the tx had staged, sorted."))
     :error-codes ("input.not-found" "environment.busy" "environment.io")))
  "One plist per journal-context command: :NAME, :SUMMARY, :DESCRIPTION,
:ARGS, :OUTPUT-FIELDS, :ERROR-CODES, in COMMAND-SCHEMA terms.")

(export '(*journal-command-schemas*))
