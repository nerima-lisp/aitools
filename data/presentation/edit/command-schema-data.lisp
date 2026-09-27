;;;; data/presentation/edit/command-schema-data.lisp
;;;;
;;;; Schema text shared across the edit context's commands
;;;; (see docs/src/reference/json-schema.md): the output fields every write command
;;;; (and every --expect-count command) adds, and the error codes every edit command may return.
;;;; AITOOLS.EDIT.PRESENTATION appends these to each command's own schema
;;;; fields; the per-command specs live in
;;;; data/application/edit/command-spec-data.lisp.
(in-package #:aitools.data)

(defparameter *edit-write-output-fields*
  '((:name "changes" :description "[{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines.")
    (:name "op_id" :description "The journal op (undo with `aitools undo <op_id>`); null when nothing changed.")
    (:name "tx" :description "With --tx: the tx the write was staged in, instead of op_id.")
    (:name "tx_op" :description "With --tx: the staged op's number.")
    (:name "dry_run" :description "With --dry-run: true; nothing was written."))
  "Output fields every command whose spec :INCLUDEs :WRITE reports.")

(defparameter *edit-common-error-codes*
  '("argument.invalid" "input.not-found" "refusal.outside-workspace" "refusal.redacted-input"
    "refusal.exists" "refusal.not-a-file" "environment.busy" "environment.io" "internal.unexpected")
  "Error codes every edit command may return, before the per-command and
per-option-group additions the presentation layer computes.")

(defparameter *edit-count-output-fields*
  '((:name "expect_count" :description "With --dry-run of a --match or replace selection: the lines or replacements it selected, the value a real run passes as --expect-count."))
  "Output fields every command whose spec :INCLUDEs :COUNT reports.")

(export '(*edit-write-output-fields* *edit-count-output-fields* *edit-common-error-codes*))
