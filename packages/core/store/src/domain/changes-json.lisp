;;;; packages/core/store/src/domain/changes-json.lisp
;;;;
;;;; The write output (docs/src/reference/json-schema.md), shared by every write command, `undo`, `tx
;;;; diff` and `tx commit`: CHANGE-RESULTs rendered as `changes[]` with a
;;;; per-change unified diff cut at `--max-diff-lines`, and the fields that
;;;; accompany them (`op_id`, or `tx`/`tx_op`, or `dry_run`, and
;;;; `next_commands` when a diff was cut). Also recovery's `recovered[]`.
;;;;
;;;; A diff is rendered only for a change whose both sides are a regular
;;;; file or absent and whose bytes are text (the NUL sniff); moves,
;;;; mode changes, symlinks and directories carry their hashes only. Lines
;;;; are split at LF and keep a CR, so an LF/CRLF conversion shows up as a
;;;; change. The diff holds hunks only (`@@ ...`), since `path` already
;;;; names the file.
(in-package #:aitools.store.domain)

(defconstant +default-max-diff-lines+ 200
  "The `--max-diff-lines` default.")

(defun %diff-lines (octets)
  "(values lines final-newline-p) of OCTETS decoded as UTF-8 with
replacement; NIL octets are an absent file (no lines)."
  (if (or (null octets) (zerop (length octets)))
      (values #() t)
      (let* ((text (aitools.text.domain:decode-utf8 octets))
             (final-newline (char= (char text (1- (length text))) #\Newline))
             (end (if final-newline (1- (length text)) (length text))))
        (values (coerce (loop with start = 0
                              for newline = (position #\Newline text :start start :end end)
                              collect (subseq text start (or newline end))
                              while newline
                              do (setf start (1+ newline)))
                        'simple-vector)
                final-newline))))

(defun %diffable-state-p (state)
  (member (entry-state-kind state) '(:absent :file)))

(defun change-diff (result)
  "RESULT's unified diff (hunks only; \"\" when the text is unchanged), or
NIL when RESULT has no text diff: a move, a mode change, a symlink or
directory on either side, or binary content on either side."
  (let ((before (change-result-before-content result))
        (after (change-result-after-content result)))
    (when (and (member (change-result-action result) '(:created :modified :deleted))
               (%diffable-state-p (change-result-before result))
               (%diffable-state-p (change-result-after result))
               (not (and before (aitools.text.domain:binary-octets-p before)))
               (not (and after (aitools.text.domain:binary-octets-p after))))
      (multiple-value-bind (old-lines old-final) (%diff-lines before)
        (multiple-value-bind (new-lines new-final) (%diff-lines after)
          (aitools.kernel.domain:render-hunks
           (aitools.kernel.domain:generate-diff-hunks
            old-lines new-lines :final-newline-a old-final :final-newline-b new-final)))))))

(defun %line-count (text)
  (count #\Newline text))

(defun %first-lines (text count)
  "The first COUNT newline-terminated lines of TEXT."
  (let ((end 0))
    (dotimes (i count (subseq text 0 end))
      (setf end (1+ (position #\Newline text :start end))))))

(defun changes->json (results &key (max-diff-lines +default-max-diff-lines+))
  "The write output's `changes[]` for CHANGE-RESULTs RESULTS, in their order. Each
element is {path, action, from (moves only), hash_before, hash_after,
diff?, diff_truncated?}; a hash is null when that side is not a regular
file. A diff longer than MAX-DIFF-LINES lines is cut to that many and
marked `diff_truncated: true`.

Returns (values changes longest-truncated): LONGEST-TRUNCATED is the line
count of the longest cut diff, or NIL when nothing was cut, for the
caller's `next_commands`."
  (check-type max-diff-lines (integer 0))
  (let ((longest nil))
    (values
     (mapcar (lambda (result)
               (let* ((diff (change-diff result))
                      (lines (and diff (%line-count diff)))
                      (cut (and lines (> lines max-diff-lines))))
                 (when cut
                   (setf longest (max lines (or longest 0))))
                 (apply #'json-object
                        (append (list "path" (change-result-path result)
                                      "action" (action-name (change-result-action result)))
                                (when (change-result-from result)
                                  (list "from" (change-result-from result)))
                                (list "hash_before" (%json-null-or (change-result-hash-before result))
                                      "hash_after" (%json-null-or (change-result-hash-after result)))
                                (when diff
                                  (list "diff" (if cut (%first-lines diff max-diff-lines) diff)))
                                (when cut
                                  (list "diff_truncated" t))))))
             results)
     longest)))

(defun op-diff-command (op-id)
  "The command a write result puts in `next_commands` to show OP-ID's full diff."
  (format nil "aitools diff --op ~A" op-id))

(defun write-result-fields (results &key op-id tx tx-op dry-run (max-diff-lines +default-max-diff-lines+))
  "The complete write-output field alist ((STRING . VALUE) ...), ready for the
command-result ON-OK continuation, for a write that produced RESULTS:
`changes`, then `op_id` (a committed write, OP-ID), `tx` and `tx_op` (a
`--tx` write, TX and TX-OP), or `dry_run: true` (DRY-RUN), then
`next_commands` when a diff was cut. A committed write that changed
nothing has OP-ID NIL and reports `op_id: null`.

The next command for a cut diff is `aitools diff --op <op_id>`; for a tx
write, `aitools tx diff <tx>` with a limit that fits the longest diff; a dry
run has nothing to point at and gets none."
  (multiple-value-bind (changes longest) (changes->json results :max-diff-lines max-diff-lines)
    (append (list (cons "changes" changes))
            (cond (dry-run (list (cons "dry_run" t)))
                  (tx (list (cons "tx" tx) (cons "tx_op" (%json-null-or tx-op))))
                  (t (list (cons "op_id" (%json-null-or op-id)))))
            (when (and longest (not dry-run) (or tx op-id))
              (list (cons "next_commands"
                          (list (if tx
                                    (format nil "aitools tx diff ~A --max-diff-lines ~D" tx longest)
                                    (op-diff-command op-id)))))))))

(defun recovered->json (entries)
  "Recovery's `recovered[]` elements {op_id, action} for RECOVER/K's
((op-id . action) ...) result."
  (mapcar (lambda (entry) (json-object "op_id" (car entry) "action" (cdr entry))) entries))
