# The journal context

[Command index](index.md#command-index).

### `history` {#history}

List journaled operations (op_id, command, paths, time), newest first.

Every committed write, tx commit and undo is one op. With a path, only ops that touched it or anything under it (a move's source counts). More ops than --limit make the result partial (exit 3) with the command that lists them all in next_commands. Reads take no lock.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional) | string |  | Only ops touching this path (absolute or relative to the working directory). |
| `--limit` | integer | `50` | Ops returned. |

| Output field | Description |
|---|---|
| `items` | [{op_id, command, paths, time, undoes?}], newest first; undoes names the op an undo reverted. |
| `total` | Ops matching, before --limit. |
| `truncated` | True when --limit cut the list. |

Errors: `argument.invalid`, `environment.io`.

### `undo` {#undo}

Revert one journaled op, checking first that none of its paths changed since.

op_id is required (see aitools history). If any path of the op is no longer in the state the op left it in, nothing is written and the changed paths come back as conflicts (exit 2). The undo is itself a new op with undoes = op_id, so undoing it redoes the original. Takes the workspace lock (--lock-timeout).

| Argument | Type | Default | Description |
|---|---|---|---|
| `op_id` (positional, required) | string |  | The op to revert, from aitools history. |
| `--dry-run` | flag |  | Validate and compute the changes without writing or journaling. |
| `--max-diff-lines` | integer | `200` | Diff lines shown per change; longer diffs are cut and marked diff_truncated. |

| Output field | Description |
|---|---|
| `changes` | [{path, action, from?, hash_before, hash_after, diff?, diff_truncated?}], as every write reports. |
| `op_id` | The new op recording the undo (absent with --dry-run). |
| `dry_run` | True with --dry-run. |
| `undoes` | The reverted op_id. |

Errors: `argument.invalid`, `input.not-found`, `refusal.target-changed`, `environment.busy`, `environment.io`.

### `tx begin` {#tx-begin}

Open a transaction: --tx writes stack up in it without touching the working tree.

Writes given --tx <tx> are validated against the tx state and recorded in it; reads given --tx see it. Nothing reaches the files until tx commit. Open transactions never expire.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--name` | string |  | Label shown by tx status. |

| Output field | Description |
|---|---|
| `tx` | The tx id for --tx and the other tx commands. |
| `name` | The --name label, or null. |

Errors: `environment.busy`, `environment.io`.

### `tx status` {#tx-status}

List open transactions, or show one tx's operations, paths, drift and stale reads.

drift lists written paths whose file changed outside the tx since the tx first touched it (commit would conflict; see tx rebase); stale_reads lists files read through the tx that changed since (commit would conflict unless re-read or --ignore-stale-reads). A path's base and staged are content hashes, null for a missing file, directory or symlink.

| Argument | Type | Default | Description |
|---|---|---|---|
| `tx` (positional) | string |  | The tx to detail; omit to list all. |

| Output field | Description |
|---|---|
| `items` | Without tx: [{tx, name, created, ops, paths, drift, stale_reads}], ops and paths as counts. |
| `total` | Without tx: number of open transactions. |
| `ops` | With tx: [{tx_op, command, paths}]. |
| `paths` | With tx: [{path, action, base, staged}]. |
| `drift` | Written paths changed outside the tx. |
| `stale_reads` | Paths read through the tx and changed since. |

Errors: `input.not-found`, `environment.io`.

### `tx diff` {#tx-diff}

Show what committing a tx would change, as write-output changes without an op_id.

Compares each path's base (its state when the tx first touched it) with its staged state. A cut diff is marked diff_truncated and next_commands repeats this command with a limit that fits.

| Argument | Type | Default | Description |
|---|---|---|---|
| `tx` (positional, required) | string |  | The tx. |
| `--max-diff-lines` | integer | `200` | Diff lines shown per change. |

| Output field | Description |
|---|---|
| `tx` | The tx. |
| `changes` | [{path, action, from?, hash_before, hash_after, diff?, diff_truncated?}]. |

Errors: `input.not-found`, `environment.io`.

### `tx drop` {#tx-drop}

Undo one tx operation and every later one, restoring the tx state before it.

Later operations build on earlier ones, so dropping one drops everything after it too.

| Argument | Type | Default | Description |
|---|---|---|---|
| `tx` (positional, required) | string |  | The tx. |
| `tx_op` (positional, required) | integer |  | The first operation to drop, from tx status. |

| Output field | Description |
|---|---|
| `tx` | The tx. |
| `dropped` | The dropped tx_op numbers, ascending. |

Errors: `argument.invalid`, `input.not-found`, `environment.busy`, `environment.io`.

### `tx rebase` {#tx-rebase}

Re-apply a tx's operations onto files that changed outside it, moving its base forward.

Only operations touching drifted paths (or paths an earlier re-applied operation rewrote) are re-run, from their recorded arguments. Content-based operations (--old, --between, --match, replace, apply, json writes) search again; position-based ones (--range, --symbol, --expect-hash) are not re-run and make the rebase a conflict. On any conflict the tx is left unchanged (exit 2).

| Argument | Type | Default | Description |
|---|---|---|---|
| `tx` (positional, required) | string |  | The tx. |

| Output field | Description |
|---|---|
| `tx` | The tx. |
| `rebased` | The drifted paths whose base moved to the current file. |

Errors: `input.not-found`, `refusal.target-changed`, `environment.busy`, `environment.io`.

### `tx commit` {#tx-commit}

Write a whole tx to the working tree as one journaled op, or report its conflicts.

Atomically write every staged path. A written path whose file changed since the tx first touched it is a write conflict (repair: tx rebase); a file read through the tx that changed since is a read conflict (repair: read it again with --tx, or --ignore-stale-reads). Any conflict writes nothing (exit 2). The op_id undoes the whole tx.

| Argument | Type | Default | Description |
|---|---|---|---|
| `tx` (positional, required) | string |  | The tx. |
| `--ignore-stale-reads` | flag |  | Commit despite read conflicts. |
| `--max-diff-lines` | integer | `200` | Diff lines shown per change. |

| Output field | Description |
|---|---|
| `changes` | [{path, action, from?, hash_before, hash_after, diff?, diff_truncated?}], as every write reports. |
| `op_id` | The op recording the commit; null for an empty tx. |

Errors: `input.not-found`, `refusal.target-changed`, `environment.busy`, `environment.io`.

### `tx abort` {#tx-abort}

Discard a tx without touching the working tree.

| Argument | Type | Default | Description |
|---|---|---|---|
| `tx` (positional, required) | string |  | The tx. |

| Output field | Description |
|---|---|
| `tx` | The tx. |
| `discarded` | The paths the tx had staged, sorted. |

Errors: `input.not-found`, `environment.busy`, `environment.io`.

