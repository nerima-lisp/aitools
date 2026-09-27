# Transactions

A transaction (tx) stacks writes from several aitools calls without touching
the working tree, lets the agent read the combined result, and then writes
everything at once as one journal operation. A single call that changes
several files (`replace`, `apply`) is already atomic through the write
protocol and does not need a tx.

The store code is in `packages/core/store/src/application/tx.lisp`,
`tx-stage.lisp`, and `tx-commit.lisp`; the commands are in the journal
context (`packages/feature/journal/src/application/tx-flows.lisp`).

## Lifecycle

```sh
aitools tx begin --name docs            # {"tx":"tx-...","name":"docs"}
aitools edit a.txt --old beta --new BETA --tx tx-...
aitools read b.txt --tx tx-...
aitools tx status tx-...                # ops, staged paths, drift, stale reads
aitools tx diff tx-...                  # what commit would write
aitools tx commit tx-...                # one op_id for the whole tx
```

`tx abort <tx>` discards the tx instead. Open transactions never expire;
`tx status` without an argument lists them with their creation time.

A write given `--tx` returns `tx` and `tx_op` (the operation's number inside
the tx) instead of `op_id`:

```json
{"schema_version":1,"status":"ok","command":"edit","changes":[{"path":"a.txt","action":"modified", ...}],"tx":"tx-20260926T011551Z-67a7c1ac","tx_op":1,"strategy":"exact"}
```

## What a tx stores

Each tx lives in `<state>/<workspace-id>/tx/<tx-id>/` (see
[Architecture](architecture.md#state-directory)):

- `index.json`: for each path, `base` (the disk state when the tx first
  touched it) and `staged` (its state inside the tx).
- `meta.json`: the transaction's name and other transaction metadata.
- `ops.jsonl` (or `ops.<generation>.jsonl` after a rebase): one record per
  operation, with its `tx_op`, argv, and the paths it changed.
- `reads.json`: the read set, described below.
- `lock`: a lock that serializes operations on the tx.

File contents live in the workspace's shared `blobs/` directory, the same one
the journal uses. An operation writes its blobs, rewrites the current ops log with
its record added, and then replaces `index.json`; both files are written by
an atomic rename, not appended in place (`%save-tx` in
`packages/core/store/src/application/tx-stage.lisp`). The `index.json` rename
is the operation's commit point: it records the last applied `tx_op`, and
the ops log records beyond it are ignored, so a crash in the middle of an
operation leaves the tx as it was after the previous operation. `tx drop`
replaces `index.json` first, then rewrites the current ops log. Rebase writes a
new generation log and updates the tx metadata/index to use it.

## Reads and writes inside a tx

- A write with `--tx` is validated against the tx state (the staged content
  if there is one, otherwise the disk), exactly as a standalone write would
  be. `--expect-hash` compares with the tx state, so a hash from
  `read --tx` can guard the next write.
- A read with `--tx` returns the tx state. Scans (`find`, `search`, `code`,
  `overview`) show the disk with the tx's additions, deletions, and changes
  applied, and use the tx's own `.gitignore` files.
- `git`, `snapshot`, `run`, `wait`, and `bg` do not see the tx. `mktemp` and
  `undo` do not accept `--tx`.

## Isolation and conflicts

A tx uses snapshot isolation on its write set plus an explicit read set.

- **Write set.** Every path the tx wrote. At commit, a path whose file on
  disk no longer matches `base` is a write conflict.
- **Read set.** A single-file read with `--tx` (`read`, `info`, `check`,
  `json get`, `json select`, `table read`, `archive read`) records the file's
  disk hash. At commit, a recorded file that has changed since is a stale
  read. Reading the file again with `--tx` refreshes the record. Scans are
  not recorded.

`tx status <tx>` reports both before you commit: `drift` lists written paths
that changed on disk, and `stale_reads` lists read paths that changed.

A conflicting commit writes nothing, exits with code 2
(`refusal.target-changed`), and lists each conflict with the repairs that
resolve it:

```json
{"schema_version":1,"status":"error","command":"tx commit","error":{"code":"refusal.target-changed","message":"2 paths changed outside tx tx-20260926T011551Z-67a7c1ac","exit_code":2,
 "repairs":[
  {"action":"rebase","detail":"Re-apply the tx's content-based operations onto the current files.","command":"aitools tx rebase tx-20260926T011551Z-67a7c1ac"},
  {"action":"reread","detail":"Read the changed file again through the tx to refresh the read set.","command":"aitools read b.txt --tx tx-20260926T011551Z-67a7c1ac"},
  {"action":"ignore-stale-reads","detail":"Commit even though files read through the tx have changed.","command":"aitools tx commit tx-20260926T011551Z-67a7c1ac --ignore-stale-reads"}],
 "conflicts":[
  {"path":"a.txt","kind":"write","base":"4fdbc441...","current":"927c9bb4..."},
  {"path":"b.txt","kind":"read","base":"2c8b08da...","current":"27dd8ed4..."}]}}
```

Several transactions may touch the same file; whichever commits later gets
the conflict. Writes outside any tx are not blocked by open transactions.

## Rebase

`tx rebase <tx>` re-runs, from their recorded arguments, the operations that
touched drifted paths, against the current disk content. On success, each
rebased path's `base` moves to the current file:

```json
{"schema_version":1,"status":"ok","command":"tx rebase","tx":"tx-20260926T011551Z-67a7c1ac","rebased":["a.txt"]}
```

Content-based operations search again: `--old`, `--between`, and `--match`
selections, `replace`, `apply`, and the `json` writes. An operation that
used `--range` or `--symbol`, or that carried `--expect-hash` (even with a
content-based selector), is not re-run, because the positions or content it
relied on may have changed; it makes the rebase fail with exit code 2, and
the tx is left unchanged. The repairs then point at `tx status` (to find the operation) and
`tx abort`.

Rebase resolves write conflicts only. A stale read still needs a re-read or
`tx commit --ignore-stale-reads`.

## Drop

`tx drop <tx> <tx_op>` removes the named operation and every later one,
restoring each path's staged state from before that operation:

```json
{"schema_version":1,"status":"ok","command":"tx drop","tx":"tx-20260926T011559Z-e9eaeac3","dropped":[2]}
```

Later operations build on earlier ones, so aitools does not remove an
operation from the middle and keep the ones after it.

## Commit

`tx commit` runs through the same write protocol as any other write:

1. Take the workspace lock and the tx lock.
2. Check the write set and the read set. Any conflict ends the commit
   without writing.
3. Write the staged contents to temporary files, record the intent, and
   apply it.
4. Record the whole tx as one journal operation and delete the tx
   directory.

`aitools undo <op_id>` with the commit's `op_id` reverts the whole tx. A
commit of a tx with no staged changes returns `"op_id":null`.

## Abort

`tx abort <tx>` takes the tx lock and deletes the tx directory. It does not
touch the working tree, and returns the paths the tx had staged:

```json
{"schema_version":1,"status":"ok","command":"tx abort","tx":"tx-20260926T011559Z-e9eaeac3","discarded":["a.txt"]}
```

## `batch --atomic`

`batch --stdin --atomic` is shorthand for a tx: it runs `tx begin`, every
element with `--tx <tx>` added, and `tx commit`, each dispatched exactly as
a standalone call (`src/batch.lisp`). If an element fails or the commit
conflicts, batch runs `tx abort`, writes nothing, and answers with the first
failure's `error.code` and exit code; the per-element envelopes are in
`error.diagnostics`. On success the output has `results`, `tx`, and the
commit's `changes` and `op_id`, so one `undo` reverts the whole batch.

Under `--atomic`, an element may not carry its own `--tx`, and
`--continue-on-error` is refused.
