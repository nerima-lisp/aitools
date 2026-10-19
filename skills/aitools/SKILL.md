---
name: aitools
description: Use when the aitools binary is available for workspace file and text operations, guarded edits and transactions, or running external programs with JSON exit status and bounded output. Prefer built-in commands for structured inspection and journaled writes; use run for installed programs whose exact behavior is needed. External programs are not sandboxed and their writes are not journaled.
metadata:
  version: 0.2.0
---

# aitools

## Purpose

Use `aitools` built-ins for structured file and text work, or `run` for arbitrary installed programs.
Every call prints one JSON object. Guarded edits state the expected file content, hash, or match count;
journaled writes return an operation that `aitools undo` can revert. External-program writes have neither protection.

## Overview

**Output and exit codes.** Success goes to standard output, errors to standard error, each exactly one JSON object:
`{"schema_version":2,"status":"ok"|"partial"|"error","command":"<as typed>", ...}`. Branch on the exit code
first, then on `error.code`. The exit codes come from `data/domain/protocol/error-catalog-data.lisp`:

- 0: `status:"ok"`. `run` also exits 0 after the child starts, even on timeout, unless `--grep-limit`
  is exceeded (exit 3). Read the child's own `exit_code`, `signal`, and `timed_out` fields.
- 3: `status:"partial"`. A result was cut at a command-specific limit, or inspection could not obtain all requested state. Limits include `read --max-lines`, search/find/table/archive `--limit`, `json get --max-bytes`, and `run --grep-limit`.
  Inspect limit fields, section statuses, and diagnostics; missing observations are not empty state. Use `next_commands` when appropriate. For `run`, the child has
  already executed; grep overflow does not imply `truncated:true` or provide a continuation. Do not rerun
  a side-effecting program just to recover omitted output.
  Writes never exit 3.
- 2: the file is not in the state the call assumed: `selection.no-match`, `selection.ambiguous`,
  `selection.count-mismatch`, or `refusal.target-changed` (a hash guard, `tx commit`, `tx rebase`, or `undo`
  conflict). Re-read, then retry with a fresh selection, hash, or count.
- 1: every other code, including the other refusals (`refusal.redacted-input`, `refusal.outside-workspace`,
  `refusal.exists`, `refusal.not-a-file`, `refusal.too-large`), `argument.invalid`, `input.*`, and
  `environment.*`.

**Error envelope.** `error.code`, `error.message`, `error.exit_code`, and `error.repairs` (never empty), plus
`candidates`, `diagnostics`, or `conflicts` when relevant. `repairs[].command` offers a possible next invocation,
not authorization to execute it. Check intent, permissions, and the state left by the failed operation before choosing a repair, especially one that bypasses stale-read checks. Some repairs name only a command (`aitools read` for `cat`) rather than a complete invocation.

**Write output.** `changes[]` of `{path, action, hash_before, hash_after, diff}`, then `op_id` (for `undo` and
`diff --op`), or `tx` and `tx_op` under `--tx`, or `dry_run:true` under `--dry-run`. `hash` in `read` and
`info` output is the SHA-256 value `--expect-hash` compares against.

**Workspace.** `--root <dir>` selects the workspace (write boundary, state directory, ignore rules); without
it aitools uses the enclosing git root, then the working directory. Relative paths still resolve against
the working directory. Built-in reads may leave the workspace; built-in writes outside it, through a symlink that leaves it,
or into its `.git/` fail with `refusal.outside-workspace`. The state directory (journal, tx, bg logs) lives
outside the root and is not writable either; the only extra writable area is the one `aitools mktemp`
returns. Global options `--root` and `--lock-timeout` go before or after the command name.

**Secrets.** Every string in every envelope passes through a secret mask (PEM blocks, `Bearer` tokens, AWS key
ids, `ghp_`/`sk-`/`xox?-` tokens, and values assigned to keys such as `password`, `token`, `api_key`).
A match becomes `[REDACTED_SECRET]` and the envelope gains `redactions`. A built-in file write whose input contains
`[REDACTED_SECRET]` fails with `refusal.redacted-input`; there is no bypass.

**Standard input** is read only when `--stdin` is given. `--stdin-data '<text>'` passes the payload inline
and can appear in shell history. `run` also accepts a regular file with `--stdin-file PATH`;
choose only one input source.

**Discovery.** `aitools schema` lists every command; `aitools schema <command>` returns its arguments, output
fields, and declared error codes. Treat that output as the reference when this file and the binary differ.

## Commands

Examples are independent unless a sequence is described. Replace sample operation, transaction,
snapshot, and background-job IDs with the IDs returned by your preceding command. `$HASH` is
the hash from the current target's read, not a literal value or a hash from another file.

### Discovery

Command catalog and workspace orientation; read-only.

```sh
aitools schema
aitools schema edit
aitools schema json get
aitools schema --all
aitools overview --limit 10
aitools --root . overview src
```

### Search and find

Content search (grep/rg), file listing (find/ls/tree/du), and definition lookup. Scans honor
.gitignore and builtin excludes unless --no-ignore; `.git` is always skipped. Default limits: search 15,
find 50.

```sh
aitools search 'defun parse-' src
aitools search 'TODO' --glob '*.lisp' --context 0 --limit 50
aitools search 'old_name' --word --output files
aitools search 'error' --ignore-case --output count src
aitools search --pattern 'FIXME' --pattern 'XXX' --output matches .
aitools search 'a.b(c)' --fixed --lang python
aitools search '^import ' --output files-without-match --glob '*.py'
aitools find '*.json' --type file
aitools find --depth 1
aitools find src --output tree --depth 2
aitools find --type dir --sizes --sort size --limit 10
aitools find --newer 1h --sort mtime
aitools code outline src/app.lisp
aitools code defs build-app src
aitools code defs parse- --prefix --kind function
aitools code refs build-app src --limit 100
```

### Read and inspect

Bounded reads (cat/head/tail/nl), path metadata and hashes, syntax checks, and comparisons.
`read` returns at most --max-lines (default 80) and a `next_commands` entry for the rest. A line longer than
16384 bytes is cut there and listed in `cut_lines` (exit 3); its `next_commands` entry is the `--as hex --bytes` dump of
the rest.

```sh
aitools read src/app.lisp
aitools read src/app.lisp --range 40:80
aitools read src/app.lisp --symbol build-app
aitools read src/app.lisp --symbol build-app --kind function
aitools read CHANGELOG.md --between '^## 0\.2' '^## 0\.1' --exclusive
aitools read server.log --match 'ERROR|WARN' --max-lines 200
aitools read server.log --tail 50
aitools read image.png --as hex --bytes 0:64
aitools read legacy.txt --encoding shift_jis --escape-invisible
aitools info src/app.lisp
aitools info dist/app.tar.gz --digest sha256
aitools info build/out.json --allow-missing
aitools check package.json
aitools check src/app.lisp --format lisp
aitools diff old.txt new.txt
aitools diff old.txt new.txt --output stat --ignore-whitespace
aitools diff dir-a dir-b
aitools diff --op op-20260926T011541Z-4a125e73
```

### Structured data

JSON by RFC 6901 pointer (jq), tables by column (awk/cut/sort/uniq -c). JSON writes accept an
optional `--expect-hash`; `table set` requires it outside `--dry-run` because rows are positional.

```sh
aitools json get package.json /version --raw
aitools json get package.json /dependencies --keys
aitools json select data.json /items --where '/status=active' --pick /id --pick /name
aitools json select data.json /items --where '/size>100' --sort-by /size --desc --limit 5
aitools json select data.json /items --where '/name~^test' --output count
aitools json diff before.json after.json
aitools json set package.json /version '"0.2.0"' --dry-run
aitools json set package.json /keywords/- '"cli"'
aitools json delete package.json /scripts/prepublish
aitools json merge config.json --stdin-data '{"debug":false}' --dry-run
aitools json patch config.json --stdin-data '[{"op":"test","path":"/debug","value":true},{"op":"replace","path":"/debug","value":false}]'
aitools json fmt config.json --indent 2 --sort-keys
aitools table read users.csv --columns name,email --where 'age>=30' --limit 20
aitools table read access.log --format ws --ws-columns 4 --columns 1,4
aitools table read /etc/passwd --format sep --delimiter : --no-header --columns 1,7
aitools table agg orders.csv --group-by customer --sum total --sort sum --desc
aitools table agg access.log --format ws --group-by 1 --count --min-count 10
aitools table set users.csv --row 3 --column email --value new@example.com --dry-run
```

### Archives and snapshots

tar/zip/gz without shelling out. Every entry is validated before extraction; entries that
would leave the workspace or enter `.git/` are refused.

```sh
aitools archive list dist/app.tar.gz
aitools archive read dist/app.tar.gz app/README.md --range 1:40
aitools archive extract dist/app.tar.gz --to vendor/app --dry-run
aitools archive extract dist/app.zip --to vendor/app --entry app/LICENSE
aitools archive create dist/src.tar.gz src docs
aitools archive create dist/src.zip src --format zip --glob '*.lisp'
aitools snapshot create --glob 'src/**'
aitools snapshot diff snap-20260927T022919Z-d57b6f42
```

### Text edits

In-place text edits (sed/perl/patch/sort). Selectors: --old,
--range S:E|S:|N, --symbol NAME [--kind], --between START-RE END-RE [--exclusive], --match RE [--invert];
one per call. Position selectors (--range, --symbol) need --expect-hash; selections that can match many
lines (--match, replace) need --expect-count, except under --dry-run, which reports it as `expect_count`.

`--old` first searches for an exact match. Only if none exists does it compare whole lines after trimming
leading and trailing whitespace, preserving internal whitespace and adjusting replacement indentation.
Multiple matches are refused; an ambiguous exact match does not trigger the fallback.

```sh
aitools edit src/app.lisp --old '(format t "Hello")' --new '(format t "Hi")'
aitools edit src/app.lisp --old '(debug-print x)' --new '' --dry-run
aitools edit src/app.lisp --range 12:14 --new '  (values)' --expect-hash "$HASH"
aitools edit src/app.lisp --symbol old-helper --new '' --expect-hash "$HASH"
aitools edit notes.md --between '^BEGIN GENERATED' '^END GENERATED' --exclusive --new 'generated'
aitools edit config.ini --match '^debug=' --new 'debug=false' --expect-count 1
aitools edit src/app.lisp --stdin-data '{"edits":[{"old":"alpha","new":"beta"},{"old":"gamma","new":"delta"}]}'
aitools insert src/app.lisp --at end --content '(main)'
aitools insert src/app.lisp --after --symbol build-app --content '(defun helper () nil)' --expect-hash "$HASH"
aitools insert README.md --before --match '^## License' --content '## Usage' --expect-count 1
aitools replace 'old-name' 'new-name' src --word --expect-count 12 --dry-run
aitools replace 'old-name' 'new-name' src --word --glob '*.lisp' --expect-count 12
aitools replace '(\w+)_id' '${1:camel}Id' src/models.js --expect-count 3
aitools replace 'v1' 'v2' README.md --fixed --range 1:20 --expect-count 1 --expect-hash "$HASH"
aitools apply --stdin --dry-run
aitools apply --stdin --strip 1 --fuzz 0
aitools transform words.txt --op sort --op unique
aitools transform data.tsv --op sort-numeric --key 2 --delimiter '\t' --range 2: --expect-hash "$HASH"
aitools transform script.sh --op strip-trailing --op eol-lf --op final-newline
aitools transform notes.md --op wrap --columns 72
aitools move-lines src/app.lisp --symbol helper --to src/util.lisp --to-position end --expect-hash "$HASH" --expect-hash "src/util.lisp=$UTIL_HASH"
```

### File operations

Create, move, copy, delete, and change files (cp/mv/rm/mkdir/chmod/ln/touch/mktemp/iconv/split).
Overwriting an existing destination needs --overwrite plus --expect-hash for that destination.

```sh
aitools write notes/todo.md --content '# TODO'
aitools write out.txt --content-file header.txt --content-file body.txt --separator ''
aitools write config.json --overwrite --content '{}' --expect-hash "$HASH"
aitools split big.log --lines 1000 --prefix parts/big. --suffix-digits 4
aitools split chapters.md --at-match '^# ' --dry-run
aitools transcode legacy.txt --from shift_jis --to utf-8
aitools move old/name.lisp new/name.lisp
aitools move draft.md final.md --overwrite --expect-hash "final.md=$HASH"
aitools copy template.lisp src/new.lisp
aitools copy assets public/assets --recursive --max-bytes 100MiB
aitools delete tmp/scratch.txt --expect-hash "$HASH"
aitools mkdir build/reports
aitools chmod bin/run.sh --exec
aitools chmod secrets.env --mode 600
aitools link ../shared/config.lisp src/config.lisp
aitools touch build/.stamp --mtime 2026-09-26T00:00:00Z
aitools mktemp --suffix .json
aitools mktemp --dir
```

`rm` takes exactly one path. Without `--recursive`, it accepts a file, symlink,
or empty directory. For a tree, first run a dry run, inspect the selected paths
and returned count, then repeat with `--expect-count COUNT`:

```sh
aitools rm build/scratch --recursive --dry-run
aitools rm build/scratch --recursive --expect-count COUNT
```

The count includes the target directory, nested directories,
files, and symlinks. Symlinks are removed without following their targets.
The defaults are 1,000 entries and 64 MiB of regular-file content; raise
`--max-entries` or `--max-bytes` only after inspecting the intended target.
These selection limits are not a process-memory limit.

Workspace, home, and filesystem roots, version-control metadata, and state
metadata are refused. Keep the returned `op_id` for undo. Stop external writers
to the target tree before deletion: a dry run does not reserve the tree, and
the journal does not isolate it from other processes. A failure after the durable
commit requires recovery to attempt completion, not automatic rollback.
Recovery stops and retains the intent if a recorded directory was replaced or
cannot be reopened safely. A crash after mode `000` is applied but before it
is synced can require operator intervention; aitools does not widen permissions.
Inspect diagnostics before retrying or modifying the affected paths.

This journal and undo behavior applies to workspace targets. Files in the
`mktemp` scratch area are not journaled or undoable; a failure there can leave
part of a multi-path operation applied.

### Journal and transactions

Every committed journaled write is one op. `undo` refuses (exit 2) when a later change touched the same
paths. A tx stages writes without touching the working tree and commits them as one op; `batch --atomic`
is the one-call form of the same thing.

```sh
aitools history --limit 10
aitools history src/app.lisp
aitools undo op-20260926T011541Z-4a125e73 --dry-run
aitools undo op-20260926T011541Z-4a125e73
aitools tx begin --name rename-helper
aitools edit src/a.lisp --old 'old-helper' --new 'new-helper' --tx tx-20260926T011551Z-67a7c1ac
aitools read src/b.lisp --tx tx-20260926T011551Z-67a7c1ac
aitools tx status
aitools tx status tx-20260926T011551Z-67a7c1ac
aitools tx diff tx-20260926T011551Z-67a7c1ac --max-diff-lines 400
aitools tx drop tx-20260926T011551Z-67a7c1ac 2
aitools tx rebase tx-20260926T011551Z-67a7c1ac
aitools tx commit tx-20260926T011551Z-67a7c1ac
aitools tx abort tx-20260926T011551Z-67a7c1ac
aitools batch --stdin --atomic
aitools batch --stdin --continue-on-error
```

### Processes

Run programs from argv after `--`; arguments are passed directly without shell expansion.
Use `run -- sh -c '...'` only when shell syntax is needed, and keep untrusted data out of the script string.
`run` captures non-interactive output; it does not allocate a terminal or PTY.
`run` keeps the first --head and last
--tail lines of each stream; --timeout (default 120s) sends SIGTERM to the process group, then SIGKILL.
`bg` starts detached processes whose logs live in the state directory; `wait` blocks on one condition
(default timeout 60s, then environment.timeout).

`run` defaults to closed stdin, the current directory, and the inherited environment.
Use mutually exclusive `--stdin`, `--stdin-data TEXT`, or `--stdin-file PATH` for input.
`--stdin-file` rejects FIFOs, devices, and directories; use `--stdin` for piped input.
`--cwd PATH` changes only the child's directory. Relative input-file paths still use the caller's directory.
Repeat `--env KEY=VALUE` to overlay the parent environment; the last value wins and empty values are allowed.
These input, directory, and environment options belong to `run`, not `bg start`.

External programs retain normal filesystem and network access. `--root` is not a sandbox;
external writes bypass guards, transactions, and undo. `--stdout-to` saves raw, unmasked bytes to a new,
non-journaled file. Inspect child `exit_code`, `signal`, and `timed_out` even when aitools exits 0;
inspect `truncated` and `capture_capped` before assuming the returned output is complete.

```sh
aitools run -- make test
aitools run -- awk 'END { print NR }' README.md
aitools run --stdin-data 'hello' -- sed 's/hello/world/'
aitools run --cwd src --env LC_ALL=C -- grep -n TODO main.lisp
aitools run --timeout 600s --grep 'FAIL|ERROR' --tail 40 -- cargo test
aitools run --stdout-to build/report.json -- ./gen-report
aitools bg start --name devserver -- npm run dev
aitools wait --bg bg-1 --pattern 'listening on' --timeout 30s
aitools wait --port 8080 --timeout 30s
aitools wait --file build.log --pattern '^DONE'
aitools wait --bg bg-1 --exit --timeout 10m
aitools wait --duration 2s
aitools bg logs bg-1 --tail 50
aitools bg logs bg-1 --from 4096 --grep ERROR
aitools bg status
aitools bg stop bg-1 --grace 10s
```

### Git

The built-in `git` commands are read-only. Calling git through `run` does not impose that restriction;
obtain authorization before any version-control write.

```sh
aitools git status
aitools git log src/app.lisp --limit 5
aitools git diff --output stat
aitools git diff src --staged --max-lines 200
aitools git diff --ref HEAD~1
aitools git blame src/app.lisp --symbol build-app
aitools git show HEAD:src/app.lisp --range 1:40
```

### Host, time, and utilities

Host facts (uname/env/which/ps/lsof/top), time (date), and byte and number utilities
(base64/xxd/bc/uuidgen/openssl rand). `sys env` masks secret-named variables.

```sh
aitools sys info
aitools sys env LANG
aitools sys tools git sbcl jq
aitools sys procs sbcl --limit 20
aitools sys metrics --limit 10
aitools sys ports
aitools sys network --limit 20
aitools sys disk .
aitools sys snapshot . --process-limit 10 --network-limit 20 --port-limit 20
aitools time now --tz Asia/Tokyo
aitools time convert 1790475971 --to iso8601 --tz UTC
aitools time convert now --add 90m --to epoch_ms
aitools time diff 2026-09-26T00:00:00Z 2026-09-27T12:30:00Z
aitools util encode base64 --content 'hello'
aitools util decode base64 --content 'aGVsbG8='
# Writes decoded bytes to a file; the other utility examples return JSON.
aitools util decode hex --content-file blob.hex --to out/blob.bin
aitools util redact --content-file .env
aitools util tokens --content-file README.md
aitools util calc '(2**64 - 1) / 3' --decimals 4
aitools util calc -- -2**2
aitools util uuid --kind v7 --count 3
aitools util random --length 24 --alphabet base64url
```

`sys env PREFIX` matches a case-sensitive variable-name prefix, not an exact name.
`sys tools` finds commands on PATH and executes their version probes; it is not
only a path lookup. `sys procs PATTERN` matches a case-insensitive substring of
the command line. `sys ports` covers TCP LISTEN sockets only, not open files, UDP,
or established connections.

`sys metrics` returns one JSON snapshot after a one-second CPU sampling interval.
System CPU is a percentage of all logical CPUs; process CPU can exceed 100% and
uses a lifetime average on Linux or a decaying average on macOS, not the system
sampling interval. Memory and resident sizes are bytes. Process entries contain
command names, not arguments, and are sorted by CPU usage. Inspect the reported
basis fields when comparing results. Truncating the process list at the limit
returns partial status and exit code 3; unavailable sources return an error
instead of zero readings. There is no process filter or pagination; if the top
processes do not answer the question, use an authorized external collector rather
than treating a partial list as a complete inventory.

`sys network` reports cumulative interface counters, not traffic rates or
connectivity. `sys disk PATH` reports capacity for the filesystem containing
PATH, not disk I/O or all mounts; available bytes exclude blocks reserved from
unprivileged users, so do not subtract them from total bytes to infer used space.

`sys snapshot PATH` collects info, metrics, network, disk, and TCP listening ports
sequentially, not atomically. Inspect each section's status and observation times;
unavailable or incomplete sections make the result partial (exit code 3), and all
sections unavailable produce an error (exit code 1). PATH selects only the disk
section's filesystem; info still reports the workspace filesystem, which may
differ. The limits bound returned entries, not acquisition memory
or total duration. Source commands have individual 60-second timeouts, but socket
owner scans and OS calls have no timeout; there is no overall deadline.

## Patterns

### Read then guarded edit

Select by content when possible; when you must select by position, carry the hash from the
read you based the edit on.

```sh
aitools code outline src/app.lisp
aitools read src/app.lisp --symbol build-app
aitools edit src/app.lisp --symbol build-app --new '(defun build-app () (make-app))' --expect-hash "$HASH" --dry-run
aitools edit src/app.lisp --symbol build-app --new '(defun build-app () (make-app))' --expect-hash "$HASH"
aitools check src/app.lisp
```

### Counted bulk replace

Count first, then pass the count as the guard. A --dry-run needs no --expect-count: it writes
nothing and reports the selection's count as `expect_count` next to the diff. A real write without
--expect-count fails with a repair that is that --dry-run; a count-mismatch repair is the same
--dry-run, and its `diagnostics[0].actual` is the current count.

```sh
aitools replace 'old-name' 'new-name' src --word --dry-run
aitools replace 'old-name' 'new-name' src --word --expect-count 12
aitools search 'old-name' src --word --output count
```

### Multi-file change in a tx

Stage several writes, review the combined diff, then commit as one op. On a commit
conflict (exit 2), inspect the failure and current state before choosing an
authorized repair. Candidates include `tx rebase` for write drift and a re-read
with --tx for a stale read; a repair is not permission to overwrite a conflict.

```sh
aitools tx begin --name rename-helper
aitools edit src/a.lisp --old 'old-helper' --new 'new-helper' --tx tx-20260926T011551Z-67a7c1ac
aitools edit src/b.lisp --old 'old-helper' --new 'new-helper' --tx tx-20260926T011551Z-67a7c1ac
aitools tx status tx-20260926T011551Z-67a7c1ac
aitools tx diff tx-20260926T011551Z-67a7c1ac
aitools tx commit tx-20260926T011551Z-67a7c1ac
```

### Atomic batch

One staged commit: `batch --stdin --atomic` reads a JSON array of argv arrays (without
the leading `aitools`), runs them in a tx, and commits once. Elements may not read --stdin (use
--content or --stdin-data) and may not carry --tx. The single op_id undoes the whole batch.
Pre-commit failures leave the working tree unchanged. Physical writes are sequential;
after a durable commit, recovery attempts completion instead of rolling it back.
Unsafe directory replacement or inaccessible permission checkpoints stop recovery
with the intent retained, as described in the removal section.

```sh
aitools batch --stdin --atomic < edits.json
aitools history --limit 1
aitools undo op-20260926T011541Z-4a125e73 --dry-run
```

### Paged read

Follow `next_commands` from a partial (exit 3) result instead of raising limits blindly.

```sh
aitools read server.log --max-lines 80
aitools read server.log --range 81:160
```

### Build and serve

Build with a bounded run, then start a server in the background and wait for it.

```sh
aitools run --timeout 900s --grep 'error' -- make
aitools bg start --name web -- python3 -m http.server 8080
aitools wait --port 8080 --timeout 30s
aitools bg logs bg-1 --tail 20
aitools bg stop bg-1
```

### Patch apply

Validate a unified diff before committing it as one journal operation; preview first.

```sh
aitools apply --stdin --dry-run < fix.patch
aitools apply --stdin < fix.patch
aitools git diff --output stat
```

## Shell habits and their aitools replacements

| Shell habit | aitools |
|---|---|
| cat, head, tail, nl | `aitools read` (--range, --tail, --match, --between, --symbol) |
| grep, rg, egrep, fgrep | `aitools search` (--fixed, --word, --invert, --output files, count, matches) |
| ls, fd, find, tree, du | `aitools find` (--depth 1, --output tree, --sizes, --type) |
| diff, cmp, comm | `aitools diff` (--output unified, stat, set); `aitools json diff` for JSON |
| sed, perl for one occurrence | `aitools edit --old ... --new ...` |
| sed, perl for every occurrence | `aitools replace PATTERN REPLACEMENT PATH... --expect-count N` |
| awk, cut | `aitools table read` (--format ws or sep, --columns, --where); `aitools table agg` for sums and counts |
| sort, uniq, tac, shuf | `aitools transform --op sort, unique, reverse, shuffle` |
| tr, dos2unix, expand, unexpand, fold, fmt | `aitools transform --op OP` (upper, lower, eol-lf, tabs-to-spaces, spaces-to-tabs, wrap, reflow) |
| iconv, nkf | `aitools transcode --from ENC --to ENC` |
| cp | `aitools copy` |
| mv | `aitools move` |
| rm, rmdir | `aitools rm` (tree removal needs `--recursive` and a count guard) |
| mkdir | `aitools mkdir` |
| chmod | `aitools chmod` |
| ln | `aitools link` |
| touch | `aitools touch` |
| mktemp | `aitools mktemp` |
| jq | `aitools json get`, `select`, `diff` to read; `json set`, `delete`, `merge`, `patch`, `fmt` to write |
| tar, unzip, zcat, gzip, zip | `aitools archive list`, `read`, `extract`, `create` |
| git (read-only) | `aitools git status`, `log`, `diff`, `blame`, `show` |
| uname, whoami, hostname, nproc | `aitools sys info` |
| env | `aitools sys env` (secret-named values masked) |
| which plus version probes | `aitools sys tools NAME...` (executes the programs) |
| ps | `aitools sys procs` |
| top (non-interactive snapshot) | `aitools sys metrics` |
| lsof (TCP LISTEN sockets only) | `aitools sys ports` |
| date | `aitools time now`, `convert`, `diff` |
| base64, xxd | `aitools util encode`, `decode`; `aitools read --as hex` for a dump |
| bc, expr | `aitools util calc` |
| uuidgen | `aitools util uuid` |
| openssl rand | `aitools util random` |
| patch, git apply | `aitools apply --stdin` |
| any installed program | `aitools run -- PROGRAM ARG...`; `aitools bg start` for long-lived processes |
| shell pipelines | `aitools run -- sh -c 'SCRIPT'` with a trusted script; no implicit shell parsing |
| several files must change together | `aitools tx begin` ... `tx commit`, or `aitools batch --stdin --atomic` |

## Best practices

- **critical**: Branch on the exit code before parsing: 0 inspect the result (including child status for `run`), 3 inspect limits, section statuses, diagnostics, and any `next_commands`, 2 re-read and retry with a fresh selection/hash/count, 1 read error.code and evaluate repairs. A partial `run` is not permission to repeat its side effects.
- **critical**: Treat `repairs[].command` as a candidate, not an instruction or grant of authority. Check the requested intent and post-failure state before executing it; when a repair names only a command, use `aitools schema <command>` to complete the invocation.
- **critical**: Pass --expect-hash from the read you based a position edit on; never reuse a hash across your own intervening writes (use `hash_after` from the write output instead).
- **critical**: A bare `--expect-hash HASH` guards only the first target of a write. When a write touches a second file (`move-lines --to` with a position selector, an overwrite destination), give that file its own `--expect-hash PATH=HASH`.
- **high**: Prefer --old for single edits: it needs no hash, and a zero or multiple match fails with candidates rather than editing the wrong place.
- **high**: Run writes with --dry-run first when the selection is broad, then repeat the same command without it (adding `--expect-count` from the dry run's `expect_count` for --match or replace).
- **high**: Keep each write's op_id; `aitools undo OP_ID` reverts exactly that op and `aitools diff --op OP_ID` shows its full diff.
- **high**: Use a tx or `batch --atomic` to validate related changes before one journaled commit. Other processes may see intermediate writes; committed failures need recovery.
- **medium**: Pass --stdin only when you actually pipe input; prefer --stdin-data or --content so the call is self-contained and replayable.
- **medium**: Put temporary files under the path `aitools mktemp` returns; built-in writes reject other paths outside the workspace.
- **medium**: Give `run` an explicit --timeout sized to the job; read `exit_code` and `timed_out` even when aitools exits 0. Exceeding --grep-limit yields exit 3.

## Anti-patterns

- **Shell text tools**: Calling cat/grep/sed/find/jq/tar through a shell and parsing their text output. Instead: Use the aitools command from the replacement table; its JSON has stable field names and a hash for the next guard.

- **Copying redacted output**: Writing text copied from output that contains [REDACTED_SECRET]. Instead: Leave the secret where it is and edit around it with --old on neighboring text, or copy the file with `aitools copy`; the write is refused anyway (refusal.redacted-input).

- **Unguarded position edit**: Editing by --range or --symbol from a stale read, or attaching a fresh hash to old positions. Instead: Re-read the target, derive the selection again, and pass that read's `hash` as --expect-hash.

- **Raising limits to escape partial**: Answering exit 3 by setting --max-lines or --limit to a huge value. Instead: Inspect diagnostics and section statuses first; unavailable observations need a supported collector or permission, not a larger output limit. For bounded read-only results, follow available next_commands or narrow the selection. For `run`, inspect retained output and child status; do not rerun a side-effecting command to recover output.

- **Writing into git or state**: Editing files under .git/ or the aitools state directory, or writing outside --root. Instead: Use `aitools git ...` for repository facts and `aitools mktemp` for scratch space.

- **Sequential multi-file writes**: Issuing dependent writes to several files one by one and hoping none fails midway. Instead: Stage them in a tx or send them as one `batch --stdin --atomic`.

## Rules (critical)

- Prefer direct argv with `run` and `bg start`. If a shell is required, invoke it explicitly and never interpolate untrusted data into its script.
- Never use an external program to bypass a denied write or claim its changes can be undone by aitools.

## Rules (standard)

- Give --root explicitly when the working directory is not inside the intended workspace.
- Commit or abort every tx you begin; open transactions never expire.

## Workflow

### Orient

1. Run `aitools overview` or `aitools find --depth 1` for the layout.
2. Locate code with `aitools search`, `aitools code defs`, or `aitools code outline`.
3. Read the target with a selector; keep the returned `hash`.

### Change

1. Pick the narrowest selector (--old first) and the guard it requires.
2. Preview broad writes with --dry-run and check the diff and counts.
3. For several files, stage in a tx and inspect `tx diff` before `tx commit`.

### Verify

1. Run `aitools check` on JSON and Lisp files you touched, and the project's tests with `aitools run`.
2. Record the op_id; if the change is wrong, `aitools undo OP_ID --dry-run`, then `aitools undo OP_ID`.

## Error escalation

- **low**: selection.no-match or selection.ambiguous (exit 2): read the candidates, widen --old with a neighboring line or switch to a guarded --range.
- **low**: input.not-found: run the `aitools find` repair; check whether the relative path resolves from the working directory.
- **medium**: refusal.target-changed (exit 2): the target no longer matches the guarded state; this may include your own intervening write. Re-read, re-derive the edit, and pass the new hash. In a tx, inspect the conflict and evaluate the rebase or re-read candidate under the repair-authority rule above.
- **medium**: environment.busy: another writer holds the workspace lock; the repair repeats the same command, optionally with a longer --lock-timeout.
- **high**: refusal.outside-workspace or refusal.redacted-input: the write is aimed at the wrong place or carries masked text; do not look for another spelling, change the target or the input.
- **critical**: internal.unexpected, or `recovered` entries reporting a discarded operation: stop writing, report the envelope, and verify the affected files with `aitools read` and `aitools history`.
