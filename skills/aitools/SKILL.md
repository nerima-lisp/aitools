---
name: aitools
description: This skill should be used when reading, searching, or editing files in a workspace and the `aitools` binary is available. Use it instead of cat, head, tail, grep, rg, find, ls, sed, perl, awk, sort, diff, jq, tar, unzip, base64, date, ps, env, and read-only git for file and text work, because aitools returns one JSON object per call, guards every write with a hash or count, journals writes for undo, masks secrets in output, and refuses writes outside the workspace. Use it for multi-file changes that must land together (tx, batch --atomic), for running a program without a shell (run, bg, wait), and whenever a shell habit would parse text output by hand.
version: 0.1.1
---

# aitools

## Purpose

Provide usage patterns for `aitools`, a Common Lisp CLI that replaces the shell tools an agent reaches for
during file and text work. Every call prints exactly one JSON object, every write states what it expects
the file to look like, and every committed write is one journal operation that `aitools undo` reverts.

## Overview

**Output and exit codes.** Success goes to standard output, errors to standard error, each exactly one JSON object:
`{"schema_version":1,"status":"ok"|"partial"|"error","command":"<as typed>", ...}`. Branch on the exit code
first, then on `error.code`. The exit codes come from `data/domain/protocol/error-catalog-data.lisp`:

- 0: `status:"ok"`. `run` also exits 0 whenever the child started, even on timeout; the child's own status
  is the `exit_code`, `signal`, and `timed_out` fields.
- 3: `status:"partial"`. A result was cut at a command-specific limit: `read --max-lines`, search/find/table/archive limits such as `--limit`, `json get --max-bytes`, or `run --grep-limit`.
  The output is a valid prefix with `truncated:true`; `next_commands` holds the invocation that continues it.
  Writes never exit 3.
- 2: the file is not in the state the call assumed: `selection.no-match`, `selection.ambiguous`,
  `selection.count-mismatch`, or `refusal.target-changed` (a hash guard, `tx commit`, `tx rebase`, or `undo`
  conflict). Re-read, then retry with a fresh selection, hash, or count.
- 1: every other code, including the other refusals (`refusal.redacted-input`, `refusal.outside-workspace`,
  `refusal.exists`, `refusal.not-a-file`, `refusal.too-large`), `argument.invalid`, `input.*`, and
  `environment.*`.

**Error envelope.** `error.code`, `error.message`, `error.exit_code`, and `error.repairs` (never empty), plus
`candidates`, `diagnostics`, or `conflicts` when relevant. `repairs[].command` is the aitools invocation to
run next. Some repairs name only a command (`aitools read` for `cat`) rather than a complete invocation.

**Write output.** `changes[]` of `{path, action, hash_before, hash_after, diff}`, then `op_id` (for `undo` and
`diff --op`), or `tx` and `tx_op` under `--tx`, or `dry_run:true` under `--dry-run`. `hash` in `read` and
`info` output is the SHA-256 value `--expect-hash` compares against.

**Workspace.** `--root <dir>` selects the workspace (write boundary, state directory, ignore rules); without
it aitools uses the enclosing git root, then the working directory. Relative paths still resolve against
the working directory. Reads may leave the workspace; writes outside it, through a symlink that leaves it,
or into its `.git/` fail with `refusal.outside-workspace`. The state directory (journal, tx, bg logs) lives
outside the root and is not writable either; the only extra writable area is the one `aitools mktemp`
returns. Global options `--root` and `--lock-timeout` go before or after the command name.

**Secrets.** Every string in every envelope passes through a secret mask (PEM blocks, `Bearer` tokens, AWS key
ids, `ghp_`/`sk-`/`xox?-` tokens, and values assigned to keys such as `password`, `token`, `api_key`).
A match becomes `[REDACTED_SECRET]` and the envelope gains `redactions`. A write whose input contains
`[REDACTED_SECRET]` fails with `refusal.redacted-input`; there is no bypass.

**Standard input** is read only when `--stdin` is given. `--stdin-data '<text>'` passes the same payload inline
and is what `history` records.

**Discovery.** `aitools schema` lists every command; `aitools schema <command>` returns its arguments, output
fields, and declared error codes. Treat that output as the reference when this file and the binary differ.

## Commands

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

JSON by RFC 6901 pointer (jq), tables by column (awk/cut/sort/uniq -c). Writes take the same
guards as text edits.

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

In-place text edits (sed/perl/patch/sort). Selectors: --old (exact, then whitespace-insensitive),
--range S:E|S:|N, --symbol NAME [--kind], --between START-RE END-RE [--exclusive], --match RE [--invert];
one per call. Position selectors (--range, --symbol) need --expect-hash; selections that can match many
lines (--match, replace) need --expect-count, except under --dry-run, which reports it as `expect_count`.

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

### Journal and transactions

Every committed write is one op. `undo` refuses (exit 2) when a later change touched the same
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
aitools tx commit tx-20260926T011551Z-67a7c1ac --ignore-stale-reads
aitools tx abort tx-20260926T011551Z-67a7c1ac
aitools batch --stdin --atomic
aitools batch --stdin --continue-on-error
```

### Processes

Run programs from an argv after `--`, never through a shell. `run` keeps the first --head and last
--tail lines of each stream; --timeout (default 120s) sends SIGTERM to the process group, then SIGKILL.
`bg` starts detached processes whose logs live in the state directory; `wait` blocks on one condition
(default timeout 60s, then environment.timeout).

```sh
aitools run -- make test
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

Read-only git. There is no aitools command that commits, pushes, or changes refs.

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

Host facts (uname/env/which/ps/lsof), time (date), and byte and number utilities
(base64/xxd/bc/uuidgen/openssl rand). `sys env` masks secret-named variables.

```sh
aitools sys info
aitools sys env LANG
aitools sys tools git sbcl jq
aitools sys procs sbcl --limit 20
aitools sys ports
aitools time now --tz Asia/Tokyo
aitools time convert 1790475971 --to iso8601 --tz UTC
aitools time convert now --add 90m --to epoch_ms
aitools time diff 2026-09-26T00:00:00Z 2026-09-27T12:30:00Z
aitools util encode base64 --content 'hello'
aitools util decode base64 --content 'aGVsbG8='
aitools util decode hex --content-file blob.hex --to out/blob.bin
aitools util redact --content-file .env
aitools util tokens --content-file README.md
aitools util calc '(2**64 - 1) / 3' --decimals 4
aitools util calc -- -2**2
aitools util uuid --kind v7 --count 3
aitools util random --length 24 --alphabet base64url
```

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

Stage several writes, review the combined diff, then commit as one op. On a commit conflict
(exit 2) run the repair it names: `tx rebase` for write drift, a re-read with --tx for a stale read.

```sh
aitools tx begin --name rename-helper
aitools edit src/a.lisp --old 'old-helper' --new 'new-helper' --tx tx-20260926T011551Z-67a7c1ac
aitools edit src/b.lisp --old 'old-helper' --new 'new-helper' --tx tx-20260926T011551Z-67a7c1ac
aitools tx status tx-20260926T011551Z-67a7c1ac
aitools tx diff tx-20260926T011551Z-67a7c1ac
aitools tx commit tx-20260926T011551Z-67a7c1ac
```

### Atomic batch

One call, all or nothing: `batch --stdin --atomic` reads a JSON array of argv arrays (without
the leading `aitools`), runs them in a tx, and commits once. Elements may not read --stdin (use
--content or --stdin-data) and may not carry --tx. The single op_id undoes the whole batch.

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
aitools search 'timeout' --limit 15
aitools search 'timeout' --limit 60 --output matches
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

Apply a unified diff to every file or none; preview first.

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
| rm, rmdir | `aitools delete` |
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
| which | `aitools sys tools NAME...` |
| ps | `aitools sys procs` |
| lsof | `aitools sys ports` |
| date | `aitools time now`, `convert`, `diff` |
| base64, xxd | `aitools util encode`, `decode`; `aitools read --as hex` for a dump |
| bc, expr | `aitools util calc` |
| uuidgen | `aitools util uuid` |
| openssl rand | `aitools util random` |
| patch, git apply | `aitools apply --stdin` |
| a shell pipeline or redirection to run a program | `aitools run -- ARGV...`; `aitools bg start` for long-lived processes |
| several files must change together | `aitools tx begin` ... `tx commit`, or `aitools batch --stdin --atomic` |

## Best practices

- **critical**: Branch on the exit code before parsing: 0 use it, 3 continue with next_commands, 2 re-read and retry with a fresh selection/hash/count, 1 read error.code and run a repair.
- **critical**: Run `repairs[].command` as the next step; when a repair names only a command, run `aitools schema <command>` to complete the invocation.
- **critical**: Pass --expect-hash from the read you based a position edit on; never reuse a hash across your own intervening writes (use `hash_after` from the write output instead).
- **critical**: A bare `--expect-hash HASH` guards only the first target of a write. When a write touches a second file (`move-lines --to` with a position selector, an overwrite destination), give that file its own `--expect-hash PATH=HASH`.
- **high**: Prefer --old for single edits: it needs no hash, and a zero or multiple match fails with candidates rather than editing the wrong place.
- **high**: Run writes with --dry-run first when the selection is broad, then repeat the same command without it (adding `--expect-count` from the dry run's `expect_count` for --match or replace).
- **high**: Keep each write's op_id; `aitools undo OP_ID` reverts exactly that op and `aitools diff --op OP_ID` shows its full diff.
- **high**: Use a tx or `batch --atomic` for any change that spans files and must not land half-done.
- **medium**: Pass --stdin only when you actually pipe input; prefer --stdin-data or --content so the call is self-contained and replayable.
- **medium**: Put temporary files under the path `aitools mktemp` returns; everything else outside the workspace is read-only.
- **medium**: Give `run` an explicit --timeout sized to the job; its exit code is 0 whenever the child started, so read `exit_code` and `timed_out`.

## Anti-patterns

- **Shell text tools**: Calling cat/grep/sed/find/jq/tar through a shell and parsing their text output. Instead: Use the aitools command from the replacement table; its JSON has stable field names and a hash for the next guard.

- **Copying redacted output**: Writing text copied from output that contains [REDACTED_SECRET]. Instead: Leave the secret where it is and edit around it with --old on neighboring text, or copy the file with `aitools copy`; the write is refused anyway (refusal.redacted-input).

- **Unguarded position edit**: Editing by --range or --symbol from a stale read, or guessing a hash. Instead: Run `aitools info PATH` (or `aitools read`) and pass its `hash` as --expect-hash.

- **Raising limits to escape partial**: Answering exit 3 by setting --max-lines or --limit to a huge value. Instead: Follow next_commands, or narrow with a selector, --glob, or a more specific pattern.

- **Writing into git or state**: Editing files under .git/ or the aitools state directory, or writing outside --root. Instead: Use `aitools git ...` for repository facts and `aitools mktemp` for scratch space.

- **Sequential multi-file writes**: Issuing dependent writes to several files one by one and hoping none fails midway. Instead: Stage them in a tx or send them as one `batch --stdin --atomic`.

## Rules (critical)

- Never pass text containing [REDACTED_SECRET] to a write.
- Every --range or --symbol write carries --expect-hash; every --match or replace write carries --expect-count.
- Never retry an exit-2 failure with the same arguments; re-read first.
- Never hand a program to a shell string; `run`, `bg start`, and `batch` take argv arrays.

## Rules (standard)

- Use `aitools schema COMMAND` rather than guessing an option.
- Give --root explicitly when the working directory is not inside the intended workspace.
- Commit or abort every tx you begin; open transactions never expire.

## Workflow

### Orient

Know the workspace and the exact text before changing it.

1. Run `aitools overview` or `aitools find --depth 1` for the layout.
2. Locate code with `aitools search`, `aitools code defs`, or `aitools code outline`.
3. Read the target with a selector; keep the returned `hash`.

### Change

Make a guarded, reviewable write.

1. Pick the narrowest selector (--old first) and the guard it requires.
2. Preview broad writes with --dry-run and check the diff and counts.
3. For several files, stage in a tx and inspect `tx diff` before `tx commit`.

### Verify

Confirm the result and keep a way back.

1. Run `aitools check` on JSON and Lisp files you touched, and the project's tests with `aitools run`.
2. Record the op_id; if the change is wrong, `aitools undo OP_ID --dry-run`, then `aitools undo OP_ID`.

## Error escalation

- **low**: selection.no-match or selection.ambiguous (exit 2): read the candidates, widen --old with a neighboring line or switch to a guarded --range.
- **low**: input.not-found: run the `aitools find` repair; check whether the relative path resolves from the working directory.
- **medium**: refusal.target-changed (exit 2): someone else wrote the file; re-read, re-derive the edit, and pass the new hash. In a tx, follow the tx rebase or re-read repair.
- **medium**: environment.busy: another writer holds the workspace lock; the repair repeats the same command, optionally with a longer --lock-timeout.
- **high**: refusal.outside-workspace or refusal.redacted-input: the write is aimed at the wrong place or carries masked text; do not look for another spelling, change the target or the input.
- **critical**: internal.unexpected, or `recovered` entries reporting a discarded operation: stop writing, report the envelope, and verify the affected files with `aitools read` and `aitools history`.

## Constraints

- Must: Read the exit code and, on failure, error.code before anything else.
- Must: Guard position-based writes with --expect-hash and multi-match writes with --expect-count.
- Must: Use a tx or batch --atomic for multi-file changes that must be all or nothing.
- Avoid: Shell text tools for file work when an aitools command covers it.
- Avoid: Writing [REDACTED_SECRET] or writing outside the workspace, into .git/, or into the state directory.
- Avoid: Reading standard input implicitly; pass --stdin only when input is piped.
