# Architecture

aitools is cut vertically into 13 bounded contexts (5 core, 8 feature), and
each context is split into the DDD layers it needs. Adding or changing a
command touches one feature context rather than a layer that spans every
context. The layout follows nshell's architecture.

## Contexts

| Context | Layers present | Responsibility |
|---|---|---|
| core/kernel | domain | Shared value types: path, selector, guard, duration, size, digests and the content hash, token estimate, unified diff generation, parsing, and application. |
| core/protocol | domain, application, infrastructure | Envelopes, the error catalog, secret masking, the schema model, the command registry, unknown-name repairs, the streaming JSON writer. |
| core/workspace | domain, application, infrastructure | Root resolution, the write boundary, native `.gitignore` evaluation, the parallel ordered scan. |
| core/text | domain, application, infrastructure | Byte decoding, line index, binary sniffing, line endings and BOM, charsets and encoding guesses, MIME, the language table, and the byte-format codecs (deflate, gzip, zip, tar). |
| core/store | domain, application, infrastructure | State directory, locks, the write protocol and recovery, the journal, blobs, the tx overlay. |
| feature/search | domain, application, infrastructure, presentation | `search`, `find`, `code outline`, `code defs`, `code refs`, `overview` |
| feature/inspect | domain, application, infrastructure, presentation | `read`, `info`, `check`, `diff`, `json get/select/diff`, `table read/agg`, `archive list/read`, `snapshot create/diff` |
| feature/edit | domain, application, infrastructure, presentation | Every write command, `edit` through `archive create` in the [command index](commands.md#command-index) |
| feature/journal | domain, application, infrastructure, presentation | `history`, `undo`, the `tx` group |
| feature/process | domain, application, infrastructure, presentation | `run`, `wait`, the `bg` group |
| feature/vcs | domain, application, infrastructure, presentation | The read-only `git` group |
| feature/env | domain, application, infrastructure, presentation | The `sys` and `time` groups |
| feature/util | domain, application, infrastructure, presentation | The `util` group |

`schema` and `batch` read or run commands of every context, so they belong to
no context; they live in the composition root, `src/schema.lisp` and
`src/batch.lisp`.

A CLI group can span contexts: `json get` is in inspect and `json set` in
edit. CLI groups follow what an agent would guess; contexts follow whether a
command reads or writes.

### Placement decisions

- **Byte codecs in core/text.** `archive list` and `archive read` (inspect)
  and `archive extract` and `archive create` (edit) all need deflate, gzip,
  zip, and tar. A feature domain may not depend on another feature context,
  so the codecs live in `packages/core/text/src/domain/codec-*.lisp`, where
  both can reach them.
- **Command registry in core/protocol.** A presentation layer may reference
  protocol but not the composition root, and presentation modules load
  before `aitools/cli`. So `make-command-registry` and `register-command`
  live in `packages/core/protocol/src/application/command-registry.lisp`.
  The registry treats cl-cli command values as opaque; only
  `finalize-app-commands` (`src/registry.lisp`) interprets them, which keeps
  the `aitools` system free of cl-cli.
- **core/text has an application layer.** `call-with-sniffed-octets/k` and
  `call-with-text-file/k`
  (`packages/core/text/src/application/source.lisp`) give every context one
  way to read a file's bytes through a port, deciding text or binary from
  the first bytes over a single open.
- **The streaming JSON writer in protocol infrastructure calls json-kit.**
  json-kit is otherwise a domain-layer kit, but writing an envelope directly
  to the output stream, rather than building the whole document as a string
  first, requires calling `json-kit:write-json` where the stream is. The
  structure test allows
  `JSON-KIT` in infrastructure for this reason
  (`packages/core/protocol/src/infrastructure/json-writer.lisp`).

## Layers and dependency direction

| Layer | Holds | May reference |
|---|---|---|
| domain | Values, rules, pure functions; no I/O | Its own domain, every core context's domain, the pure kits `cl-regex-kit`, `json-kit`, `cl-codec-kit` |
| application | Use cases, port definitions, continuation-based flow | Its own domain and application, core domains and applications, and (feature contexts only) other feature contexts' applications |
| infrastructure | Port implementations (adapters) | Its own domain and application, `host-kit`, `process-kit`, `vcs-kit`, `cl-concurrent-kit`, `cl-boundary-kit`, `sb-posix`, and `json-kit` |
| presentation | cl-cli command definitions, argument conversion | Its own application, `aitools.protocol.domain`, `aitools.protocol.application`, `cl-cli` |

Each context/layer pair is one package, `aitools.<context>.<layer>`
(`aitools.search.domain`). Table data lives in the shared `aitools.data`
package, filled by `data/<layer>/<context>/*-data.lisp` files that each
export their own symbols.

Secret format prefixes and assignment key names live in
`data/domain/protocol/redaction-patterns-data.lisp`. The protocol matcher
derives its first-character dispatch table from those values when the system
loads. Add a pattern there and cover its masking behavior in the protocol
tests; no scanner branch needs a matching edit.

### The structure test

`t/integration/structure-test.lisp` enforces the table. For every
`packages/<core|feature>/<context>/src/<layer>/*.lisp` file, it strips
comments and string literals and looks for package-qualified references
(`name:` or `name::`) to any package it knows. A reference that the file's
layer may not make is a violation. The rules beyond the table:

- A core context's application may reach only core applications, never a
  feature context.
- Only presentation may reference `CL-CLI`, so the `aitools` system, which
  holds every domain, application, and infrastructure layer, never depends
  on cl-cli.

The test also runs against fixtures under `t/integration/fixtures/`: three
violating trees (a domain file referencing an effectful kit, a core file
referencing a feature context, an application file referencing another
context's domain) must fail, and a clean tree must pass.

The scan covers `packages/` only; the composition root in `src/` may
reference every layer.

## Ports and the composition root

Side effects reach application code through ports: structures whose slots
hold plain functions, such as `search-ports`
(`packages/feature/search/src/application/ports.lisp`) or `store-io`
(`packages/core/store/src/application/port.lisp`). Each feature context's
infrastructure exports a constructor,
`make-production-<context>-ports (&key ... &allow-other-keys)`, that builds
the production port without doing I/O.

`src/context-registration.lisp` is the only place that sees both
presentation and infrastructure. For each feature context whose
presentation package is loaded, it calls the context's
`make-production-<context>-ports` with four shared pieces (a
state-directory function, the workspace host, the text source, and
`make-posix-store`) and passes the result to
`register-<context>-commands (registry ports)`. Nothing is looked up
through a global at run time.

Application code holds closure ports rather than cl-boundary-kit objects for
two reasons:

- Only infrastructure may depend on cl-boundary-kit (see the layer table
  above), and the structure test enforces it. A port made of plain functions keeps
  application code free of the kit, while infrastructure builds those
  functions from cl-boundary-kit boundaries (the env context's clock,
  environment, filesystem, and host-info in
  `packages/feature/env/src/infrastructure/production-ports.lisp`; the
  store's clock and sleeper in
  `packages/core/store/src/infrastructure/posix-io.lisp`).
- cl-boundary-kit's filesystem shape (whole-file read, store, rename,
  delete) cannot express the write protocol, which needs exclusive creation
  with fsync, a separately synced append, `lstat` without following
  symlinks, `chmod`, symlink creation, and `flock`. The store defines its own
  port with one closure per primitive.

Unit tests build the same port structures from fakes.
`with-text-boundaries` and `with-workspace-boundaries` install the text
source and workspace host in a cl-boundary-kit boundary context; the
integration tests use them, and the composition root passes ports
directly instead.

## Command handler contract

A presentation handler converts parsed options into a flow call wrapped in
`call-with-command-result/k`
(`packages/core/protocol/src/application/command-result.lisp`). The flow
receives three continuations and calls exactly one: `on-ok` or `on-partial`
with an alist of output fields in envelope order, or `on-error` with
`(code message &key candidates diagnostics conflicts repairs)`. A flow never
writes to a stream and never chooses an exit code. `src/dispatch.lisp` turns
the result into an envelope and an exit code, and adds `recovered`.

`dispatch` calls cl-cli's `parse-argv` directly instead of `run-app`,
because `run-app` prints usage errors and help as plain text. Every exit
from `dispatch`, including an uncaught condition, writes one JSON envelope.

## State directory

The store keeps all state outside the working tree, under
`$XDG_STATE_HOME/aitools/` (or `~/.local/state/aitools/`), one directory
per workspace:

```text
<state>/<workspace-id>/
  lock                  workspace lock (flock)
  commit/               intent records of in-flight writes
  journal/ops.jsonl     one line per committed operation
  blobs/                contents named by content hash, shared by journal and tx
  tx/<tx-id>/           open transactions
  snapshots/ bg/ tmp/   snapshot records, bg logs, mktemp area
```

The workspace id is the root directory's name followed by the first 16 hex
digits of the SHA-256 of the root's real path (`workspace-id` in
`packages/core/store/src/domain/layout.lisp`). The only files aitools
creates in the working tree are the write protocol's temporary files,
`.aitools-<op_id>-<n>.tmp`, which every scan excludes.

The store creates its state files with mode `0600` and its directories with
mode `0700`, opening files `O_CREAT|O_EXCL|O_NOFOLLOW`, so the state is
private to the owner and a pre-planted symlink cannot redirect a create.
The bg records store a redacted copy of the launched argv, so
display-only state holds no raw secrets.

The journal keeps the newest 20 operations per path
(`+retained-generations+` in `packages/core/store/src/domain/journal.lisp`).
The workspace lock is an exclusive `flock(2)` on `lock`, taken by polling a
non-blocking attempt with a backoff from 5 ms to 100 ms until
`--lock-timeout` expires (`packages/core/store/src/application/lock.lisp`).
The OS releases it when the process exits.

## CPS audit

aitools uses continuation-passing style at control boundaries and plain
return values for computations with one outcome. Control boundaries are
traversals (an emit continuation per element), operations with several
outcomes (a `name/k` function with one continuation per exit), the command
boundary (ok, partial, and error continuations), and resources that must be
released on every exit (a `call-with-X` function, with a `with-X` macro as
sugar). The tables list the applied sites per context, found with:

```sh
rg -n '^\(def(un|macro) +([a-z0-9%-]*/k|call-with-[a-z0-9-]*|with-[a-z0-9-]*)\b' packages src
```

Names starting with `%` are internal helpers of the listed functions and are
omitted.

### Multiple exits (`name/k`) and resources (`call-with-X`)

| Context | `/k` functions (multiple exits) | `call-with-X` resources |
|---|---|---|
| kernel | `apply-hunks/k` (hunk applied, conflict) | none |
| protocol | none | `call-with-command-result/k` (ok, partial, error; returns the command result) |
| workspace | `call-with-resolved-root/k`, `call-with-workspace-boundary/k` (inside, outside), `resolve-user-path/k` (inside, outside) | `call-with-workspace-scan/k` (emit per entry), `call-with-ordered-mapper` (worker pool), `with-workspace-boundaries` |
| text | `decode-utf8-strict/k`, `decode-octets/k`, `encode-string/k` | `call-with-sniffed-octets/k` (text, binary, missing, unreadable, too large), `call-with-text-file/k`, `with-text-boundaries` |
| store | `plan-changes/k`, `commit-changes/k`, `recover/k` (rolled forward, discarded, none, busy), `undo-op/k`, `tx-begin/k`, `tx-status/k`, `tx-stage/k`, `tx-record-read/k`, `tx-drop/k`, `tx-rebase/k`, `tx-diff/k`, `tx-commit/k`, `tx-abort/k` | `call-with-workspace-lock/k` and `with-workspace-lock`, `call-with-tx-lock/k` and `with-tx-lock` (acquired, timeout), `call-with-tx-view/k`, `call-with-prepared-intent/k` and `with-prepared-intent` (intent record and write temp files), `call-with-temp-file` and `with-temp-file`, `call-with-temp-dir` and `with-temp-dir` |
| search | `build-matcher/k`, `search/k`, `find/k`, `code-outline/k`, `code-defs/k`, `code-refs/k`, `overview/k`, `scan-options/k` | `call-with-session/k`, `call-with-regex-budget/k` |
| inspect | `parse-selector-options/k`, `resolve-selector/k`, `resolve-line-selector/k`, `text-view/k`, `record-read/k`, `compile-pattern/k`, `resolve-json-pointer/k`, `make-comparison/k`, `parse-json-document/k`, `decode-charset-lines/k`, `parse-table/k`, `table-comparison/k`, `open-archive/k`, `archive-item-content/k`, `decode-snapshot/k`, `call-with-regex-limit/k` (result, step limit) | `call-with-inspect-context/k`, `call-with-readable-file/k`, `call-with-inspect-file/k`, `call-with-target-text/k`, `call-with-archive/k` |
| edit | `run-write-command/k`, `check-expect-count/k`, `require-expect-hash/k`, `resolve-extra-path/k`, `read-stdin/k`, `read-stdin-text/k`, `read-stdin-json/k`, `read-input-file/k`, `read-content/k`, `read-content-text/k`, `read-file-octets/k`, `read-document/k`, `parse-selector/k`, `resolve-lines/k`, `scan-files/k`, `compile-pattern/k`, `decode-text-document/k`, `parse-json-text/k`, `find-old/k`, `compile-search-pattern/k` | `call-with-regex-refusals` |
| journal | none of its own; its flows call the store's `/k` functions | none |
| process | `run-command/k`, `wait-until/k`, `wait-command/k`, `bg-start/k`, `bg-logs/k`, `bg-status/k`, `bg-stop/k` | `call-with-bg-log` and `with-bg-log` |
| vcs | `git-status/k`, `git-log/k`, `git-diff/k`, `git-blame/k`, `git-show/k`, `line-window/k`, `diff-files/k` | none |
| env | `resolve-zone/k`, `time-now/k`, `time-convert/k`, `time-diff/k`, `sys-info/k`, `sys-env/k`, `sys-tools/k`, `sys-procs/k`, `sys-ports/k` | none |
| util | `resolve-input/k`, `evaluate-expression/k`, `decode-text/k` | none |
| composition root | `call-with-invocation-workspace/k` (ready, busy, invalid timeout) | none |

### Resources

Each resource is acquired by a `call-with-X` function that releases it on
every exit, with a `with-X` macro as sugar:

| Resource | Form | Used by |
|---|---|---|
| Intent record and the write protocol's temporary files | `call-with-prepared-intent/k`, `with-prepared-intent` (`packages/core/store/src/application/write-preparation.lisp`) | `commit-changes/k` (`write-protocol.lisp`) |
| A temporary file renamed into place | `call-with-temp-file`, `with-temp-file` (`packages/core/store/src/application/files.lisp`) | blob writes (`blobs.lisp`) and whole-file replacement of store files (`files.lisp`) |
| A temporary directory filled, then renamed into place | `call-with-temp-dir`, `with-temp-dir` (`files.lisp`) | `tx-begin/k`, which builds the tx directory under a staging name (`tx.lisp`) |
| The bg log | `call-with-bg-log`, `with-bg-log` (`packages/feature/process/src/infrastructure/host.lisp`) | `bg start` (`bg-launcher.lisp`) |

`call-with-prepared-intent/k` distinguishes three exits. An error before
the commit point removes the temporary files and, best effort, the
incomplete intent record, then lets the error propagate; the workspace was
never touched. A non-error unwind (the fault injector's model of a crash)
leaves the prepared state exactly as a dead process would, for the next
recovery to discard. A normal return is the commit, so a complete intent
record survives for recovery to roll forward. `call-with-temp-file` and
`call-with-temp-dir` remove the temporary path after their function returns
or unwinds; after a successful rename there is nothing left to remove. The
bg log is reopened with `O_APPEND|O_NOFOLLOW` at mode `0600` and closed on
any exit.

The paths `mktemp` creates are not resources: they are the command's result
and outlive the call by design. `mktemp` removes entries of its area older
than 7 days when it runs
(`packages/feature/edit/src/application/mktemp.lisp`).

### Traversals with emit continuations

These call a function per element; the function can return `:stop` to end
the traversal where the traversal supports it.

| Function | Elements | File |
|---|---|---|
| `call-with-workspace-scan/k` | Directory entries, in path order | `packages/core/workspace/src/application/scan.lisp` |
| `map-lines` | Lines of a byte buffer | `packages/core/text/src/domain/line-index.lisp` |
| `map-matches`, `map-selected-lines` | Regex matches and selected lines | `packages/feature/search/src/domain/matcher.lisp` |
| `map-word-occurrences` | Identifier occurrences for `code refs` | `packages/feature/search/src/domain/code.lisp` |
| `map-journal-entries` | Journal entries | `packages/core/store/src/application/journal.lisp` |
| `map-log-records`, `map-blame-lines` | `git log` and `git blame` output | `packages/feature/vcs/src/domain/log.lisp`, `blame.lisp` |
| `scan-lisp-delimiters` | Delimiters for `check --format lisp` | `packages/feature/inspect/src/domain/lisp-scan.lisp` |

The search hot path declares its per-line continuations `dynamic-extent`
(`packages/feature/search/src/domain/results.lisp`, `matcher.lisp`), and
`t/perf/search-allocation-test.lisp` checks that allocation does not grow
with the number of non-matching lines. See
[Benchmarks](benchmarks.md#allocation).

### Deliberately value-returning

| Computation | Function | File |
|---|---|---|
| Content hash and digests | `content-hash`, `sha256-hex`, `sha1-hex`, `md5-hex` | `packages/core/kernel/src/domain/digest.lisp` |
| Token estimate | `approx-token-count` | `packages/core/kernel/src/domain/token-estimate.lisp` |
| Masking one string | `redact-secrets` | `packages/core/protocol/src/domain/redaction.lisp` |
| Diff generation (LCS) | `generate-diff-hunks`, `generate-unified-diff` | `packages/core/kernel/src/domain/unified-diff.lisp` |
| Diff parsing | `parse-unified-diff` | `packages/core/kernel/src/domain/unified-diff-patch.lisp` |
| `transform` operations | `transform-lines` | `packages/feature/edit/src/domain/transform.lisp` |
| Compression | `inflate`, `deflate` | `packages/core/text/src/domain/codec-deflate.lisp` |
| Envelope construction | `make-ok-envelope`, `make-error-envelope` | `packages/core/protocol/src/domain/envelope.lisp` |

Each has a single outcome (or signals a condition that its caller's `/k`
function converts), so a continuation would add a call without adding a
branch.

## ASDF systems

`aitools.asd` defines the standard three systems:

- `aitools`: `data/` and every context's domain, application, and
  infrastructure layers. It does not depend on cl-cli.
- `aitools/cli`: every presentation layer and the composition root in
  `src/`; `program-op` builds the `aitools` executable.
- `aitools/test`: the cl-weave suites under `t/`.

The delivered binary is built in two SBCL processes (`overrideOutputs` in
`flake.nix`). The first compiles every fasl of the `aitools/cli` closure and
never dumps an image. The second runs `program-op` through cl-nix-forge's
`mkExecutable` over that precompiled tree, so the dumping process loads the
fasls without compiling and the dumped core never shares a heap with the
compiler. See [Development](../project/development.md#building-the-binary).

`aitools.asd` lists each context's files inline, grouped by context in load order: core kernel, protocol, workspace,
text, store, then feature search, inspect, journal, edit, process, vcs, env,
util. See
[Development](../project/development.md#the-component-lists).
