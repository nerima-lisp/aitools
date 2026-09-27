# JSON output

Every invocation writes exactly one JSON object followed by a newline:
success to standard output, errors to standard error. Object members appear
in a fixed order, and the same input produces byte-identical output
(`make-ok-envelope` and `make-error-envelope` in
`packages/core/protocol/src/domain/envelope.lisp` build ordered objects
rather than hash tables). The writer streams the object to the output
(`write-envelope` in `packages/core/protocol/src/infrastructure/json-writer.lisp`)
instead of building one string first.

The shapes of individual commands' fields are in
[Commands](commands.md). This page covers the fields that are shared.

## Success envelope

```json
{"schema_version":1,"status":"ok","command":"read", ...command fields...,
 "next_commands":["..."], "recovered":[...], "redactions":2}
```

| Field | Presence | Meaning |
|---|---|---|
| `schema_version` | always | `1`. |
| `status` | always | `"ok"` (exit code 0) or `"partial"` (exit code 3). |
| `command` | always | The command as typed: `"read"`, `"json get"`, `"tx commit"`. |
| command fields | per command | Listed in each command's schema. |
| `next_commands` | when non-empty | Complete aitools invocations that continue the result. |
| `recovered` | after a recovery | See [Recovered operations](#recovered-operations). |
| `redactions` | when anything was masked | Number of masked secrets in the whole object. See [Redactions](#redactions). |

`next_commands` is one of the command's own fields, so its position among
them depends on the command. `recovered` follows the command fields, and
`redactions` is always last.

### Partial results

A read cut at a limit returns `status:"partial"`, `truncated:true`, and exit
code 3, with `next_commands` holding the command that reads the rest:

```json
{"schema_version":1,"status":"partial","command":"read","mode":"text","path":"nums.txt","start_line":1,"lines":["1","2","3"],"total_lines":100,"hash":"93d4e5c7...","truncated":true,"encoding_errors":0,"approx_tokens":2,"next_commands":["aitools read nums.txt --range 4:6"]}
```

Totals such as `total_lines`, `total`, and `total_matches` count the whole
input, not only the returned part. A partial result is a success; it has no
`error` member.

### Reads

- Lines come as `start_line` (the 1-based number of the first returned line)
  and `lines` (the line texts, without terminators or a BOM). When lines are
  not contiguous (`read --match`), `line_numbers` gives each line's number.
- Lists come as `items` with `total`, the count before `--limit`.
- A command whose output shape varies with a flag reports that flag's value
  in `mode` (`read --as`, `search --output`, `find --output`).
- Content-returning commands include `approx_tokens`, computed as
  `ceil(characters / 4)` over the returned text, where a multi-byte UTF-8
  character counts once. It is a rough size estimate, not a real tokenizer
  count (`approx-token-count` in
  `packages/core/kernel/src/domain/token-estimate.lisp`).
- `path` in `read`, `info`, and `check` output (and the compared paths in
  `diff` output) is relative to the workspace root when the file's real path is
  inside the root (`.` for the root itself), and the absolute real path when
  it is outside, since reads are not limited to the workspace. A path with no
  real path, such as a dangling symlink, is reported in its absolute form
  (`target-display-path` in
  `packages/feature/inspect/src/application/files.lisp`).
- `hash` is the SHA-256 hex digest of the file bytes. It is the value
  `--expect-hash` compares against, and the value in `hash_before` and
  `hash_after`.

## Write output

Every write command returns the same shape:

```json
{"schema_version":1,"status":"ok","command":"edit",
 "changes":[{"path":"hello.lisp","action":"modified","hash_before":"de2187a5...","hash_after":"5f132d76...","diff":"@@ -1,2 +1,2 @@\n ..."}],
 "op_id":"op-20260926T011541Z-4a125e73","strategy":"exact"}
```

| Field | Meaning |
|---|---|
| `changes[].path` | The changed path, relative to the workspace root. |
| `changes[].action` | `created`, `modified`, `deleted`, `moved`, `mode-changed`, or `linked`. |
| `changes[].from` | For `moved`: the source path. |
| `changes[].hash_before` | The content hash before the write; `null` when the path did not exist. |
| `changes[].hash_after` | The content hash after the write. |
| `changes[].diff` | A unified diff of a text change. |
| `changes[].diff_truncated` | `true` when the diff was cut at 200 lines. |
| `op_id` | The journal operation, for `aitools undo` and `aitools diff --op`. `null` when nothing changed. |
| `tx`, `tx_op` | With `--tx`: the transaction and the staged operation's number, instead of `op_id`. |
| `dry_run` | With `--dry-run`: `true`. There is no `op_id`, and nothing was written. |

Command-specific fields follow (`strategy` for `edit --old`, `inserted_at`
for `insert`, `changes[].count` for `replace`).

A cut diff does not make the write partial. The write has completed, so the
status stays `"ok"` and the exit code 0, and `next_commands` holds
`aitools diff --op <op_id>` to show the whole diff:

```json
{"status":"ok","changes":[{"action":"created","diff_truncated":true, ...}],"op_id":"op-20260926T011609Z-c41512f8","next_commands":["aitools diff --op op-20260926T011609Z-c41512f8"]}
```

(Shortened with `jq`; the real output has every field.)

A write that changes nothing, such as `mkdir` on an existing directory,
returns `"changes":[]` and `"op_id":null`.

`mktemp` is a write command but reports its result as a top-level `path`, the
absolute real path of the new entry (writes below it are allowed), plus
`hash`, rather than a `changes[]` array. That `path` is absolute, unlike the
workspace-relative `changes[].path`. `run --stdout-to` likewise reports an
absolute `{path, bytes}` for the file it writes.

## Error envelope

```json
{"schema_version":1,"status":"error","command":"edit",
 "error":{"code":"selection.no-match","message":"--old matches nothing in nums.txt","exit_code":2,
          "repairs":[{"action":"read","detail":"Read the file to choose a unique, current selection.","command":"aitools read nums.txt"}],
          "candidates":[{"line":1,"text":"1"},{"line":2,"text":"2"},{"line":3,"text":"3"}]}}
```

| Field | Presence | Meaning |
|---|---|---|
| `error.code` | always | `<namespace>.<name>`; see [Errors and exit codes](errors.md). |
| `error.message` | always | A sentence for the agent. |
| `error.exit_code` | always | The process exit code, from the error catalog. |
| `error.repairs` | always, at least one | `{action, detail, command}`; `command` is the aitools invocation to run next. |
| `error.candidates` | when relevant | Near matches: lines for selectors, paths for missing files, command names for `schema`. |
| `error.diagnostics` | when relevant | Positions, such as `{line, col, message}` for `check` syntax errors. |
| `error.conflicts` | when relevant | `{path, kind, base, current}` for `tx commit` and `tx rebase`; the changed paths for `undo`. |

`command` is `"aitools"` when the failure happened before a command was
identified. An error envelope carries no `recovered` member, even when
recovery ran first.

## Recovered operations

Before every command, aitools checks the workspace's `commit/` directory for
intent records a crashed writer left behind. When there are any, it
takes the workspace lock, finishes or discards each operation, and reports
the result in the success envelope of the command that triggered it:

```json
{"schema_version":1,"status":"ok","command":"read", ... ,
 "recovered":[{"op_id":"op-20260926T000000Z-deadbeef","action":"discarded"}]}
```

`action` is `"rolled-forward"` when the intent record was complete (the
write is finished) or `"discarded"` when it was incomplete (the workspace
was never changed and the operation's temporary files are removed). The
check costs one directory listing when `commit/` is empty. The code is
`recover/k` in `packages/core/store/src/application/recovery.lisp`, called
from `call-with-invocation-workspace/k` in `src/workspace-context.lisp`.

## Redactions

Every string value in every envelope passes through the secret mask before
it is written (`redact-json-value` in
`packages/core/protocol/src/application/redaction-flow.lisp`). The mask is
always on. It detects only known formats:

- private-key blocks: a `-----BEGIN ...-----`/`-----END ...-----` block whose
  label ends in `PRIVATE KEY` (such as `RSA PRIVATE KEY` or `PRIVATE KEY`) or
  is OpenPGP's `PGP PRIVATE KEY BLOCK`. A block inside one string is masked
  whole, and a block split across several strings (as in `read`, `search`,
  `run`, or `diff` output, one line per string) is caught by a line-based
  pass over that one JSON array. A block whose END line is not in the array
  is masked to the end of the array; it never continues into another array
  or field, so a later file's lines and scalar fields are masked only on
  their own merits. Other PEM blocks, such as `CERTIFICATE` and `PUBLIC KEY`, are not
  secrets and pass through unmasked.
- `Bearer <token>`
- AWS access key ids: `AKIA` or `ASIA` followed by exactly 16 uppercase
  letters or digits
- tokens starting with `ghp_`, `gho_`, `github_pat_`, `sk-`, or `xoxb-`,
  `xoxp-`, `xoxa-`, `xoxr-`, `xoxs-`, `xoxo-`
- the value assigned with `=` or `:` (spaces, tabs, or quotes may stand
  between the key and the sign) to a key named `password`, `passwd`,
  `secret`, `token`, `api_key`, `apikey`, `access_key`, `access_token`,
  `auth_token`, `client_secret`, `secret_key`, or `private_key` (the key
  name is matched case-insensitively)

The lists come from `data/domain/protocol/redaction-patterns-data.lisp`.
Each match becomes `[REDACTED_SECRET]`, and the envelope gains a top-level
`redactions` count:

```json
{"schema_version":1,"status":"ok","command":"read","mode":"text","path":"cfg.env","start_line":1,"lines":["api_key=[REDACTED_SECRET]","[REDACTED_SECRET]"],"total_lines":2,"hash":"02886ad6...","truncated":false,"encoding_errors":0,"approx_tokens":11,"redactions":2}
```

Object keys are masked when their names match the same secret-key patterns.
Values such as git SHAs, UUIDs, and other long identifiers are not masked,
because the mask matches formats rather than randomness.

`read --as hex` and `archive read --as hex` mask secrets in the hex dump too.
Before rendering, the byte window is widened out to the surrounding line
boundaries so a secret straddling the window edge is still seen whole; each
line runs through the same secret mask, and the bytes of any match are
overwritten with `*` (`2a`) in the output (`redact-hex-octets` in
`packages/feature/inspect/src/domain/read-render.lisp`).

A write whose input contains the literal text `[REDACTED_SECRET]` fails with
`refusal.redacted-input` and writes nothing. There is no flag to bypass
either the mask or the refusal.
