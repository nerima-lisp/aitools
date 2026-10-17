# JSON output

Normal command results are JSON envelopes followed by a newline: success
goes to standard output, errors to standard error. Command-specific
`--help` is an exception: it returns the command's schema object without an
envelope. Serialization preserves member order, but consumers should access
fields by name. Repeating a command can produce different values because
of time, randomness, or changes to observed state.

The shapes of individual commands' fields are in
[Commands](commands.md). This page covers the fields that are shared.

## Success envelope

```json
{"schema_version":1,"status":"ok","command":"mkdir",
 "changes":[],"op_id":null}
```

| Field | Presence | Meaning |
|---|---|---|
| `schema_version` | always | `1`. |
| `status` | always | `"ok"` (exit code 0) or `"partial"` (exit code 3). |
| `command` | always | The command as typed: `"read"`, `"json get"`, `"tx commit"`. |
| command fields | per command | Listed in each command's schema. |
| `next_commands` | when non-empty | Complete aitools invocations that continue the result. |
| `recovered` | after a recovery | See [Recovered operations](#recovered-operations). |
| `redactions` | when anything was masked | Count of masked regions, not distinct secret values. See [Redactions](#redactions). |

`next_commands` is one of the command's own fields, so its position among
them depends on the command. `recovered` follows the command fields, and
`redactions` may be added by the command or by the final serialization pass;
its position is not fixed.

### Partial results

A read cut at a limit returns `status:"partial"`, `truncated:true`, and exit
code 3, with `next_commands` holding a command for the next page. Follow each
page's continuation until the result is no longer partial:

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
- Content-returning commands include `approx_tokens`, a rough size
  estimate, not a real tokenizer count. The estimate depends on the command
  and mode. Text `read` uses `ceil(characters / 4)`, counting one extra
  character per returned line, including the last, before secret masking;
  a multi-byte UTF-8 character counts once. Hex reads estimate three
  characters per returned byte. Masked output cannot be used to reconstruct
  the original estimate.
- `path` in `read`, `info`, and `check` output (and the compared paths in
  `diff` output) is relative to the workspace root when the file's real path is
  inside the root (`.` for the root itself), and the absolute real path when
  it is outside, since reads are not limited to the workspace. A path with no
  real path, such as a dangling symlink, is reported in its absolute form.
- `hash` is the SHA-256 hex digest of the file bytes. It is the value
  `--expect-hash` compares against, and the value in `hash_before` and
  `hash_after`.

## Write output

Journaled file writes share the following shape. See the exceptions below
for `mktemp` and `run --stdout-to`. Hashes in the examples are abbreviated;
real content hashes contain 64 hexadecimal characters.

```json
{"schema_version":1,"status":"ok","command":"edit",
 "changes":[{"path":"hello.lisp","action":"modified","hash_before":"de2187a5...","hash_after":"5f132d76...","diff":"@@ -1 +1 @@\n-old\n+new"}],
 "op_id":"op-20260926T011541Z-4a125e73","strategy":"exact"}
```

| Field | Meaning |
|---|---|
| `changes[].path` | The changed path, relative to the workspace root. |
| `changes[].action` | `created`, `modified`, `deleted`, `moved`, `mode-changed`, or `linked`. |
| `changes[].from` | For `moved`: the source path. |
| `changes[].hash_before` | The content hash before the write; `null` when the entry was absent or not a regular file. |
| `changes[].hash_after` | The content hash after the write; `null` when the entry is absent or not a regular file. |
| `changes[].diff` | Optional unified-diff hunks, without file headers, for non-binary file creation, modification, or deletion. |
| `changes[].diff_truncated` | Present as `true` only when the diff exceeds the line cap, 200 by default. |
| `op_id` | The journal operation, for `aitools undo` and `aitools diff --op`. `null` when nothing changed. |
| `tx`, `tx_op` | With `--tx`: the transaction and the staged operation's number, instead of `op_id`. |
| `dry_run` | With `--dry-run`: `true`. There is no `op_id`, and nothing was written. |

Command-specific fields follow (`strategy` for `edit --old`, `inserted_at`
for `insert`, `changes[].count` for `replace`).

A cut diff does not make the write partial. The write has completed, so the
status stays `"ok"` and the exit code 0. A journaled write supplies
`aitools diff --op <op_id>` to show the whole diff:

```json
{"status":"ok","changes":[{"action":"created","diff_truncated":true}],"op_id":"op-20260926T011609Z-c41512f8","next_commands":["aitools diff --op op-20260926T011609Z-c41512f8"]}
```

(Selected fields only; the real output has the full envelope and change
records.) A staged write instead suggests `aitools tx diff <tx>` with a
`--max-diff-lines` value large enough for the diff. A dry run has no saved
operation to inspect and does not supply a diff continuation.

A write that changes nothing, such as `mkdir` on an existing directory,
returns `"changes":[]` and `"op_id":null`.

`mktemp` is a write command but reports its result as a top-level `path`, the
absolute real path of the new entry (writes below it are allowed), plus
`hash`, rather than a `changes[]` array. That `path` is absolute, unlike the
workspace-relative `changes[].path`. `run --stdout-to` likewise reports an
absolute `{path, bytes}` object in its `stdout` field for the file it writes.

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

Before executing a workspace command, aitools checks `commit/` inside the
workspace's state directory, not inside the workspace file tree, for intent
records a crashed writer left behind. When there are any, it
takes the workspace lock, finishes or discards each operation, and reports
the result in the success envelope of the command that triggered it:

```json
{"schema_version":1,"status":"ok","command":"mkdir","changes":[],"op_id":null,
 "recovered":[{"op_id":"op-20260926T000000Z-deadbeef","action":"discarded"}]}
```

`action` is `"rolled-forward"` when the intent record was complete (the
write is finished) or `"discarded"` when it was incomplete (the workspace
was never changed and the operation's temporary files are removed).

## Redactions

String values and object keys pass through the secret mask before JSON is
written. The mask is always on. It detects only known formats:

- private-key blocks: a `-----BEGIN ...-----`/`-----END ...-----` block whose
  label ends in `PRIVATE KEY` (such as `RSA PRIVATE KEY` or `PRIVATE KEY`) or
  is OpenPGP's `PGP PRIVATE KEY BLOCK`. The mask covers recognized boundary
  lines and base64-like payload lines, not arbitrary intervening text. A
  block can occupy one string or consecutive strings in one JSON array.
  With a missing END line, payload detection continues to the end of that
  string or array, never into another array or field. A detected block
  counts once even when several output lines contain mask markers. Other
  PEM blocks, such as `CERTIFICATE` and `PUBLIC KEY`, are not secret formats.
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

Matched spans become `[REDACTED_SECRET]`. Touching or overlapping spans
merge and count once; the same secret appearing in separate regions can
count more than once. The envelope gains a top-level `redactions` count:

```json
{"schema_version":1,"status":"ok","command":"read","mode":"text","path":"cfg.env","start_line":1,"lines":["api_key=[REDACTED_SECRET]","[REDACTED_SECRET]"],"total_lines":2,"hash":"02886ad6...","truncated":false,"encoding_errors":0,"approx_tokens":11,"redactions":2}
```

Object keys are masked by the same patterns as string values, so a key
that contains a token or an assignment such as `api_key=<value>` is masked.
A key that is only a secret-sounding name, such as `password`, is not.
Different keys can become the same masked key. Ordered objects preserve
such duplicate members; objects built from hash tables can lose members
when masked keys collide. Do not treat masked JSON as a lossless copy or
use masked keys as unique identifiers.
Values such as git SHAs, UUIDs, and other long identifiers are not masked,
because the mask matches formats rather than randomness.

`read --as hex` and `archive read --as hex` mask secrets in the hex dump too.
Detection examines the surrounding line boundaries so a secret straddling
the requested byte window is still seen whole; the output remains limited
to the requested window. Each affected line is masked from the start of
its first match through the end of its last match, including intervening
bytes, using `*` (`2a`).

A write whose input contains the literal text `[REDACTED_SECRET]` fails with
`refusal.redacted-input` and writes nothing. There is no flag to bypass
either the mask or the refusal.
