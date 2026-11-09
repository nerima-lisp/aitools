# The md context

[Command index](index.md#command-index).

### `md headings` {#md-headings}

List parsed Markdown headings with one-based section ordinals.

Reads the whole UTF-8 file up to 16 MiB. The parser uses GFM mode and selects ATX and setext headings. It excludes a front-matter block at the start of the file, delimited by `---`, and headings inside fenced code. Binary input returns `input.unsupported-format`; parser failures return `input.syntax-error`. The hash is the SHA-256 of the original file bytes.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Markdown file path. |

| Output field | Description |
|---|---|
| `path` | Path as given. |
| `hash` | SHA-256 of the complete original file. |
| `headings` | [{section,heading,level,line}] in document order; section is a one-based ordinal and line is one-based. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.too-large`, `environment.io`, `internal.unexpected`.

### `md read` {#md-read}

Read one complete Markdown section by its one-based heading ordinal.

Returns the selected heading and all content up to, but not including, the next heading at the same or a higher level. The whole UTF-8 file is parsed up to 16 MiB.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Markdown file path. |
| `section` (positional, required) | integer |  | One-based ordinal from md headings; duplicate titles have separate ordinals. |

| Output field | Description |
|---|---|
| `path` | Path as given. |
| `hash` | SHA-256 of the complete original file. |
| `section` | Selected one-based heading ordinal. |
| `heading` | Selected heading text. |
| `level` | Selected heading level. |
| `line` | One-based source line. |
| `content` | Complete selected section including its heading. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `selection.no-match`, `refusal.too-large`, `environment.io`, `internal.unexpected`.

### `md replace` {#md-replace}

Replace one Markdown section from standard input.

Requires --stdin and --expect-hash. Writes a complete replacement section through the journal or stages it with --tx. Include the selected heading in standard input to retain it; omitting it removes that heading. It parses the whole UTF-8 file up to 16 MiB and preserves every original byte outside the selected section span.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Workspace file path. |
| `section` (positional, required) | integer |  | One-based ordinal from md headings. |
| `--expect-hash`, required | string |  | Required SHA-256 of the current target. |
| `--stdin`, required | flag |  | Read the replacement section from standard input. |
| `--dry-run` | flag |  | Validate the replacement and show a diff without writing. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when validated without applying the write. |
| `next_commands` | Command to read a complete diff when changes.diff_truncated is true. |
| `section` | Selected one-based heading ordinal. |
| `heading` | Selected heading text before replacement. |
| `level` | Selected heading level. |
| `line` | One-based source line of the selected heading. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `selection.no-match`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`.

