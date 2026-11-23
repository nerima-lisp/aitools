# The org context

[Command index](index.md#command-index).

### `org outline` {#org-outline}

List Org headings and named or unnamed source and example blocks.

Syntax diagnostics appear in `error.diagnostics` when available. Source positions use one-based `line` and `col` fields. Errors include at least one repair; see [the error envelope](../errors.md#the-error-envelope) for repair fields and how to use the suggested command. Failed parses return no entries.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file, relative to the working directory. |
| `--limit` | integer |  | Maximum entries; 0 returns none. Omit to return all entries. |

| Output field | Description |
|---|---|
| `entries` | [{kind,name,level,line}] in document order. line is one-based and marks each entry's starting source line. kind is headline, src-block, or example-block. Headline name omits TODO keywords. A block name uses #+NAME, then source language for src-block, or empty for unnamed example-block. level is null for blocks. |
| `total` | All outline entries before --limit. |
| `truncated` | True only when total exceeds --limit. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.too-large`, `environment.io`, `internal.unexpected`.

### `org query` {#org-query}

Find Org headings that match an Org match expression.

Match syntax errors appear in `error.diagnostics`. Source positions use one-based `line` and `col` fields and include a `message`. Every error includes at least one repair; see [the error envelope](../errors.md#the-error-envelope) for its fields and how to use the suggested command. The command searches directories recursively for `*.org`, including ignored files, and skips symlinks found during traversal. All explicit paths must resolve inside the workspace. Every result includes the resolved file `path` relative to the workspace root, including for a single file. The command reads each resolved file path once and skips `*.gpg` files. The command validates options before reading files. Errors that first arise when evaluating a selected document report the file's path. An empty selection returns zero results. A missing path or a file read or parse error fails the request. File read and parse errors identify the file.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string, repeatable |  | One or more files or directories, relative to the working directory. |
| `--match`, required | string |  | Org match syntax covers tag, TODO, level, property, and planning predicates. `+` means AND; a vertical bar separates OR alternatives; for example, `work+LEVEL=2/TODO` combines tag, level, and TODO. Relative planning dates use `--at` as their reference date; for example, `DEADLINE<"<today>"`. This is not a title substring search. |
| `--at`, required | string |  | Zone-less local datetime in `YYYY-MM-DDTHH:MM:SS` form; seconds must be `00` because the Org time context has minute precision. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | Resolve DST gaps and overlaps for `--at`. `compatible` chooses the earlier instant for overlaps and moves forward by the gap width. `earlier` chooses the earlier overlap instant and moves backward by the gap width. `later` chooses the later overlap instant and moves forward by the gap width. `strict` rejects gaps and overlaps. Relative planning predicates `SCHEDULED`, `DEADLINE`, and `CLOSED` use local wall time in `--tz`. |

| Output field | Description |
|---|---|
| `matches` | [{line,level,title,todo,tags,heading_path,path}] in document order within each file. line is one-based; title omits its TODO keyword and tags; todo is a keyword or null; tags is a string array. heading_path is an array of `{title,index}` objects from root to match. Each index is the one-based ordinal among siblings with the same title and parent. Matches are grouped by path in lexicographic order. |
| `count` | Number of matching headings. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.too-large`, `refusal.outside-workspace`, `refusal.not-a-file`, `environment.io`, `internal.unexpected`.

### `org export` {#org-export}

Export an Org file as HTML, GFM, or ASCII text.

Unsupported syntax fails the whole command without content. HTTP, HTTPS, and mailto links are supported; other link types return `input.unsupported-format`. Source-block `:exports` modes are `code` (the default, source only), `results` (the next `#+RESULTS` block, after the source with only whitespace between), `both` (source and that block), and `none` (neither). Unknown, repeated, or value-less `:exports` declarations fail with `input.unsupported-format`. Errors include at least one repair; see [the error envelope](../errors.md#the-error-envelope) for its fields and how to use the suggested command. Exporter syntax errors include a source location when provided.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file, relative to the working directory. |
| `--backend`, required | enum: `html`, `gfm`, `ascii` |  | Export backend. |

| Output field | Description |
|---|---|
| `backend` | The selected backend. |
| `content` | Complete export text. Present only on success. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.too-large`, `environment.io`, `internal.unexpected`.

### `org capture` {#org-capture}

Add a heading under a selected heading or date.

For a headline target, select exactly one heading. For a datetree target, --at supplies the date. Body text must not contain Org headings.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | For headline targets, an Org match expression selecting exactly one heading; give exactly one of --match and --heading-path. Omit for datetree targets. |
| `--heading-path` | string |  | For headline targets, the heading_path JSON array returned by org query; give exactly one of --match and --heading-path. Omit for datetree targets. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--target`, required | enum: `headline`, `datetree` |  | Target kind. |
| `--title`, required | string |  | New heading title. |
| `--body` | string |  | Optional heading body. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `changed` | True when the file changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org archive` {#org-archive}

Move one heading subtree to an archive file.

Creates the destination file and heading when absent. Archive metadata records the source path relative to the destination.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Source Org file. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--to`, required | string |  | Destination Org file. |
| `--target-heading`, required | string |  | Destination heading title. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `changed` | True when the subtree was moved. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org todo set` {#org-todo-set}

Change the TODO keyword on one matching heading.

The state must be defined in the document TODO workflow. The optional note follows the document TODO logging policy.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--state`, required | string |  | TODO keyword defined by the document. |
| `--note` | string |  | Optional TODO logging note. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `previous_state` | Previous TODO keyword, or null. |
| `new_state` | New TODO keyword. |
| `changed` | True when the selected heading changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org property set` {#org-property-set}

Set one property on one matching heading.

Adds an unregistered property when needed. The property value must be a single line.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--name`, required | string |  | Property name. |
| `--value`, required | string |  | Single-line property value. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `property` | Property name. |
| `changed` | True when the property changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org property delete` {#org-property-delete}

Delete one property from one matching heading.

A missing property is a no-op. Duplicate same-name properties in the drawer are rejected as ambiguous.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--name`, required | string |  | Property name. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `property` | Property name. |
| `changed` | True when the property was removed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org priority set` {#org-priority-set}

Set the priority of one heading.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--priority`, required | string |  | Priority value, for example A. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `changed` | True when the document changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org priority delete` {#org-priority-delete}

Remove the priority of one heading.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `changed` | True when the document changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org tags add` {#org-tags-add}

Add local tags to one heading.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--tags`, required | string |  | Comma-separated tag names. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `changed` | True when the document changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org tags remove` {#org-tags-remove}

Remove local tags from one heading.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--tags`, required | string |  | Comma-separated tag names. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `changed` | True when the document changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org tags replace` {#org-tags-replace}

Replace local tags on one heading.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--tags`, required | string |  | Comma-separated tag names; an empty value clears them. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `changed` | True when the document changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org heading rename` {#org-heading-rename}

Change the text of one heading.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--title`, required | string |  | New heading text. If it contains a statistics cookie, it replaces the existing cookies; otherwise existing cookies are appended. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `changed` | True when the document changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org checkbox toggle` {#org-checkbox-toggle}

Toggle one checkbox and update statistics cookies.

Changes [X] to [ ], and [ ] or [-] to [X]. For an item without child checkboxes, repeating the command toggles it again; use --dry-run to inspect the change. An item with child checkboxes is recalculated from its children and may not flip. Parent item checkboxes are updated from their direct children, then every statistics cookie owned by the selected heading is recalculated.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--item`, required | integer |  | One-based checkbox number in document order, including nested items but excluding child headings. The numbering of checkboxes in inline tasks is undefined. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `item` | One-based checkbox number. |
| `changed` | True when the document changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org planning set` {#org-planning-set}

Set one planning timestamp on one matching heading.

Accepts one active timestamp for scheduled or deadline and one inactive timestamp for closed. Weekday tokens, diary expressions, date ranges, multiple repeater or warning tokens, and multiline values are rejected. The heading stores the timestamp with the weekday Org writes after the date, so `<2026-10-20>` becomes `<2026-10-20 Tue>`.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--kind`, required | enum: `scheduled`, `deadline`, `closed` |  | Planning kind to set. |
| `--timestamp`, required | string |  | One timestamp in the supported Org planning subset. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `kind` | Planning kind. |
| `timestamp` | Timestamp written to the selected heading. |
| `changed` | True when the planning timestamp changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org planning delete` {#org-planning-delete}

Delete one planning timestamp from one matching heading.

A missing timestamp of the requested kind is a no-op.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--kind`, required | enum: `scheduled`, `deadline`, `closed` |  | Planning kind to delete. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `kind` | Planning kind. |
| `changed` | True when the timestamp was removed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org clock in` {#org-clock-in}

Start a clock on one matching heading.

Requires an explicit local time context. A heading with an existing running clock is rejected.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `timestamp` | Timestamp opened for the clock. |
| `changed` | True when a clock was started. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org clock out` {#org-clock-out}

Stop the running clock on one matching heading.

The selected heading must contain exactly one running clock.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `interval` | Closed clock interval and elapsed duration. |
| `changed` | True when the running clock was closed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`.

### `org agenda` {#org-agenda}

List Org agenda entries in an inclusive date range.

Both range endpoints are required. Future repeats default to all. The explicit time context controls relative timestamps. The command searches directories recursively for `*.org`, including ignored files, and skips symlinks found during traversal. All explicit paths must resolve inside the workspace. Every result includes the resolved file `path` relative to the workspace root, including for a single file. The command reads each resolved file path once and skips `*.gpg` files. The command validates options before reading files. Errors that first arise when evaluating a selected document report the file's path. An empty selection returns zero results. A missing path or a file read or parse error fails the request. File read and parse errors identify the file.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string, repeatable |  | One or more files or directories, relative to the working directory. |
| `--from`, required | string |  | Inclusive start date in YYYY-MM-DD form. |
| `--through`, required | string |  | Inclusive end date in YYYY-MM-DD form. |
| `--future-repeats` | enum: `all`, `next`, `none` | `all` | Include all future occurrences, the next one, or none. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |

| Output field | Description |
|---|---|
| `from` | Inclusive start date. |
| `through` | Inclusive end date. |
| `entries` | Entries are [{date,kind,line,title,todo,tags,path}] in date order within each file. Entries are grouped by path in lexicographic order. |
| `count` | Number of agenda entries. |

Errors: `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.too-large`, `environment.io`, `internal.unexpected`, `argument.invalid`, `refusal.outside-workspace`, `refusal.not-a-file`.

### `org clock report` {#org-clock-report}

Summarize clock time for one heading and its descendants.

The range is half-open: from is included and through is excluded. Running clocks are omitted. A CLOCK line that cannot be parsed is left out of the totals and reported in warnings.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file to read. |
| `--from`, required | string |  | Inclusive start of the half-open local date-time range. |
| `--through`, required | string |  | Exclusive end of the half-open local date-time range. |
| `--match` | string |  | Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path. |
| `--heading-path` | string |  | The heading_path of one `org query` match, passed as the JSON array it reports, for example [{"title":"Project","index":1},{"title":"Tasks","index":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |

| Output field | Description |
|---|---|
| `from` | Inclusive range start. |
| `through` | Exclusive range end. |
| `total_minutes` | Total minutes across selected rows. |
| `rows` | [{level,title,tags,minutes}] by heading. |
| `warnings` | CLOCK lines left out of the totals because they could not be parsed; each has code, message, path (workspace-relative), line, and source. Always present, empty when none. |

Errors: `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.too-large`, `environment.io`, `internal.unexpected`, `selection.no-match`, `selection.ambiguous`, `argument.invalid`.

### `org table align` {#org-table-align}

Align one numbered Org table.

Selects the table by its one-based document order. No table is selected implicitly.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--table`, required | integer |  | One-based table number in the document. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `table` | One-based table number. |
| `changed` | True when table spacing changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `argument.invalid`.

### `org table sort` {#org-table-sort}

Sort the standard rows of one numbered Org table.

Rows are one-based and exclude horizontal rules. Type defaults to lexical and order to ascending. Start and end must be supplied together and must remain inside one horizontal-rule-delimited section.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--table`, required | integer |  | One-based table number in the document. |
| `--column`, required | integer |  | One-based column to compare. |
| `--type` | enum: `lexical`, `numeric` | `lexical` | Cell comparison type. |
| `--order` | enum: `ascending`, `descending` | `ascending` | Sort direction. |
| `--start` | integer |  | First standard row in the inclusive sort range. |
| `--end` | integer |  | Last standard row in the inclusive sort range. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `table` | One-based table number. |
| `changed` | True when row order changed. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`, `argument.invalid`.

### `org table recalculate` {#org-table-recalculate}

Recalculate formulas for one numbered Org table.

Uses the first formula directive following the selected table. No directive is a successful no-op. Formula failures reject the whole command.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--table`, required | integer |  | One-based table number in the document. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `table` | One-based table number. |
| `changed` | True when a formula changed cell values. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`.

### `org dynamic-block update` {#org-dynamic-block-update}

Update one numbered Org dynamic block.

Select a block by its one-based document order. The clocktable :scope must be nil (the default), file, agenda, subtree, tree, or treeN with N of 1 or more; file-with-archives, agenda-with-archives, a file name, and a list of files are rejected as argument.invalid. Agenda globs are relative to the workspace root; only .org files are aggregated, and .git entries are skipped. The agenda files are read only when the block asks for the agenda scope, and the staged content applies under --tx. An agenda scope with no matching files is invalid. Clocktable :link headings are relative file links computed from the workspace-relative paths of the block document and each aggregated document. A CLOCK line that cannot be parsed is left out of the totals and reported in warnings.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--block`, required | integer |  | One-based dynamic block number in document order. |
| `--agenda-glob` | string, repeatable |  | Workspace-relative glob for agenda files. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |
| `--dry-run` | flag |  | Validate and show the diff without changing disk or a transaction overlay. |
| `--tx` | string |  | Stage the write in this transaction. |

| Output field | Description |
|---|---|
| `block` | One-based dynamic block number. |
| `changed` | True when the block changed. |
| `warnings` | CLOCK lines left out of the totals because they could not be parsed; each has code, message, path (workspace-relative), line, and source. Always present, empty when none. |
| `changes` | Standard write changes with path, action, hashes, and a diff when available. |
| `op_id` | Journal operation id for undo; null when nothing changed. |
| `tx` | Transaction receiving the staged write. |
| `tx_op` | Staged operation number in the transaction. |
| `dry_run` | True when the write was validated without being applied. |
| `next_commands` | Command to read a full diff when changes.diff_truncated is true. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.not-a-file`, `refusal.outside-workspace`, `refusal.target-changed`, `refusal.redacted-input`, `refusal.too-large`, `environment.busy`, `environment.io`, `internal.unexpected`.

