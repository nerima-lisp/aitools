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

Match syntax errors appear in `error.diagnostics`. Source positions use one-based `line` and `col` fields and include a `message`. Every error includes at least one repair; see [the error envelope](../errors.md#the-error-envelope) for its fields and how to use the suggested command.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file, relative to the working directory. |
| `--match`, required | string |  | Org match syntax covers tag, TODO, level, property, and planning predicates. `+` means AND; a vertical bar separates OR alternatives; for example, `work+LEVEL=2/TODO` combines tag, level, and TODO. Relative planning dates use `--at` as their reference date; for example, `DEADLINE<"<today>"`. This is not a title substring search. |
| `--at`, required | string |  | Zone-less local datetime in `YYYY-MM-DDTHH:MM:SS` form; seconds must be `00` because the Org time context has minute precision. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | Resolve DST gaps and overlaps for `--at`. `compatible` chooses the earlier instant for overlaps and moves forward by the gap width. `earlier` chooses the earlier overlap instant and moves backward by the gap width. `later` chooses the later overlap instant and moves forward by the gap width. `strict` rejects gaps and overlaps. Relative planning predicates `SCHEDULED`, `DEADLINE`, and `CLOSED` use local wall time in `--tz`. |

| Output field | Description |
|---|---|
| `matches` | [{line,level,title,todo,tags,heading_path}] in document order. line is one-based; title omits its TODO keyword and tags; todo is a keyword or null; tags is a string array. heading_path is an array of `{title,index}` objects from root to match. Each index is the one-based ordinal among siblings with the same title and parent. |
| `count` | Number of matching headings. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.too-large`, `environment.io`, `internal.unexpected`.

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

### `org todo set` {#org-todo-set}

Change the TODO keyword on one matching heading.

The state must be defined in the document TODO workflow. The optional note follows the document TODO logging policy.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match`, required | string |  | Org match expression selecting exactly one heading. |
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
| `--match`, required | string |  | Org match expression selecting exactly one heading. |
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
| `--match`, required | string |  | Org match expression selecting exactly one heading. |
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

### `org planning set` {#org-planning-set}

Set one planning timestamp on one matching heading.

Accepts one active timestamp for scheduled or deadline and one inactive timestamp for closed. Weekday tokens, diary expressions, date ranges, multiple repeater or warning tokens, and multiline values are rejected.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file inside the selected workspace root. |
| `--match`, required | string |  | Org match expression selecting exactly one heading. |
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
| `--match`, required | string |  | Org match expression selecting exactly one heading. |
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
| `--match`, required | string |  | Org match expression selecting exactly one heading. |
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
| `--match`, required | string |  | Org match expression selecting exactly one heading. |
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

Both range endpoints are required. Future repeats default to all. The explicit time context controls relative timestamps.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file to read. |
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
| `entries` | [{date,kind,line,title,todo,tags}] in document order. |
| `count` | Number of agenda entries. |

Errors: `input.not-found`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `refusal.too-large`, `environment.io`, `internal.unexpected`, `argument.invalid`.

### `org clock report` {#org-clock-report}

Summarize clock time for one heading and its descendants.

The range is half-open: from is included and through is excluded. Running clocks are omitted.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Org file to read. |
| `--match`, required | string |  | Org match expression selecting one heading. |
| `--from`, required | string |  | Inclusive start of the half-open local date-time range. |
| `--through`, required | string |  | Exclusive end of the half-open local date-time range. |
| `--at`, required | string |  | Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. |
| `--tz`, required | string |  | Resolvable IANA time zone name. |
| `--dst-policy` | enum: `compatible`, `earlier`, `later`, `strict` | `compatible` | DST gap and overlap policy for local timestamps. |

| Output field | Description |
|---|---|
| `from` | Inclusive range start. |
| `through` | Exclusive range end. |
| `total_minutes` | Total minutes across selected rows. |
| `rows` | [{level,title,tags,minutes}] by heading. |

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

