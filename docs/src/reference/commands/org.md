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

