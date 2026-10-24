# The inspect context

[Command index](index.md#command-index).

### `read` {#read}

Read a file's lines with line numbers, a selector, and a line limit.

One of: a selector, --tail, --as hex, --as strings. --max-lines always applies; when the output stops before the end of the file, next_commands holds the next --range. A binary file read as text returns {binary,size,mime} instead of lines. With --match, line_numbers lists each returned line's number. encoding_errors counts U+FFFD substitutions in the returned lines. approx_tokens is ceil(characters of the returned text / 4). A line longer than 16384 bytes (16384 characters under a non-UTF-8 --encoding) is cut there and listed in cut_lines; the result is partial, and next_commands names the --as hex --bytes dump of the bytes after the cut. A --as strings run longer than 16384 characters is cut the same way and marked cut:true.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | File to read (relative to the working directory). |
| `--tail` | integer |  | The last N lines. |
| `--as` | enum: `text`, `hex`, `strings` | `text` | Output view; sets mode. |
| `--bytes` | string | `0:256` | With --as hex: byte span S:E or S: (0-based, E exclusive). |
| `--min-length` | integer | `4` | With --as strings: shortest run reported. |
| `--escape-invisible` | flag |  | Show control, zero-width, NBSP, ideographic-space, and line-end CR characters as \u{XXXX}. |
| `--encoding` | enum: `utf-8`, `shift_jis`, `euc-jp`, `iso-8859-1`, `utf-16le`, `utf-16be` |  | Decode from this encoding instead of UTF-8. |
| `--max-lines` | integer | `80` | Maximum lines (text), rows of 16 bytes (hex), or strings returned. |
| `--range` | string |  | Selector: lines S:E, S: (to the end), or N (1-based, inclusive). |
| `--symbol` | string |  | Selector: a definition's lines, found with the `code outline` language table. |
| `--kind` | string |  | With --symbol: only definitions of this kind (function, macro, ...). |
| `--between` | string, 2 values |  | Selector: START-RE END-RE; from a line matching START to the first later line matching END. |
| `--exclusive` | flag |  | With --between: leave out the two boundary lines. |
| `--match` | string |  | Selector: every line matching the regular expression (cl-regex-kit, per line). |
| `--invert` | flag |  | With --match: the lines that do not match. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `mode` | text, hex, or strings (the --as value). |
| `start_line` | text: number of the first returned line. |
| `lines` | text: line texts without terminators or BOM. |
| `line_numbers` | text with --match: the number of each returned line. |
| `cut_lines` | text: numbers of the returned lines cut at 16384 bytes (status partial, exit 3). |
| `total_lines` | text: lines in the file. |
| `hash` | text: SHA-256 of the file bytes (the change-detection hash). |
| `truncated` | True when --max-lines cut the output or a line or string was cut (status partial, exit 3). |
| `encoding_errors` | text: invalid UTF-8 sequences replaced in the returned lines. |
| `binary` | text on a binary file: true, with size and mime instead of lines. |
| `rows` | hex: [{offset,hex}] with 16 bytes per row. |
| `strings` | strings: [{offset,text}] printable runs; cut:true on a run cut at 16384 characters. |
| `approx_tokens` | ceil(characters of returned text / 4). |
| `next_commands` | The command reading the next part, when there is one. |

Errors: `argument.invalid`, `input.not-found`, `input.unsupported-format`, `input.unsupported-language`, `input.syntax-error`, `selection.no-match`, `selection.ambiguous`, `refusal.not-a-file`, `environment.busy`, `environment.io`.

### `info` {#info}

Describe a path: location, kind, size, lines, words, encoding, mode, hash, and digests.

Path fields always; content fields for an existing file. --allow-missing turns a missing path into exists:false with the path fields only. digest uses a standard algorithm (compare with sha256sum and friends); hash is aitools' change-detection hash.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Path to describe. |
| `--digest` | enum: `sha256`, `sha1`, `md5` |  | Add {algorithm,value} for this digest of the file bytes. |
| `--allow-missing` | flag |  | Succeed with exists:false when the path does not exist. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `absolute` | Lexical absolute path. |
| `real` | Path with every symlink resolved. |
| `relative` | Path relative to the workspace root, or null outside it. |
| `exists` | Whether the path exists (a dangling symlink does not). |
| `kind` | file, directory, symlink (dangling), other, or null. |
| `inside_workspace` | Whether the real path is below the real workspace root. |
| `ignored` | Whether the workspace ignore rules exclude the path. |
| `size` | Bytes. |
| `lines` | Line count (a final line without a newline counts); null for binary files. |
| `words` | Whitespace-separated words, as wc -w; null for binary files. |
| `max_line_chars` | Characters in the longest line; null for binary files. |
| `binary` | NUL in the first 8 KiB. |
| `mime` | MIME type from magic bytes, then the extension. |
| `utf8_valid` | Whether the bytes are valid UTF-8. |
| `encoding_guess` | utf-8, shift_jis, euc-jp, utf-16le, utf-16be, or unknown. |
| `line_ending` | lf, crlf, mixed, or none; null for binary files. |
| `trailing_newline` | Whether the file ends with a line feed. |
| `bom` | Whether the file starts with a UTF-8 BOM. |
| `mode` | Permission bits as four octal digits. |
| `mtime` | Modification time, ISO 8601 UTC; with --tx, the staged time of a file the tx touched, and null for a file the tx otherwise changed. |
| `hash` | The change-detection hash, the value --expect-hash compares: SHA-256 of the bytes of the regular file the path leads to (symlinks followed); for a symlink that leads to no regular file (dangling, or to a directory), SHA-256 of its target text, reported with --allow-missing for a dangling one; absent for a directory. |
| `approx_tokens` | ceil(characters / 4); null for binary files. |
| `digest` | {algorithm,value} with --digest. |

Errors: `argument.invalid`, `input.not-found`, `environment.busy`, `environment.io`.

### `check` {#check}

Check a JSON file's syntax or a Lisp file's delimiter balance.

Lisp balance skips strings, comments, and character literals of the file's dialect (Common Lisp, Emacs Lisp, Scheme, Clojure).

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | File to check. |
| `--format` | enum: `json`, `lisp` |  | Format; default from the extension. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `valid` | Always true on success. |
| `format` | json or lisp. |

Errors: `input.syntax-error`, `input.unsupported-format`, `input.not-found`, `refusal.not-a-file`, `environment.busy`, `environment.io`.

### `diff` {#diff}

Compare two files or directories, or show an op's full diff.

Files: unified (identical, diff), stat (added, deleted), or set (only_a, only_b, both_count: comm over distinct lines). Directories: added, removed, modified, identical_count (recursive, .git skipped). --op <op_id>: every change of that journal op with its full diff.

| Argument | Type | Default | Description |
|---|---|---|---|
| `a` (positional) | string |  | Old file or directory. |
| `b` (positional) | string |  | New file or directory. |
| `--op` | string |  | Journal op to show instead of two paths. |
| `--context` | integer | `3` | Unchanged lines around each hunk. |
| `--output` | enum: `unified`, `stat`, `set` | `unified` | File comparison shape; sets mode. |
| `--ignore-whitespace` | flag |  | Compare lines with all whitespace removed (diff -w). |
| `--ignore-eol` | flag |  | Ignore CR before LF and a missing final newline. |
| `--limit` | integer | `100` | Maximum hunks, set lines, directory entries, or op changes returned. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `mode` | unified, stat, set, directory, or op. |
| `identical` | Files: no difference under the chosen rules (cmp without ignore flags). |
| `diff` | unified: the diff text with ---/+++ headers. |
| `added` | stat: inserted lines; directory: paths only in b. |
| `deleted` | stat: deleted lines. |
| `only_a` | set: distinct lines only in a. |
| `only_b` | set: distinct lines only in b. |
| `both_count` | set: distinct lines in both. |
| `removed` | directory: paths only in a. |
| `modified` | directory: paths in both with different content. |
| `identical_count` | directory: paths in both with equal content. |
| `changes` | op: [{path,action,from?,diff?}] with the full diff of each text change. |
| `truncated` | True when --limit cut a list (status partial, exit 3). |

Errors: `argument.invalid`, `input.not-found`, `environment.io`.

### `json get` {#json-get}

Get the value at a JSON pointer, its keys, length, or raw string.

RFC 6901 pointers ("" is the whole document; ~0 is ~ and ~1 is /). length is the array's elements, the object's keys, or the string's characters. A value whose JSON is longer than --max-bytes comes back as value_preview with truncated:true (status partial, exit 3).

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | JSON file. |
| `pointer` (positional, required) | string |  | RFC 6901 pointer. |
| `--max-bytes` | size | `16KiB` | Largest rendered value returned. |
| `--keys` | flag |  | Return the object's keys or the array's indexes instead of the value. |
| `--raw` | flag |  | Return text: a string value unescaped, anything else as JSON text. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `pointer` | The pointer asked for. |
| `type` | object, array, string, number, boolean, or null. |
| `length` | Elements, keys, or characters; null for other types. |
| `value` | The value (default). |
| `keys` | With --keys. |
| `text` | With --raw. |
| `value_preview` | The start of the rendered value when --max-bytes was exceeded. |
| `truncated` | True when --max-bytes cut the value. |
| `approx_tokens` | ceil(characters of the returned value / 4). |
| `next_commands` | Narrower reads when truncated. |

Errors: `argument.invalid`, `input.not-found`, `input.unsupported-format`, `refusal.not-a-file`, `environment.busy`, `environment.io`.

### `json select` {#json-select}

Filter, sort, and project the elements of a JSON array.

--where <rel-pointer><op><json-value> (repeat for AND); op is = != < <= > >= or ~ (regex on strings). The right side is JSON when it parses as JSON, else a string. A missing member satisfies only !=. Ordering compares numbers with numbers and strings with strings.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | JSON file. |
| `pointer` (positional, required) | string |  | Pointer to the array. |
| `--where` | string, repeatable |  | Condition on each element. |
| `--pick` | string, repeatable |  | Return {pointer: value} for these relative pointers instead of the element. |
| `--sort-by` | string |  | Relative pointer to order by (null first, then booleans, numbers, strings). |
| `--desc` | flag |  | Descending order. |
| `--output` | enum: `items`, `count` | `items` | Sets mode. |
| `--limit` | integer | `50` | Maximum items returned. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `mode` | items or count. |
| `items` | [{pointer,value}] of the selected elements. |
| `count` | Selected elements, in count mode. |
| `total` | Selected elements, in items mode. |
| `truncated` | True when --limit cut items (status partial, exit 3). |

Errors: `argument.invalid`, `input.not-found`, `input.syntax-error`, `input.unsupported-format`, `refusal.not-a-file`, `environment.busy`, `environment.io`.

### `json diff` {#json-diff}

List the add, remove, and replace operations between two JSON files.

Key order and whitespace are ignored; numbers compare by value. Array elements compare by index.

| Argument | Type | Default | Description |
|---|---|---|---|
| `a` (positional, required) | string |  | Old JSON file. |
| `b` (positional, required) | string |  | New JSON file. |
| `--limit` | integer | `100` | Maximum operations returned. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `identical` | No differences. |
| `ops` | [{op,pointer,old?,new?}], op add, remove, or replace. |
| `total` | Number of operations. |
| `truncated` | True when --limit cut ops (status partial, exit 3). |

Errors: `input.not-found`, `input.unsupported-format`, `refusal.not-a-file`, `environment.io`.

### `yaml get` {#yaml-get}

Get a value from a YAML document.

Use RFC 6901 pointers. Values rendered as JSON longer than --max-bytes are returned as a preview with truncated:true (status partial, exit 3).

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | YAML file. |
| `pointer` (positional, required) | string |  | RFC 6901 pointer. |
| `--max-bytes` | size | `16KiB` | Largest rendered value returned. |
| `--keys` | flag |  | Return the object's keys or the array's indexes. |
| `--raw` | flag |  | Return a string value unescaped. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `pointer` | The pointer asked for. |
| `type` | object, array, string, number, boolean, or null. |
| `length` | Elements, keys, or characters; null for other types. |
| `value` | The value (default). |
| `keys` | With --keys. |
| `text` | With --raw. |
| `value_preview` | The start of a value longer than --max-bytes. |
| `truncated` | True when --max-bytes cut the value. |
| `approx_tokens` | ceil(characters of the returned value / 4). |
| `next_commands` | Narrower reads when truncated. |

Errors: `argument.invalid`, `input.not-found`, `input.syntax-error`, `input.unsupported-value`, `refusal.not-a-file`, `environment.busy`, `environment.io`.

### `yaml select` {#yaml-select}

Filter, sort, and project a YAML array.

--where <rel-pointer><op><json-value> repeats for AND. The right side is JSON when it parses as JSON, otherwise it is a string. A missing member satisfies only !=.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | YAML file. |
| `pointer` (positional, required) | string |  | Pointer to the array. |
| `--where` | string, repeatable |  | Condition on each element. |
| `--pick` | string, repeatable |  | Relative pointer to return instead of the element. |
| `--sort-by` | string |  | Relative pointer to order by. |
| `--desc` | flag |  | Descending order. |
| `--output` | enum: `items`, `count` | `items` | Sets mode. |
| `--limit` | integer | `50` | Maximum items returned. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `mode` | items or count. |
| `items` | [{pointer,value}] of the selected elements. |
| `count` | Selected elements, in count mode. |
| `total` | Selected elements, in items mode. |
| `truncated` | True when --limit cut items (status partial, exit 3). |

Errors: `argument.invalid`, `input.not-found`, `input.syntax-error`, `input.unsupported-value`, `refusal.not-a-file`, `environment.busy`, `environment.io`.

### `yaml diff` {#yaml-diff}

List value changes between two YAML files.

Values are compared after conversion to JSON. Key order is ignored; numbers compare by value, and array elements compare by index.

| Argument | Type | Default | Description |
|---|---|---|---|
| `a` (positional, required) | string |  | Old YAML file. |
| `b` (positional, required) | string |  | New YAML file. |
| `--limit` | integer | `100` | Maximum operations returned. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `identical` | No differences. |
| `ops` | [{op,pointer,old?,new?}], op add, remove, or replace. |
| `total` | Number of operations. |
| `truncated` | True when --limit cut ops (status partial, exit 3). |

Errors: `argument.invalid`, `input.not-found`, `input.syntax-error`, `input.unsupported-value`, `refusal.not-a-file`, `environment.io`.

### `toml get` {#toml-get}

Get a value from a TOML document.

Use RFC 6901 pointers. Values rendered as JSON longer than --max-bytes are returned as a preview with truncated:true (status partial, exit 3).

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | TOML file. |
| `pointer` (positional, required) | string |  | RFC 6901 pointer. |
| `--max-bytes` | size | `16KiB` | Largest rendered value returned. |
| `--keys` | flag |  | Return the object's keys or the array's indexes. |
| `--raw` | flag |  | Return a string value unescaped. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `pointer` | The pointer asked for. |
| `type` | object, array, string, number, boolean, or null. |
| `length` | Elements, keys, or characters; null for other types. |
| `value` | The value (default). |
| `keys` | With --keys. |
| `text` | With --raw. |
| `value_preview` | The start of a value longer than --max-bytes. |
| `truncated` | True when --max-bytes cut the value. |
| `approx_tokens` | ceil(characters of the returned value / 4). |
| `next_commands` | Narrower reads when truncated. |

Errors: `argument.invalid`, `input.not-found`, `input.syntax-error`, `input.unsupported-value`, `refusal.not-a-file`, `environment.busy`, `environment.io`.

### `toml select` {#toml-select}

Filter, sort, and project a TOML array.

--where <rel-pointer><op><json-value> repeats for AND. The right side is JSON when it parses as JSON, otherwise it is a string. A missing member satisfies only !=.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | TOML file. |
| `pointer` (positional, required) | string |  | Pointer to the array. |
| `--where` | string, repeatable |  | Condition on each element. |
| `--pick` | string, repeatable |  | Relative pointer to return instead of the element. |
| `--sort-by` | string |  | Relative pointer to order by. |
| `--desc` | flag |  | Descending order. |
| `--output` | enum: `items`, `count` | `items` | Sets mode. |
| `--limit` | integer | `50` | Maximum items returned. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `mode` | items or count. |
| `items` | [{pointer,value}] of the selected elements. |
| `count` | Selected elements, in count mode. |
| `total` | Selected elements, in items mode. |
| `truncated` | True when --limit cut items (status partial, exit 3). |

Errors: `argument.invalid`, `input.not-found`, `input.syntax-error`, `input.unsupported-value`, `refusal.not-a-file`, `environment.busy`, `environment.io`.

### `toml diff` {#toml-diff}

List value changes between two TOML files.

Values are compared after conversion to JSON. Key order is ignored; numbers compare by value, and array elements compare by index.

| Argument | Type | Default | Description |
|---|---|---|---|
| `a` (positional, required) | string |  | Old TOML file. |
| `b` (positional, required) | string |  | New TOML file. |
| `--limit` | integer | `100` | Maximum operations returned. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `identical` | No differences. |
| `ops` | [{op,pointer,old?,new?}], op add, remove, or replace. |
| `total` | Number of operations. |
| `truncated` | True when --limit cut ops (status partial, exit 3). |

Errors: `argument.invalid`, `input.not-found`, `input.syntax-error`, `input.unsupported-value`, `refusal.not-a-file`, `environment.io`.

### `table read` {#table-read}

Read rows of a CSV, TSV, JSONL, JSON, whitespace, separator, or line table.

Columns get a type (integer, number, boolean, string, null; mixed for JSON sources). ws splits each line on whitespace runs, awk-style, so rows may differ in width; --ws-columns N bounds the fields and keeps the rest of the line in the last column. A first row of unique non-numeric cells is the header unless --no-header.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Table file. |
| `--columns` | string |  | Comma-separated column names or 1-based numbers. |
| `--range` | string |  | Rows S:E, S:, or N (1-based, after --where). |
| `--limit` | integer | `50` | Maximum rows returned. |
| `--format` | enum: `csv`, `tsv`, `jsonl`, `json`, `ws`, `sep`, `lines` |  | Table format; default from the extension, then the content (ws, sep, lines are never guessed). |
| `--delimiter` | string |  | With --format sep: the fixed separator string (cut -d). |
| `--pointer` | string |  | With --format json: RFC 6901 pointer to the array of rows. |
| `--no-header` | flag |  | Treat the first row as data; columns are named 1, 2, ... |
| `--ws-columns` | integer |  | With --format ws: split into at most N columns, the last keeping the rest of the line (awk's $N). Without it every whitespace run splits. |
| `--where` | string, repeatable |  | <column><op><value>, op one of = != < <= > >= ~ (regex); repeat for AND. Columns by name or 1-based number. |
| `--encoding` | enum: `utf-8`, `shift_jis`, `euc-jp`, `iso-8859-1`, `utf-16le`, `utf-16be` |  | Decode from this encoding instead of UTF-8. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `format` | The format read. |
| `columns` | [{name,type}] of the returned columns. |
| `start_row` | Number of the first returned row. |
| `rows` | Arrays of cell values in column order. |
| `total_rows` | Rows after --where. |
| `truncated` | True when --limit stopped before the range end (status partial, exit 3). |
| `next_commands` | The command reading the next rows. |

Errors: `argument.invalid`, `input.not-found`, `input.syntax-error`, `input.unsupported-format`, `refusal.not-a-file`, `environment.busy`, `environment.io`.

### `table agg` {#table-agg}

Group table rows and count, sum, average, or take min, max, or distinct counts.

Without --group-by all rows form one group. --sum, --avg, --min, --max need a numeric column; other values fail with the offending rows in diagnostics. count is reported with --count or when no other aggregate is asked for. Groups are in key order unless --sort.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Table file. |
| `--group-by` | string, repeatable |  | Grouping column (repeatable). |
| `--count` | flag |  | Rows per group. |
| `--sum` | string |  | Column to sum. |
| `--avg` | string |  | Column to average. |
| `--min` | string |  | Column to take the minimum of. |
| `--max` | string |  | Column to take the maximum of. |
| `--distinct` | string |  | Column whose distinct values are counted. |
| `--min-count` | integer |  | Only groups with at least this many rows. |
| `--sort` | string |  | Output column to order by: count, sum, avg, min, max, distinct, or a --group-by column. |
| `--desc` | flag |  | Descending order. |
| `--limit` | integer | `50` | Maximum groups returned. |
| `--format` | enum: `csv`, `tsv`, `jsonl`, `json`, `ws`, `sep`, `lines` |  | Table format; default from the extension, then the content (ws, sep, lines are never guessed). |
| `--delimiter` | string |  | With --format sep: the fixed separator string (cut -d). |
| `--pointer` | string |  | With --format json: RFC 6901 pointer to the array of rows. |
| `--no-header` | flag |  | Treat the first row as data; columns are named 1, 2, ... |
| `--ws-columns` | integer |  | With --format ws: split into at most N columns, the last keeping the rest of the line (awk's $N). Without it every whitespace run splits. |
| `--where` | string, repeatable |  | <column><op><value>, op one of = != < <= > >= ~ (regex); repeat for AND. Columns by name or 1-based number. |
| `--encoding` | enum: `utf-8`, `shift_jis`, `euc-jp`, `iso-8859-1`, `utf-16le`, `utf-16be` |  | Decode from this encoding instead of UTF-8. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `format` | The format read. |
| `groups` | [{key:{column:value},count?,sum?,avg?,min?,max?,distinct?}]. |
| `total_groups` | Groups after --min-count. |
| `truncated` | True when --limit cut groups (status partial, exit 3). |

Errors: `argument.invalid`, `input.not-found`, `input.syntax-error`, `input.unsupported-format`, `refusal.not-a-file`, `environment.busy`, `environment.io`.

### `archive list` {#archive-list}

List the members of a zip, tar, tar.gz, or gz archive.

The format is detected from magic bytes (a gzip stream holding a tar is tar.gz). Decompression stops at 256 MiB (refusal.too-large).

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Archive file. |
| `--limit` | integer | `200` | Maximum members returned. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `format` | zip, tar, tar.gz, or gz. |
| `items` | [{path,kind,size,mode,mtime}]; kind is file, directory, symlink, hardlink, or other; mode is four octal digits or null; mtime ISO 8601 UTC. |
| `total` | Members in the archive. |
| `truncated` | True when --limit cut the list (status partial, exit 3). |

Errors: `input.not-found`, `input.unsupported-format`, `refusal.too-large`, `refusal.not-a-file`, `environment.busy`, `environment.io`.

### `archive read` {#archive-read}

Read one archive member in read's shape, with selectors and a line limit.

A gz archive has one member and takes no entry. Members larger than 64 MiB are refused. A binary member read as text returns {binary,size,mime}. A line longer than 16384 bytes is cut there and listed in cut_lines; the result is partial, and next_commands names the --as hex dump through the cut.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | Archive file. |
| `entry` (positional) | string |  | Member path as archive list shows it. |
| `--as` | enum: `text`, `hex` | `text` | Output view; sets mode. |
| `--max-lines` | integer | `80` | Maximum lines (text) or 16-byte rows (hex). |
| `--range` | string |  | Selector: lines S:E, S: (to the end), or N (1-based, inclusive). |
| `--symbol` | string |  | Selector: a definition's lines, found with the `code outline` language table. |
| `--kind` | string |  | With --symbol: only definitions of this kind (function, macro, ...). |
| `--between` | string, 2 values |  | Selector: START-RE END-RE; from a line matching START to the first later line matching END. |
| `--exclusive` | flag |  | With --between: leave out the two boundary lines. |
| `--match` | string |  | Selector: every line matching the regular expression (cl-regex-kit, per line). |
| `--invert` | flag |  | With --match: the lines that do not match. |
| `--tx` | string |  | Read the tx's state. Single-file reads also record the file's disk state in the tx read set. |

| Output field | Description |
|---|---|
| `mode` | text or hex. |
| `entry` | The member read. |
| `start_line` | text: number of the first returned line. |
| `lines` | text: line texts without terminators or BOM. |
| `cut_lines` | text: numbers of the returned lines cut at 16384 bytes (status partial, exit 3). |
| `total_lines` | text: lines in the member. |
| `hash` | text: SHA-256 of the member bytes. |
| `rows` | hex: [{offset,hex}]. |
| `truncated` | True when --max-lines cut the output or a line was cut (status partial, exit 3). |
| `approx_tokens` | ceil(characters of returned text / 4). |
| `next_commands` | The command reading the next part, when there is one. |

Errors: `argument.invalid`, `input.not-found`, `input.unsupported-format`, `input.unsupported-language`, `input.syntax-error`, `selection.no-match`, `selection.ambiguous`, `refusal.too-large`, `refusal.not-a-file`, `environment.busy`, `environment.io`.

### `snapshot create` {#snapshot-create}

Record every workspace file's size, mtime, and content hash for a later snapshot diff.

Independent of the journal and of any tx; ignored files are left out unless --no-ignore. The scan options are stored and reused by snapshot diff.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--glob` | string, repeatable |  | Only paths matching this glob (repeatable). |
| `--lang` | string |  | Only files of this language. |
| `--no-ignore` | flag |  | Include ignored files. |
| `--skip-larger-than` | size | `10MiB` | Leave out larger files. |
| `--newer` | string |  | Only files modified after this path's mtime or within this duration. |

| Output field | Description |
|---|---|
| `snapshot_id` | Id for snapshot diff. |
| `files` | Files recorded. |
| `ignore_source` | gitignore, builtin, or none. |

Errors: `argument.invalid`, `environment.io`.

### `snapshot diff` {#snapshot-diff}

List files added, removed, or modified since a snapshot.

A file counts as modified only when its size or mtime changed and its content hash differs from the recorded one.

| Argument | Type | Default | Description |
|---|---|---|---|
| `snapshot_id` (positional, required) | string |  | Id from snapshot create. |
| `--limit` | integer | `100` | Maximum paths per list. |

| Output field | Description |
|---|---|
| `added` | Paths not in the snapshot. |
| `removed` | Recorded paths now missing. |
| `modified` | Paths whose content changed. |
| `truncated` | True when --limit cut a list (status partial, exit 3). |

Errors: `input.not-found`, `environment.io`.

