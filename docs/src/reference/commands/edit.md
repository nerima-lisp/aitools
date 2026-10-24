# The edit context

[Command index](index.md#command-index).

### `edit` {#edit}

Replace text selected by --old or a selector; --new '' deletes it.

--old matches exactly first, then line-wise ignoring leading and trailing whitespace (re-indenting --new to the file). Line selectors replace whole lines including their newline. --stdin reads {"old","new"} or {"edits":[{old|between|match..., new}]} applied in order, all or nothing; edits[] accept only content-based selectors.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--old` | string |  | Text to replace; exact, then whitespace-insensitive. |
| `--new` | string |  | Replacement; '' deletes the selection. |
| `--range` | string |  | Lines S:E, S: or N (1-based, inclusive). |
| `--symbol` | string |  | The lines of a definition. |
| `--kind` | string |  | Definition kind for --symbol. |
| `--between` | two strings |  | START-RE END-RE: a start line through the next end line. |
| `--exclusive` | flag |  | With --between: exclude both ends. |
| `--match` | string |  | Every line matching RE. |
| `--invert` | flag |  | With --match: the lines that do not match. |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--expect-count` | string |  | Guard: the number of selected lines or replacements. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `expect_count` | With --dry-run of a --match or replace selection: the lines or replacements it selected, the value a real run passes as --expect-count. |
| `strategy` | exact or whitespace, for --old. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `selection.count-mismatch`, `selection.no-match`, `selection.ambiguous`, `input.syntax-error`, `input.unsupported-language`, `input.not-utf8`.

### `insert` {#insert}

Insert content at the start or end of a file, or before/after the lines a selector picks.

--at end ends a last line lacking a newline before appending. --before/--after with --match inserts at every matching line and needs --expect-count.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--at` | string |  | start or end. |
| `--before` | flag |  | Insert before the selection. |
| `--after` | flag |  | Insert after the selection. |
| `--range` | string |  | Lines S:E, S: or N (1-based, inclusive). |
| `--symbol` | string |  | The lines of a definition. |
| `--kind` | string |  | Definition kind for --symbol. |
| `--between` | two strings |  | START-RE END-RE: a start line through the next end line. |
| `--exclusive` | flag |  | With --between: exclude both ends. |
| `--match` | string |  | Every line matching RE. |
| `--invert` | flag |  | With --match: the lines that do not match. |
| `--content` | string |  | Content text. |
| `--content-file` | string |  | Content read as bytes from this file. |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--expect-count` | string |  | Guard: the number of selected lines or replacements. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `expect_count` | With --dry-run of a --match or replace selection: the lines or replacements it selected, the value a real run passes as --expect-count. |
| `inserted_at` | Line numbers (after the write) where each inserted block starts. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `selection.count-mismatch`, `selection.no-match`, `selection.ambiguous`, `input.syntax-error`, `input.unsupported-language`, `input.not-utf8`.

### `replace` {#replace}

Regex (or --fixed) replace across files or within one file's selection, with $1/${name} replacement templates.

Replaces every non-overlapping match per line (whole file with --multiline). Templates: $0 $1 $name ${name} $$ and ${1:filter:...} with upper lower capitalize snake camel kebab trim inc dec padN. \1-style references are refused unless --literal-replacement, which inserts the replacement verbatim. All files are validated before anything is written. --stdin reads {"pattern","replacement"}; positionals are then all paths.

| Argument | Type | Default | Description |
|---|---|---|---|
| `pattern replacement [path...]` (positional) | string |  | Repeatable. |
| `--fixed` | flag |  | PATTERN is a literal string. |
| `--ignore-case` | flag |  | Case-insensitive match. |
| `--word` | flag |  | Match whole words only. |
| `--multiline` | flag |  | Match across lines; . still excludes newlines unless (?s). |
| `--nth` | string |  | Replace only the Nth match of each file. |
| `--literal-replacement` | flag |  | Insert REPLACEMENT verbatim, without template expansion. |
| `--range` | string |  | Lines S:E, S: or N (1-based, inclusive). |
| `--symbol` | string |  | The lines of a definition. |
| `--kind` | string |  | Definition kind for --symbol. |
| `--between` | two strings |  | START-RE END-RE: a start line through the next end line. |
| `--exclusive` | flag |  | With --between: exclude both ends. |
| `--match` | string |  | Every line matching RE. |
| `--invert` | flag |  | With --match: the lines that do not match. |
| `--glob` | string, repeatable |  | Only paths matching this glob; repeatable. |
| `--lang` | string |  | Only files of this language. |
| `--no-ignore` | flag |  | Include ignored files. |
| `--skip-larger-than` | string | `10MiB` | Skip larger files. |
| `--newer` | string |  | Only files newer than PATH or a duration ago. |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--expect-count` | string |  | Guard: the number of selected lines or replacements. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `expect_count` | With --dry-run of a --match or replace selection: the lines or replacements it selected, the value a real run passes as --expect-count. |
| `changes[].count` | Replacements made in that file. |
| `skipped` | [{path,reason}] files left out by scan options. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `selection.count-mismatch`, `selection.no-match`, `selection.ambiguous`, `input.syntax-error`, `input.unsupported-language`, `input.not-utf8`.

### `apply` {#apply}

Apply a unified diff from --stdin to the workspace, all files or none.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--fuzz` | string | `3` | Lines around a hunk's recorded position searched for its context. |
| `--reverse` | flag |  | Apply the inverse patch. |
| `--strip` | string |  | Leading path components dropped; default drops a/ and b/. |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `applied_hunks` | Hunks applied across all files. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `selection.no-match`, `selection.ambiguous`.

### `transform` {#transform}

Apply line operations (sort, unique, wrap, comment, ...) to a file or a selection.

--op repeats and applies in order. Line ops: sort sort-numeric sort-version reverse shuffle unique delete-blank squeeze-blank strip-trailing indent dedent tabs-to-spaces spaces-to-tabs upper lower nfc nfkc wrap reflow comment uncomment. Whole-file ops (no selector): eol-lf eol-crlf final-newline no-final-newline strip-bom. wrap and reflow leave Markdown code fences alone. --width defaults to 2 for indent/dedent (dedent without it removes the common indent) and 8 for tab conversion.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--op` | string, repeatable |  | Operation; repeat to chain. |
| `--width` | string |  | Indent width or tab stop. |
| `--key` | string |  | Sort/unique key field (1-based). |
| `--delimiter` | string |  | Key field separator regex (default whitespace). |
| `--columns` | string | `80` | wrap and reflow width. |
| `--seed` | string |  | Required by shuffle. |
| `--range` | string |  | Lines S:E, S: or N (1-based, inclusive). |
| `--symbol` | string |  | The lines of a definition. |
| `--kind` | string |  | Definition kind for --symbol. |
| `--between` | two strings |  | START-RE END-RE: a start line through the next end line. |
| `--exclusive` | flag |  | With --between: exclude both ends. |
| `--match` | string |  | Every line matching RE. |
| `--invert` | flag |  | With --match: the lines that do not match. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--expect-count` | string |  | Guard: the number of selected lines or replacements. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `expect_count` | With --dry-run of a --match or replace selection: the lines or replacements it selected, the value a real run passes as --expect-count. |
| `removed_lines` | Lines removed, for ops that remove lines. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `selection.count-mismatch`, `selection.no-match`, `selection.ambiguous`, `input.syntax-error`, `input.unsupported-language`, `input.not-utf8`.

### `move-lines` {#move-lines}

Move selected lines to another place in the same file or into another file.

| Argument | Type | Default | Description |
|---|---|---|---|
| `src` (positional, required) | string |  |  |
| `--to` | string |  | Destination file (default: the same file). |
| `--to-position` | string | `end` | start, end, after:N, before:N, after-symbol:NAME or before-symbol:NAME (line numbers of the destination before the move). |
| `--range` | string |  | Lines S:E, S: or N (1-based, inclusive). |
| `--symbol` | string |  | The lines of a definition. |
| `--kind` | string |  | Definition kind for --symbol. |
| `--between` | two strings |  | START-RE END-RE: a start line through the next end line. |
| `--exclusive` | flag |  | With --between: exclude both ends. |
| `--match` | string |  | Every line matching RE. |
| `--invert` | flag |  | With --match: the lines that do not match. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--expect-count` | string |  | Guard: the number of selected lines or replacements. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `expect_count` | With --dry-run of a --match or replace selection: the lines or replacements it selected, the value a real run passes as --expect-count. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `selection.count-mismatch`, `selection.no-match`, `selection.ambiguous`, `input.syntax-error`, `input.unsupported-language`, `input.not-utf8`.

### `write` {#write}

Create a file from --content, --content-file (repeatable, concatenated) or --stdin.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--separator` | string |  | Inserted between concatenated inputs. |
| `--overwrite` | flag |  | Replace an existing file (needs --expect-hash). |
| `--content` | string, repeatable |  | Content text; repeat to concatenate. |
| `--content-file` | string, repeatable |  | Content bytes from this file; repeat to concatenate. |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`.

### `split` {#split}

Split a file into numbered pieces by line count, before matching lines, or by bytes.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--lines` | string |  | Lines per piece. |
| `--at-match` | string |  | Start a piece at each line matching this regex. |
| `--bytes` | string |  | Bytes per piece. |
| `--prefix` | string |  | Piece path prefix (default <path>.). |
| `--suffix-digits` | string | `3` | Digits of the piece number. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `changes[].start_line` | First source line of the piece. |
| `changes[].lines` | Source lines the piece holds. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`.

### `transcode` {#transcode}

Convert a file between utf-8, shift_jis (CP932), euc-jp, iso-8859-1, utf-16le and utf-16be.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--from` | string |  | Source encoding (default: guessed). |
| `--to` | string | `utf-8` | Target encoding. |
| `--replace-unmappable` | flag |  | Write ? for characters the target cannot represent. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `from` | Source encoding. |
| `to` | Target encoding. |
| `replaced` | Characters written as ?. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.unsupported-format`, `input.syntax-error`.

### `move` {#move}

Move or rename a file or directory; never merges into an existing directory.

| Argument | Type | Default | Description |
|---|---|---|---|
| `src` (positional, required) | string |  |  |
| `dst` (positional, required) | string |  |  |
| `--overwrite` | flag |  | Replace an existing destination file (needs --expect-hash dst=hash). |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`.

### `copy` {#copy}

Copy a file, or a directory with --recursive, byte for byte.

--recursive copies everything, ignore rules included; symlinks leading outside the workspace are skipped and listed.

| Argument | Type | Default | Description |
|---|---|---|---|
| `src` (positional, required) | string |  |  |
| `dst` (positional, required) | string |  |  |
| `--overwrite` | flag |  | Replace an existing destination file (needs --expect-hash dst=hash). |
| `--recursive` | flag |  | Copy a directory tree. |
| `--max-bytes` | string | `1GiB` | Largest total size copied. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `files` | Files copied (directories). |
| `skipped` | [{path,reason}] entries not copied. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `refusal.too-large`.

### `rm` {#rm}

Remove one file, symlink, or empty directory; journaled and undoable.

A tree needs --recursive and --expect-count (including its root); preview with --dry-run first. Never follows symlinks. Refuses workspace/home/filesystem roots and version-control or state metadata. Limits bound deleted entries and file bytes, not memory use.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--recursive` | flag |  | Remove a directory tree; requires --expect-count except in dry-run. |
| `--max-entries` | string | `1000` | Maximum deleted entries, including directories and symlinks. |
| `--max-bytes` | string | `64MiB` | Maximum total deleted regular-file bytes; not a memory limit. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--expect-count` | string |  | Guard: the number of selected lines or replacements. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `expect_count` | With --dry-run of a --match or replace selection: the lines or replacements it selected, the value a real run passes as --expect-count. |
| `entries` | Number of deleted entries, including the target itself. |
| `bytes` | Total regular-file bytes selected for deletion. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `selection.count-mismatch`.

### `delete` {#delete}

Delete a file, a symlink, or an empty directory.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`.

### `mkdir` {#mkdir}

Create a directory and its parents; an existing directory is a no-op.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`.

### `chmod` {#chmod}

Set or clear the executable bits, or set an octal mode.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--exec` | flag |  | Add execute permission. |
| `--no-exec` | flag |  | Remove execute permission. |
| `--mode` | string |  | Octal mode, such as 644. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `previous_mode` | The octal mode before the change. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`.

### `link` {#link}

Create a symlink LINK pointing at TARGET inside the workspace.

| Argument | Type | Default | Description |
|---|---|---|---|
| `target` (positional, required) | string |  |  |
| `link` (positional, required) | string |  |  |
| `--overwrite` | flag |  | Replace an existing symlink (not a file). |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`.

### `touch` {#touch}

Create an empty file, or set an existing file's modification time.

Journaled like every write: undo deletes a file touch created, or sets an existing file's previous modification time back (refusal.target-changed when its content, mode or mtime changed since). With --tx the time is staged and applied at tx commit.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--mtime` | string |  | Unix seconds, @seconds, or ISO 8601 (default now). |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `mtime` | The modification time set (ISO 8601 UTC). |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`.

### `mktemp` {#mktemp}

Create a temporary file or directory in the workspace's state tmp/ area (not journaled).

| Argument | Type | Default | Description |
|---|---|---|---|
| `--dir` | flag |  | Create a directory. |
| `--suffix` | string |  | Name suffix, such as .json. |

| Output field | Description |
|---|---|
| `path` | Absolute real path of the new entry; writes below it are allowed. |
| `hash` | For a file (not --dir): the content hash info reports for it, ready for write --overwrite --expect-hash. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`.

### `json set` {#json-set}

Set the value at a JSON Pointer (/- appends to an array).

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `pointer` (positional, required) | string |  |  |
| `value` (positional) | string |  |  |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`.

### `json delete` {#json-delete}

Delete the value at a JSON Pointer.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `pointer` (positional, required) | string |  |  |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`.

### `json merge` {#json-merge}

Apply an RFC 7386 JSON Merge Patch read from --stdin.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`.

### `json patch` {#json-patch}

Apply an RFC 6902 JSON Patch read from --stdin; all operations or none.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `selection.no-match`, `selection.ambiguous`, `input.unsupported-format`, `input.syntax-error`.

### `json fmt` {#json-fmt}

Re-indent a JSON file, keeping key order unless --sort-keys.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--indent` | string |  | Indent width (default: the file's own, else 2). |
| `--minify` | flag |  | Write on one line. |
| `--sort-keys` | flag |  | Sort object keys. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`.

### `table set` {#table-set}

Set one cell of a CSV or TSV file by data row and column name.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--row` | string |  | Data row, 1-based after the header. |
| `--column` | string |  | Header name (or 1-based index). |
| `--value` | string |  | New cell text. |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `previous` | The cell's previous text. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`.

### `archive extract` {#archive-extract}

Extract a zip, tar, tar.gz or gz archive after validating every entry.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--to` | string |  | Destination directory (required). |
| `--entry` | string, repeatable |  | Entry to extract; repeat (default: all). |
| `--max-bytes` | string | `1GiB` | Largest total extracted size. |
| `--max-entries` | string | `100000` | Most entries extracted. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `total` | Changes in the operation; changes lists the first 200. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `input.unsupported-format`, `input.syntax-error`, `refusal.too-large`.

### `archive create` {#archive-create}

Create a zip, tar, tar.gz or gz archive from workspace files (ignored files excluded).

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `src` (positional) | string |  | Repeatable. |
| `--format` | string |  | zip, tar, tar.gz or gz (default: from the extension). |
| `--glob` | string, repeatable |  | Only paths matching this glob; repeatable. |
| `--lang` | string |  | Only files of this language. |
| `--no-ignore` | flag |  | Include ignored files. |
| `--skip-larger-than` | string | `10MiB` | Skip larger files. |
| `--newer` | string |  | Only files newer than PATH or a duration ago. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `entries` | Entries written. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`.

### `yaml set` {#yaml-set}

Set a JSON value in a YAML document at a JSON Pointer; string values need JSON quotes.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `pointer` (positional, required) | string |  |  |
| `value` (positional) | string |  |  |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `warnings` | [{code,message,count}] for dropped comments; always present, empty when none. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `input.unsupported-value`.

### `yaml delete` {#yaml-delete}

Delete a value from a YAML document at a JSON Pointer.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `pointer` (positional, required) | string |  |  |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `warnings` | [{code,message,count}] for dropped comments; always present, empty when none. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `input.unsupported-value`.

### `yaml merge` {#yaml-merge}

Apply an RFC 7386 JSON Merge Patch from --stdin to a YAML document.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `warnings` | [{code,message,count}] for dropped comments; always present, empty when none. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `input.unsupported-value`.

### `yaml patch` {#yaml-patch}

Apply an RFC 6902 JSON Patch from --stdin to a YAML document; all operations or none.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `warnings` | [{code,message,count}] for dropped comments; always present, empty when none. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `selection.no-match`, `selection.ambiguous`, `input.unsupported-format`, `input.syntax-error`, `input.unsupported-value`.

### `yaml fmt` {#yaml-fmt}

Format a YAML document; a second fmt is a byte-for-byte no-op.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `warnings` | [{code,message,count}] for dropped comments; always present, empty when none. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `input.unsupported-value`.

### `toml set` {#toml-set}

Set a JSON value in a TOML document at a JSON Pointer; string values need JSON quotes.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `pointer` (positional, required) | string |  |  |
| `value` (positional) | string |  |  |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `warnings` | [{code,message,count}] for dropped comments; always present, empty when none. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `input.unsupported-value`.

### `toml delete` {#toml-delete}

Delete a value from a TOML document at a JSON Pointer.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `pointer` (positional, required) | string |  |  |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `warnings` | [{code,message,count}] for dropped comments; always present, empty when none. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `input.unsupported-value`.

### `toml merge` {#toml-merge}

Apply an RFC 7386 JSON Merge Patch from --stdin to a TOML document.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `warnings` | [{code,message,count}] for dropped comments; always present, empty when none. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `input.unsupported-value`.

### `toml patch` {#toml-patch}

Apply an RFC 6902 JSON Patch from --stdin to a TOML document; all operations or none.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--stdin` | flag |  | Read the input from standard input (never read otherwise). |
| `--stdin-data` | string |  | The --stdin input given inline; history records --stdin writes this way. |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `warnings` | [{code,message,count}] for dropped comments; always present, empty when none. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `selection.no-match`, `selection.ambiguous`, `input.unsupported-format`, `input.syntax-error`, `input.unsupported-value`.

### `toml fmt` {#toml-fmt}

Format a TOML document. Value entries precede child tables and arrays of tables; order within each group is preserved, groups may reorder, and a second fmt is a byte-for-byte no-op.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  |  |
| `--expect-hash` | string, repeatable |  | Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text. |
| `--dry-run` | flag |  | Validate and show the diff; write nothing. |
| `--tx` | string |  | Stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `changes` | [{path,action,from?,hash_before,hash_after,diff?,diff_truncated?}]; action is created, modified, deleted, moved, mode-changed or linked; diff is cut at 200 lines. |
| `op_id` | The journal op (undo with `aitools undo <op_id>`); null when nothing changed. |
| `tx` | With --tx: the tx the write was staged in, instead of op_id. |
| `tx_op` | With --tx: the staged op's number. |
| `dry_run` | With --dry-run: true; nothing was written. |
| `warnings` | [{code,message,count}] for dropped comments; always present, empty when none. |

Errors: `argument.invalid`, `input.not-found`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `refusal.not-a-file`, `environment.busy`, `environment.io`, `internal.unexpected`, `refusal.target-changed`, `input.not-utf8`, `input.unsupported-format`, `input.syntax-error`, `input.unsupported-value`.

