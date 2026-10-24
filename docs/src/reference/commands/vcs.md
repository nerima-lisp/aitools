# The vcs context

[Command index](index.md#command-index).

### `git status` {#git-status}

Show the branch, upstream distance, and staged, unstaged, and untracked paths.

| Output field | Description |
|---|---|
| `branch` | Current branch, or "(detached)". |
| `upstream` | Tracked upstream branch, or null. |
| `ahead` | Commits ahead of upstream, or null without one. |
| `behind` | Commits behind upstream, or null without one. |
| `staged` | [{path,status,from?}] index changes; status is git's one-letter code. |
| `unstaged` | [{path,status}] work-tree changes; unmerged paths have status U. |
| `untracked` | Untracked paths. |

Errors: `environment.unavailable`, `environment.io`.

### `git log` {#git-log}

List the newest commits, optionally only those touching a path.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional) | string |  | Limit to commits touching this path. |
| `--limit` | integer | `20` | Maximum number of commits. |

| Output field | Description |
|---|---|
| `items` | [{sha,author,date,subject}], newest first; date is ISO 8601 in the author's offset. |
| `total` | Number of matching commits. |
| `truncated` | True when total exceeds --limit (status partial, exit 3). |

Errors: `environment.unavailable`, `input.not-found`, `environment.io`.

### `git diff` {#git-diff}

Show changed files with line counts and, in hunks mode, their hunks.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional) | string |  | Limit to this path. |
| `--staged` | flag |  | Compare the index with HEAD instead of the work tree with the index. |
| `--ref` | string |  | Revision or range to compare, such as HEAD~1 or A..B. |
| `--output` | enum: `hunks`, `stat` | `hunks` | hunks adds each file's hunks; stat gives counts only. |
| `--max-lines` | integer | `400` | Hunk lines returned before later files become mode summary. |

| Output field | Description |
|---|---|
| `mode` | hunks or stat. |
| `files` | [{path,from?,mode,added,deleted,binary?,hunks?}]; file mode is hunks, stat, or summary (over --max-lines). |
| `truncated` | True when a file was summarized (status partial, exit 3). |
| `approx_tokens` | ceil(characters of returned hunk lines / 4). |

Errors: `argument.invalid`, `environment.unavailable`, `input.not-found`, `environment.io`.

### `git blame` {#git-blame}

Show who last changed each line of a work-tree file.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | File to blame. |
| `--range` | string |  | Selector: lines S:E, S:, or N. Without a selector, the first 80 lines. |
| `--symbol` | string |  | Selector: the definition NAME (see code outline); --kind narrows it. |
| `--kind` | string |  | With --symbol: narrow to one definition kind. |
| `--between` | string, 2 values |  | Selector: from a line matching RE1 to the next matching RE2; --exclusive drops both. |
| `--exclusive` | flag |  | With --between: leave out the two boundary lines. |
| `--match` | string |  | Selector: every line matching RE; --invert selects the rest. |
| `--invert` | flag |  | With --match: the lines that do not match. |

| Output field | Description |
|---|---|
| `start_line` | First returned line number. |
| `lines` | [{n,sha,author,date,text}]. |
| `total_lines` | Lines in the file. |
| `truncated` | True when the default 80-line window ended before the file did. |

Errors: `argument.invalid`, `selection.no-match`, `selection.ambiguous`, `input.syntax-error`, `input.unsupported-language`, `environment.unavailable`, `input.not-found`, `environment.io`.

### `git show` {#git-show}

Read a file as stored at a revision, in read's shape.

| Argument | Type | Default | Description |
|---|---|---|---|
| `object` (positional, required) | string |  | <rev>:<path>, such as HEAD:src/a.lisp. |
| `--range` | string |  | Selector: lines S:E, S:, or N. |
| `--symbol` | string |  | Selector: the definition NAME (see code outline); --kind narrows it. |
| `--kind` | string |  | With --symbol: narrow to one definition kind. |
| `--between` | string, 2 values |  | Selector: from a line matching RE1 to the next matching RE2; --exclusive drops both. |
| `--exclusive` | flag |  | With --between: leave out the two boundary lines. |
| `--match` | string |  | Selector: every line matching RE; --invert selects the rest; lines then come with line_numbers. |
| `--invert` | flag |  | With --match: the lines that do not match. |
| `--max-lines` | integer | `80` | Maximum lines returned. |

| Output field | Description |
|---|---|
| `rev` | Revision part of the object name. |
| `path` | Path part of the object name. |
| `start_line` | First returned line number. |
| `lines` | Line texts without terminators or BOM. |
| `line_numbers` | With --match, the number of each returned line. |
| `encoding_errors` | Malformed UTF-8 sequences replaced by U+FFFD. |
| `total_lines` | Lines in the blob. |
| `hash` | SHA-256 of the blob bytes. |
| `truncated` | True when --max-lines stopped before the requested end. |
| `approx_tokens` | ceil(characters of returned lines / 4). |
| `binary` | Present and true for a blob with a NUL in its first 8 KiB; lines are then omitted and size given. |

Errors: `argument.invalid`, `selection.no-match`, `selection.ambiguous`, `input.syntax-error`, `input.unsupported-language`, `environment.unavailable`, `input.not-found`, `environment.io`.

