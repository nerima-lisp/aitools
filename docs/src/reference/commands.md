# Commands

This page lists every command the binary registers, with its arguments,
output fields, and declared error codes. The same information is available
at run time:

```sh
aitools schema                 # every command's name and summary
aitools schema json get read   # full detail for the named commands
aitools schema --all           # full detail for every command
aitools read --help            # full detail for one command
```

The per-command sections below are generated from the command schemas each
context registers, so they match `aitools schema --all` for the source they
were generated from. To regenerate them after a change, see
[Regenerating the reference](../project/development.md#regenerating-the-reference).
Options and commands that are not provided are listed under
[Not provided](#not-provided).

## Global options

These options belong to the application, not to one command. They are
accepted before or after the command name.

| Option | Description |
|---|---|
| `--root <dir>` | Workspace root. Without it, aitools uses the enclosing git root, then the working directory. |
| `--lock-timeout <duration>` | How long a write, or recovery before a command, waits for the workspace lock. Default `10s`. |
| `--version` | Prints `{"schema_version":1,"status":"ok","command":"version","name":"aitools","version":...}`. |
| `--help` | After a command name, prints that command's schema detail object. Without a command, prints the command list. |

`--help` output is the bare schema object (`name`, `summary`, `description`,
`args`, `output_fields`, `error_codes`) for a command, or
`{"schema_version":1,"status":"ok","command":"schema","commands":[...]}`
without one. It is not wrapped in the success envelope that `aitools schema`
uses.

A relative path argument resolves against the current working directory,
as in a shell. `--root` selects the workspace (the write boundary, the state
directory, the ignore rules); it is not the base for relative paths. Paths in
write output are relative to the workspace root. `read`, `info`, `check`,
and `diff` report a file inside the root relative to it and a file outside
the root by its absolute real path; see [JSON output](json-schema.md#reads).

## Conventions shared by many commands

- **Selectors** pick the part of a file a command acts on: `--old`,
  `--range`, `--symbol` (with `--kind`), `--between` (with `--exclusive`),
  and `--match` (with `--invert`). A call may use only one; two selectors
  fail with `argument.invalid`. `--range` is `S:E`, `S:` (to the end of the
  file), or `N` (line N alone), 1-based and inclusive, written in ASCII
  digits with no sign or whitespace: `3:5` is a range, while ` 3`, `+3`, and
  `3: 5` fail with `argument.invalid`.
- **Guards** make a write fail without writing when its assumptions no longer
  hold: `--expect-hash <hash>` (or `<path>=<hash>`) and `--expect-count <n>`.
  A position-based selector (`--range`, `--symbol`) on a write, or an
  overwrite of an existing destination, requires `--expect-hash`. A write
  whose selection can match more than once (`--match`, `replace`) requires
  `--expect-count`, except under `--dry-run`, which writes nothing and
  reports the count it selected as `expect_count`. A missing required guard
  fails with `argument.invalid` and a repair that shows how to obtain the
  value. A bare
  `--expect-hash <hash>` guards only the first target of a write. When a
  write touches a second file, such as `move-lines --to` with a
  position-based selector, that file needs its own `<path>=<hash>`.
- **Input** comes from `--content`, `--content-file`, or `--stdin`. aitools
  reads standard input only when `--stdin` is given. `--stdin-data <text>`
  passes the `--stdin` payload inline; the journal records `--stdin` writes
  this way. `--stdin` and `--stdin-data` come as a pair: every command that
  accepts one accepts the other, and only the content-input writes have them
  (`edit`, `insert`, `replace`, `apply`, `write`, `json set`, `json merge`,
  `json patch`, `table set`), not every write.
- **Writes** accept `--dry-run` (validate and show the diff, write nothing)
  and `--tx <tx>` (stage the write in a transaction), except where the
  argument table omits them. Every write returns the shape described in
  [JSON output](json-schema.md#write-output).
- **Limits** (`--limit`, `--max-lines`, `--max-bytes`) cut a read result to
  `status:"partial"` with exit code 3 and a `next_commands` entry that
  continues the read.
- **Durations** are `<n>ms`, `<n>s`, `<n>m`, `<n>h`, or `<n>d`. **Sizes** are
  `<n>`, `<n>KiB`, `<n>MiB`, or `<n>GiB`. `<n>` is written in ASCII digits,
  optionally with a decimal fraction; a sign, whitespace, or a digit from
  another script fails with `argument.invalid`.

The meaning and exit code of each error code are in
[Errors and exit codes](errors.md).

## Command index

<!-- BEGIN GENERATED: commands -->
| Command | Context | Summary |
|---|---|---|
| [`search`](#search) | search | Search file contents for a regular expression; results are grouped into blocks with context. |
| [`find`](#find) | search | List files and directories by name or path pattern; ls, find, tree, and du in one. |
| [`overview`](#overview) | search | Summarize the workspace: root, git state, languages, build files, and top-level entries. |
| [`code outline`](#code-outline) | search | List the definitions in one source file with their line ranges. |
| [`code defs`](#code-defs) | search | Find where a name is defined across the workspace's source files. |
| [`code refs`](#code-refs) | search | Find the lines that mention a name as a whole identifier. |
| [`read`](#read) | inspect | Read a file's lines with line numbers, a selector, and a line limit. |
| [`info`](#info) | inspect | Describe a path: location, kind, size, lines, words, encoding, mode, hash, and digests. |
| [`check`](#check) | inspect | Check a JSON file's syntax or a Lisp file's delimiter balance. |
| [`diff`](#diff) | inspect | Compare two files or directories, or show an op's full diff. |
| [`json get`](#json-get) | inspect | Get the value at a JSON pointer, its keys, length, or raw string. |
| [`json select`](#json-select) | inspect | Filter, sort, and project the elements of a JSON array. |
| [`json diff`](#json-diff) | inspect | List the add, remove, and replace operations between two JSON files. |
| [`table read`](#table-read) | inspect | Read rows of a CSV, TSV, JSONL, JSON, whitespace, separator, or line table. |
| [`table agg`](#table-agg) | inspect | Group table rows and count, sum, average, or take min, max, or distinct counts. |
| [`archive list`](#archive-list) | inspect | List the members of a zip, tar, tar.gz, or gz archive. |
| [`archive read`](#archive-read) | inspect | Read one archive member in read's shape, with selectors and a line limit. |
| [`snapshot create`](#snapshot-create) | inspect | Record every workspace file's size, mtime, and content hash for a later snapshot diff. |
| [`snapshot diff`](#snapshot-diff) | inspect | List files added, removed, or modified since a snapshot. |
| [`edit`](#edit) | edit | Replace text selected by --old or a selector; --new '' deletes it. |
| [`insert`](#insert) | edit | Insert content at the start or end of a file, or before/after the lines a selector picks. |
| [`replace`](#replace) | edit | Regex (or --fixed) replace across files or within one file's selection, with $1/${name} replacement templates. |
| [`apply`](#apply) | edit | Apply a unified diff from --stdin to the workspace, all files or none. |
| [`transform`](#transform) | edit | Apply line operations (sort, unique, wrap, comment, ...) to a file or a selection. |
| [`move-lines`](#move-lines) | edit | Move selected lines to another place in the same file or into another file. |
| [`write`](#write) | edit | Create a file from --content, --content-file (repeatable, concatenated) or --stdin. |
| [`split`](#split) | edit | Split a file into numbered pieces by line count, before matching lines, or by bytes. |
| [`transcode`](#transcode) | edit | Convert a file between utf-8, shift_jis (CP932), euc-jp, iso-8859-1, utf-16le and utf-16be. |
| [`move`](#move) | edit | Move or rename a file or directory; never merges into an existing directory. |
| [`copy`](#copy) | edit | Copy a file, or a directory with --recursive, byte for byte. |
| [`delete`](#delete) | edit | Delete a file, a symlink, or an empty directory. |
| [`mkdir`](#mkdir) | edit | Create a directory and its parents; an existing directory is a no-op. |
| [`chmod`](#chmod) | edit | Set or clear the executable bits, or set an octal mode. |
| [`link`](#link) | edit | Create a symlink LINK pointing at TARGET inside the workspace. |
| [`touch`](#touch) | edit | Create an empty file, or set an existing file's modification time. |
| [`mktemp`](#mktemp) | edit | Create a temporary file or directory in the workspace's state tmp/ area (not journaled). |
| [`json set`](#json-set) | edit | Set the value at a JSON Pointer (/- appends to an array). |
| [`json delete`](#json-delete) | edit | Delete the value at a JSON Pointer. |
| [`json merge`](#json-merge) | edit | Apply an RFC 7386 JSON Merge Patch read from --stdin. |
| [`json patch`](#json-patch) | edit | Apply an RFC 6902 JSON Patch read from --stdin; all operations or none. |
| [`json fmt`](#json-fmt) | edit | Re-indent a JSON file, keeping key order unless --sort-keys. |
| [`table set`](#table-set) | edit | Set one cell of a CSV or TSV file by data row and column name. |
| [`archive extract`](#archive-extract) | edit | Extract a zip, tar, tar.gz or gz archive after validating every entry. |
| [`archive create`](#archive-create) | edit | Create a zip, tar, tar.gz or gz archive from workspace files (ignored files excluded). |
| [`history`](#history) | journal | List journaled operations (op_id, command, paths, time), newest first. |
| [`undo`](#undo) | journal | Revert one journaled op, checking first that none of its paths changed since. |
| [`tx begin`](#tx-begin) | journal | Open a transaction: --tx writes stack up in it without touching the working tree. |
| [`tx status`](#tx-status) | journal | List open transactions, or show one tx's operations, paths, drift and stale reads. |
| [`tx diff`](#tx-diff) | journal | Show what committing a tx would change, as write-output changes without an op_id. |
| [`tx drop`](#tx-drop) | journal | Undo one tx operation and every later one, restoring the tx state before it. |
| [`tx rebase`](#tx-rebase) | journal | Re-apply a tx's operations onto files that changed outside it, moving its base forward. |
| [`tx commit`](#tx-commit) | journal | Write a whole tx to the working tree as one journaled op, or report its conflicts. |
| [`tx abort`](#tx-abort) | journal | Discard a tx without touching the working tree. |
| [`run`](#run) | process | Run a program without a shell and report its exit, timing, and trimmed output. |
| [`wait`](#wait) | process | Block until one condition holds: a file line, an open port, a bg log line, a bg exit, or a duration. |
| [`bg start`](#bg-start) | process | Start a program detached from aitools, logging stdout and stderr to the workspace state directory. |
| [`bg logs`](#bg-logs) | process | Read a bg process's log: the last lines, or the lines from a byte offset. |
| [`bg status`](#bg-status) | process | List the bg processes aitools started in this workspace, or one of them. |
| [`bg stop`](#bg-stop) | process | Stop a bg process: SIGTERM to its process group, SIGKILL if it outlives --grace. |
| [`git status`](#git-status) | vcs | Show the branch, upstream distance, and staged, unstaged, and untracked paths. |
| [`git log`](#git-log) | vcs | List the newest commits, optionally only those touching a path. |
| [`git diff`](#git-diff) | vcs | Show changed files with line counts and, in hunks mode, their hunks. |
| [`git blame`](#git-blame) | vcs | Show who last changed each line of a work-tree file. |
| [`git show`](#git-show) | vcs | Read a file as stored at a revision, in read's shape. |
| [`sys info`](#sys-info) | env | Describe the host: OS, architecture, CPUs, user, memory, and the workspace's disk. |
| [`sys env`](#sys-env) | env | List environment variables, optionally those whose name starts with PREFIX. |
| [`sys tools`](#sys-tools) | env | Find commands on PATH and report the first line of their version output. |
| [`sys procs`](#sys-procs) | env | List processes whose command line contains PATTERN (case-insensitive), ordered by pid. |
| [`sys ports`](#sys-ports) | env | List TCP sockets in LISTEN state with the owning process. |
| [`time now`](#time-now) | env | Show the current time in a zone, in UTC, and as epoch milliseconds. |
| [`time convert`](#time-convert) | env | Convert a time value to ISO 8601 or epoch, optionally shifted by durations. |
| [`time diff`](#time-diff) | env | Compute B minus A. |
| [`util encode`](#util-encode) | util | Encode the input bytes as base64, URL percent-encoding, or hex. |
| [`util decode`](#util-decode) | util | Decode base64, URL percent-encoding, or hex; binary results are returned as hex, or written to a file with --to. |
| [`util redact`](#util-redact) | util | Mask known secret formats in the input text. |
| [`util tokens`](#util-tokens) | util | Measure the input: approximate tokens, characters, bytes, lines, words, longest line. |
| [`util calc`](#util-calc) | util | Evaluate integer, decimal, and rational arithmetic with arbitrary precision. |
| [`util uuid`](#util-uuid) | util | Generate RFC 9562 UUIDs from the OS cryptographic random source. |
| [`util random`](#util-random) | util | Generate uniformly random strings over hex, alnum, or base64url. |
| [`schema`](#schema) | composition root | List implemented commands, or show one command's full schema. |
| [`batch`](#batch) | composition root | Run several aitools invocations from one JSON input, optionally as one tx. |

## The search context

### `search` {#search}

Search file contents for a regular expression; results are grouped into blocks with context.

Patterns use cl-regex-kit syntax and match the bytes of each file, with `.`, `\w`, and `[^...]` matching one UTF-8 character. Most syntax runs in linear time on the Pike VM; backreferences, lookaround, atomic groups, possessive repetition, conditionals, subroutine calls, `\K`, `\X`, and the `\G`/`\Z`/`\b{...}` anchors run on the bounded advanced executor, and a file on which it exhausts its step budget is listed in `skipped` with reason `regex-limit`. `--ignore-case` and `\w` follow cl-regex-kit's Unicode defaults (simple case folding; Unicode word characters). `^` and `$` match at line boundaries (before CR LF too). Without `--multiline` no match crosses a line end; with it, matches may span lines and `.` still excludes a newline unless the pattern sets `(?s)`. A UTF-8 BOM is never part of the first line.

| Argument | Type | Default | Description |
|---|---|---|---|
| `pattern` (positional) | string |  | The pattern. When --pattern or --stdin supplies the pattern, this positional is the first path instead. |
| `path` (positional) | string, repeatable |  | Files or directories to search; default the working directory (or the root when it is outside the workspace). |
| `--pattern` | string, repeatable |  | A pattern; repeat for lines matching any of them (`matches` items then carry pattern_index, 0-based). |
| `--stdin` | flag |  | Read the pattern as raw UTF-8 text from standard input; one trailing newline is dropped. |
| `--fixed` | flag |  | Treat the pattern as literal text. |
| `--ignore-case` | flag |  | Case-insensitive matching. |
| `--word` | flag |  | Match only at word boundaries (the pattern is wrapped in \b(?:...)\b). |
| `--line-regexp` | flag |  | Match only whole lines. |
| `--invert` | flag |  | Select the lines that do not match. Not with --output matches. |
| `--multiline` | flag |  | Let matches span lines; each file is matched as one buffer. |
| `--context` | integer | `2` | Context lines before and after each selected line. |
| `--before` | integer |  | Context lines before; overrides --context. |
| `--after` | integer |  | Context lines after; overrides --context. |
| `--output` | enum: `blocks`, `matches`, `count`, `files`, `files-without-match` | `blocks` | The result shape; `mode` repeats it. |
| `--limit` | integer | `15` | Selected lines (blocks), matches (matches), or entries (count, files, files-without-match) returned. |
| `--glob` | string, repeatable |  | Only paths matching this glob (wildmatch; `!` excludes; no `/` matches the base name). |
| `--lang` | string |  | Only files of this language (the `code` language table). |
| `--no-ignore` | flag |  | Do not apply .gitignore or the builtin excludes. `.git` and `.aitools-*.tmp` are always skipped. |
| `--skip-larger-than` | size |  | Skip files above this size (<n>, <n>KiB, <n>MiB, <n>GiB). |
| `--newer` | string |  | Only entries modified after this path's mtime, or within this duration (<n>ms\|s\|m\|h\|d) of now. |
| `--tx` | string |  | Read through this tx: staged files, deletions, and the tx's .gitignore apply. |

| Output field | Description |
|---|---|
| `mode` | The --output value. |
| `blocks` | blocks: [{path,start_line,lines[],match_lines[]}]; blocks whose context touches or overlaps are merged. |
| `matches` | matches: [{path,line,col,text,groups,named?,pattern_index?}]; col counts characters from 1; groups lists captures in order, null for a group that did not participate; named maps capture names to text or null. |
| `counts` | count: [{path,count}] selected lines per file, files with none omitted. |
| `paths` | files / files-without-match: paths of text files with at least one / no selected line. |
| `total_matches` | Exact number of selected lines (matches mode: matches) in every scanned file, also past --limit. |
| `total` | count, files, files-without-match: number of entries before --limit. |
| `files_scanned` | Text files searched. |
| `skipped` | [{path,reason}]; reason is binary, too-large, unreadable, or regex-limit. |
| `ignore_source` | gitignore, builtin, or none (--no-ignore). |
| `truncated` | True when --limit cut the result (status partial, exit 3; next_commands repeats the search with a sufficient --limit). |
| `approx_tokens` | ceil(characters of returned text / 4). |

Errors: `argument.invalid`, `input.syntax-error`, `input.not-found`, `environment.io`, `internal.unexpected`.

### `find` {#find}

List files and directories by name or path pattern; ls, find, tree, and du in one.

| Argument | Type | Default | Description |
|---|---|---|---|
| `pattern` (positional) | string |  | A glob (when it holds *, ?, or [) or a substring. Without `/` it matches the last path component; with `/`, the workspace-relative path. Omit to list everything. |
| `path` (positional) | string |  | Where to start; default the working directory (or the root when it is outside the workspace). |
| `--type` | enum: `file`, `dir`, `symlink` |  | Only entries of this kind. |
| `--depth` | integer |  | Only entries at most this many levels below the start (1: its direct entries, like ls -la). |
| `--sort` | enum: `path`, `mtime`, `size` | `path` | path order; mtime newest first; size largest first. Flat output only. |
| `--min-size` | size |  | Only entries at least this large (directories only with --sizes). |
| `--max-size` | size |  | Only entries at most this large (directories only with --sizes). |
| `--empty` | flag |  | Only empty files and directories with no listed entries. |
| `--executable` | flag |  | Only files with an execute bit. |
| `--output` | enum: `flat`, `tree` | `flat` | flat items, or a nested tree. |
| `--sizes` | flag |  | Give directories the total size of the files listed below them (du). |
| `--limit` | integer | `50` | Entries returned. |
| `--glob` | string, repeatable |  | Only paths matching this glob (wildmatch; `!` excludes; no `/` matches the base name). |
| `--lang` | string |  | Only files of this language (the `code` language table). |
| `--no-ignore` | flag |  | Do not apply .gitignore or the builtin excludes. `.git` and `.aitools-*.tmp` are always skipped. |
| `--skip-larger-than` | size |  | Skip files above this size (<n>, <n>KiB, <n>MiB, <n>GiB). |
| `--newer` | string |  | Only entries modified after this path's mtime, or within this duration (<n>ms\|s\|m\|h\|d) of now. |
| `--tx` | string |  | Read through this tx: staged files, deletions, and the tx's .gitignore apply. |

| Output field | Description |
|---|---|
| `mode` | flat or tree. |
| `items` | flat: [{path,kind,size,mode,mtime}]; kind is file, dir, symlink, or other; size is null for a directory without --sizes; mode is octal text; mtime is UTC ISO 8601. |
| `tree` | tree: {name,kind,size?,children[],omitted?}; omitted counts matching entries past --limit below that node. |
| `total` | Matching entries before --limit. |
| `ignore_source` | gitignore, builtin, or none (--no-ignore). |
| `truncated` | True when --limit cut the result (status partial, exit 3). |

Errors: `argument.invalid`, `input.not-found`, `internal.unexpected`.

### `overview` {#overview}

Summarize the workspace: root, git state, languages, build files, and top-level entries.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional) | string |  | Summarize below this directory; default the root. |
| `--limit` | integer | `30` | Languages returned. |
| `--no-ignore` | flag |  | Include ignored files. |
| `--tx` | string |  | Read through this tx. |

| Output field | Description |
|---|---|
| `root` | The workspace root. |
| `path` | The summarized directory, workspace-relative ("" for the root). |
| `ignore_source` | gitignore, builtin, or none. |
| `git` | {branch,head,untracked,deleted} read from .git without running git, or null outside a repository. branch is null on a detached HEAD. untracked and deleted count files below path; files modified in place are not counted. |
| `languages` | [{lang,files,lines,bytes}], most lines first. |
| `languages_total` | Languages before --limit. |
| `build_files` | Workspace-relative paths of build and project files (flake.nix, *.asd, Cargo.toml, package.json, ...). |
| `entries` | [{name,kind}] directly below path. |
| `truncated` | True when --limit cut the languages (status partial, exit 3). |

Errors: `input.not-found`, `argument.invalid`, `internal.unexpected`.

### `code outline` {#code-outline}

List the definitions in one source file with their line ranges.

Definitions come from the language table shared with --symbol: Common Lisp, Emacs Lisp, Scheme, Clojure, Rust, Go, Python, JavaScript, TypeScript, Nix, shell, and Markdown headings. end_line is estimated by balanced parentheses, balanced braces, indentation, or the next heading of the same or a higher level.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional, required) | string |  | The file. |
| `--limit` | integer | `200` | Symbols returned. |
| `--tx` | string |  | Read the file as staged in this tx. |

| Output field | Description |
|---|---|
| `path` | The file, workspace-relative when inside the workspace. |
| `lang` | The language. |
| `symbols` | [{line,end_line,kind,name}] in line order. |
| `total` | Definitions before --limit. |
| `truncated` | True when --limit cut the list (status partial, exit 3). |

Errors: `input.unsupported-language`, `input.unsupported-format`, `input.not-found`, `internal.unexpected`.

### `code defs` {#code-defs}

Find where a name is defined across the workspace's source files.

| Argument | Type | Default | Description |
|---|---|---|---|
| `name` (positional, required) | string |  | The definition name. |
| `path` (positional) | string |  | Where to look; default the working directory. |
| `--prefix` | flag |  | Match names starting with NAME. |
| `--kind` | string |  | Only this kind (function, macro, class, ...; see code outline). |
| `--limit` | integer | `50` | Definitions returned. |
| `--tx` | string |  | Read through this tx. |

| Output field | Description |
|---|---|
| `defs` | [{path,line,end_line,kind,name}] in path and line order. |
| `total` | Definitions before --limit. |
| `truncated` | True when --limit cut the list (status partial, exit 3). |

Errors: `argument.invalid`, `input.not-found`, `internal.unexpected`.

### `code refs` {#code-refs}

Find the lines that mention a name as a whole identifier.

| Argument | Type | Default | Description |
|---|---|---|---|
| `name` (positional, required) | string |  | The name. |
| `path` (positional) | string |  | Where to look; default the working directory. |
| `--limit` | integer | `50` | Lines returned. |
| `--tx` | string |  | Read through this tx. |

| Output field | Description |
|---|---|
| `refs` | [{path,line,kind,text}]; kind is def on a line defining the name, else ref. An occurrence counts only when no identifier character of the file's language touches it. |
| `total` | Lines before --limit. |
| `truncated` | True when --limit cut the list (status partial, exit 3). |
| `approx_tokens` | ceil(characters of returned text / 4). |

Errors: `argument.invalid`, `input.not-found`, `internal.unexpected`.


## The inspect context

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

Errors: `argument.invalid`, `input.not-found`, `environment.io`.

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


## The edit context

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


## The journal context

### `history` {#history}

List journaled operations (op_id, command, paths, time), newest first.

Every committed write, tx commit and undo is one op. With a path, only ops that touched it or anything under it (a move's source counts). More ops than --limit make the result partial (exit 3) with the command that lists them all in next_commands. Reads take no lock.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` (positional) | string |  | Only ops touching this path (absolute or relative to the working directory). |
| `--limit` | integer | `50` | Ops returned. |

| Output field | Description |
|---|---|
| `items` | [{op_id, command, paths, time, undoes?}], newest first; undoes names the op an undo reverted. |
| `total` | Ops matching, before --limit. |
| `truncated` | True when --limit cut the list. |

Errors: `argument.invalid`, `environment.io`.

### `undo` {#undo}

Revert one journaled op, checking first that none of its paths changed since.

op_id is required (see aitools history). If any path of the op is no longer in the state the op left it in, nothing is written and the changed paths come back as conflicts (exit 2). The undo is itself a new op with undoes = op_id, so undoing it redoes the original. Takes the workspace lock (--lock-timeout).

| Argument | Type | Default | Description |
|---|---|---|---|
| `op_id` (positional, required) | string |  | The op to revert, from aitools history. |
| `--dry-run` | flag |  | Validate and compute the changes without writing or journaling. |
| `--max-diff-lines` | integer | `200` | Diff lines shown per change; longer diffs are cut and marked diff_truncated. |

| Output field | Description |
|---|---|
| `changes` | [{path, action, from?, hash_before, hash_after, diff?, diff_truncated?}], as every write reports. |
| `op_id` | The new op recording the undo (absent with --dry-run). |
| `dry_run` | True with --dry-run. |
| `undoes` | The reverted op_id. |

Errors: `argument.invalid`, `input.not-found`, `refusal.target-changed`, `environment.busy`, `environment.io`.

### `tx begin` {#tx-begin}

Open a transaction: --tx writes stack up in it without touching the working tree.

Writes given --tx <tx> are validated against the tx state and recorded in it; reads given --tx see it. Nothing reaches the files until tx commit. Open transactions never expire.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--name` | string |  | Label shown by tx status. |

| Output field | Description |
|---|---|
| `tx` | The tx id for --tx and the other tx commands. |
| `name` | The --name label, or null. |

Errors: `environment.busy`, `environment.io`.

### `tx status` {#tx-status}

List open transactions, or show one tx's operations, paths, drift and stale reads.

drift lists written paths whose file changed outside the tx since the tx first touched it (commit would conflict; see tx rebase); stale_reads lists files read through the tx that changed since (commit would conflict unless re-read or --ignore-stale-reads). A path's base and staged are content hashes, null for a missing file, directory or symlink.

| Argument | Type | Default | Description |
|---|---|---|---|
| `tx` (positional) | string |  | The tx to detail; omit to list all. |

| Output field | Description |
|---|---|
| `items` | Without tx: [{tx, name, created, ops, paths, drift, stale_reads}], ops and paths as counts. |
| `total` | Without tx: number of open transactions. |
| `ops` | With tx: [{tx_op, command, paths}]. |
| `paths` | With tx: [{path, action, base, staged}]. |
| `drift` | Written paths changed outside the tx. |
| `stale_reads` | Paths read through the tx and changed since. |

Errors: `input.not-found`, `environment.io`.

### `tx diff` {#tx-diff}

Show what committing a tx would change, as write-output changes without an op_id.

Compares each path's base (its state when the tx first touched it) with its staged state. A cut diff is marked diff_truncated and next_commands repeats this command with a limit that fits.

| Argument | Type | Default | Description |
|---|---|---|---|
| `tx` (positional, required) | string |  | The tx. |
| `--max-diff-lines` | integer | `200` | Diff lines shown per change. |

| Output field | Description |
|---|---|
| `tx` | The tx. |
| `changes` | [{path, action, from?, hash_before, hash_after, diff?, diff_truncated?}]. |

Errors: `input.not-found`, `environment.io`.

### `tx drop` {#tx-drop}

Undo one tx operation and every later one, restoring the tx state before it.

Later operations build on earlier ones, so dropping one drops everything after it too.

| Argument | Type | Default | Description |
|---|---|---|---|
| `tx` (positional, required) | string |  | The tx. |
| `tx_op` (positional, required) | integer |  | The first operation to drop, from tx status. |

| Output field | Description |
|---|---|
| `tx` | The tx. |
| `dropped` | The dropped tx_op numbers, ascending. |

Errors: `argument.invalid`, `input.not-found`, `environment.busy`, `environment.io`.

### `tx rebase` {#tx-rebase}

Re-apply a tx's operations onto files that changed outside it, moving its base forward.

Only operations touching drifted paths (or paths an earlier re-applied operation rewrote) are re-run, from their recorded arguments. Content-based operations (--old, --between, --match, replace, apply, json writes) search again; position-based ones (--range, --symbol, --expect-hash) are not re-run and make the rebase a conflict. On any conflict the tx is left unchanged (exit 2).

| Argument | Type | Default | Description |
|---|---|---|---|
| `tx` (positional, required) | string |  | The tx. |

| Output field | Description |
|---|---|
| `tx` | The tx. |
| `rebased` | The drifted paths whose base moved to the current file. |

Errors: `input.not-found`, `refusal.target-changed`, `environment.busy`, `environment.io`.

### `tx commit` {#tx-commit}

Write a whole tx to the working tree as one journaled op, or report its conflicts.

Atomically write every staged path. A written path whose file changed since the tx first touched it is a write conflict (repair: tx rebase); a file read through the tx that changed since is a read conflict (repair: read it again with --tx, or --ignore-stale-reads). Any conflict writes nothing (exit 2). The op_id undoes the whole tx.

| Argument | Type | Default | Description |
|---|---|---|---|
| `tx` (positional, required) | string |  | The tx. |
| `--ignore-stale-reads` | flag |  | Commit despite read conflicts. |
| `--max-diff-lines` | integer | `200` | Diff lines shown per change. |

| Output field | Description |
|---|---|
| `changes` | [{path, action, from?, hash_before, hash_after, diff?, diff_truncated?}], as every write reports. |
| `op_id` | The op recording the commit; null for an empty tx. |

Errors: `input.not-found`, `refusal.target-changed`, `environment.busy`, `environment.io`.

### `tx abort` {#tx-abort}

Discard a tx without touching the working tree.

| Argument | Type | Default | Description |
|---|---|---|---|
| `tx` (positional, required) | string |  | The tx. |

| Output field | Description |
|---|---|
| `tx` | The tx. |
| `discarded` | The paths the tx had staged, sorted. |

Errors: `input.not-found`, `environment.busy`, `environment.io`.


## The process context

### `run` {#run}

Run a program without a shell and report its exit, timing, and trimmed output.

Runs argv directly (no shell), stdin at /dev/null, in its own process group. Exit code is 0 whenever the program started, including on timeout; the child's own status is exit_code/signal. ANSI escapes are removed and \r-redrawn progress lines keep only their last state unless --no-strip-ansi. Known secret formats in the output are masked before --grep sees it.

| Argument | Type | Default | Description |
|---|---|---|---|
| `argv` (positional, required) | string |  | Program and arguments, after --. |
| `--timeout` | duration | `120s` | SIGTERM to the process group when exceeded, SIGKILL 1s later; timed_out is then true. |
| `--head` | integer | `50` | Leading lines kept per stream. |
| `--tail` | integer | `150` | Trailing lines kept per stream. |
| `--grep` | regex |  | Report every matching line of the full output (before head/tail trimming) as matches[{n,text}]. |
| `--grep-limit` | integer | `50` | Matches reported per stream; more makes the result partial (exit 3). |
| `--no-strip-ansi` | flag |  | Keep ANSI escapes and \r redraws. |
| `--stdout-to` | string |  | Write stdout, unmodified, to this new file (inside the workspace or the mktemp area; never overwrites; not journaled). stdout then reports {path,bytes}. |

| Output field | Description |
|---|---|
| `exit_code` | The child's exit code, or null when a signal ended it. |
| `signal` | The signal that ended the child, or null. |
| `timed_out` | True when --timeout ended the child. |
| `duration_ms` | Wall time from start to exit. |
| `stdout` | {head,tail,total_lines,truncated,matches?,total_matches?}; head and tail never overlap. With --stdout-to, {path,bytes}. |
| `stderr` | Same shape as stdout. |
| `redactions` | Secrets masked across both streams. |
| `capture_capped` | Present and true when a stream exceeded 64 Mi characters; later output was not captured. |

Errors: `argument.invalid`, `input.syntax-error`, `refusal.outside-workspace`, `refusal.exists`, `environment.unavailable`, `environment.io`.

### `wait` {#wait}

Block until one condition holds: a file line, an open port, a bg log line, a bg exit, or a duration.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--file` | string |  | File to watch; with --pattern. |
| `--pattern` | regex |  | Line pattern for --file or --bg. |
| `--port` | integer |  | TCP port that must accept a connection on 127.0.0.1 or ::1. |
| `--bg` | string |  | bg ID; with --pattern (log line) or --exit. |
| `--exit` | flag |  | With --bg: wait for the process to end. |
| `--duration` | duration |  | Wait this long. |
| `--timeout` | duration | `60s` | Give up after this long with environment.timeout. |

| Output field | Description |
|---|---|
| `matched` | Always true on success. |
| `elapsed_ms` | Time until the condition held. |
| `line` | The first matching line (pattern conditions). |
| `redactions` | Secrets masked in line (pattern conditions). |
| `port` | The port that accepted (--port). |
| `exit_code` | Exit code, or null (--bg --exit). |
| `signal` | Ending signal, or null (--bg --exit). |
| `duration_ms` | The duration waited (--duration). |

Errors: `argument.invalid`, `input.syntax-error`, `input.not-found`, `environment.timeout`, `environment.unavailable`, `environment.io`.

### `bg start` {#bg-start}

Start a program detached from aitools, logging stdout and stderr to the workspace state directory.

The process gets its own session and keeps running after aitools exits. A small sh supervisor records its exit status; argv reaches it only as "$@" and is never parsed by a shell.

| Argument | Type | Default | Description |
|---|---|---|---|
| `argv` (positional, required) | string |  | Program and arguments, after --. |
| `--name` | string |  | Label shown by bg status (1-64 characters). |

| Output field | Description |
|---|---|
| `id` | bg ID (bg-<n>) for bg logs/status/stop and wait --bg. |
| `name` | The --name label, or null. |
| `pid` | PID of the supervisor, which leads the process's session and process group. |
| `log` | Absolute path of the combined stdout/stderr log. |

Errors: `argument.invalid`, `environment.unavailable`, `environment.busy`, `environment.io`.

### `bg logs` {#bg-logs}

Read a bg process's log: the last lines, or the lines from a byte offset.

Without --from, the last --tail lines. With --from, up to --tail lines starting at that byte offset; next_offset continues exactly after the last returned line. The read position is kept by the caller only.

| Argument | Type | Default | Description |
|---|---|---|---|
| `id` (positional, required) | string |  | bg ID. |
| `--tail` | integer | `100` | Lines returned. |
| `--from` | integer |  | Byte offset to read from, usually a previous next_offset. |
| `--grep` | regex |  | Only lines matching this pattern. |
| `--no-strip-ansi` | flag |  | Keep ANSI escapes and \r redraws. |

| Output field | Description |
|---|---|
| `id` | bg ID. |
| `running` | Whether the process is still running. |
| `lines` | Log lines. |
| `truncated` | True when lines were left out (status partial, exit 3). |
| `next_offset` | Byte offset for the next --from. |
| `redactions` | Secrets masked in lines. |

Errors: `argument.invalid`, `input.syntax-error`, `input.not-found`, `environment.unavailable`, `environment.io`.

### `bg status` {#bg-status}

List the bg processes aitools started in this workspace, or one of them.

| Argument | Type | Default | Description |
|---|---|---|---|
| `id` (positional) | string |  | Only this bg ID. |

| Output field | Description |
|---|---|
| `items` | [{id,name,pid,argv,running,exit_code,signal,started}]; started is UTC RFC 3339. |
| `total` | Number of items. |

Errors: `input.not-found`, `environment.unavailable`, `environment.io`.

### `bg stop` {#bg-stop}

Stop a bg process: SIGTERM to its process group, SIGKILL if it outlives --grace.

| Argument | Type | Default | Description |
|---|---|---|---|
| `id` (positional, required) | string |  | bg ID. |
| `--grace` | duration | `5s` | Time between SIGTERM and SIGKILL. |

| Output field | Description |
|---|---|
| `id` | bg ID. |
| `stopped` | False when the process had already ended. |
| `exit_code` | Exit code, or null. |
| `signal` | Ending signal (15 or 9 when stopped here), or null. |

Errors: `argument.invalid`, `input.not-found`, `environment.unavailable`, `environment.io`.


## The vcs context

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


## The env context

### `sys info` {#sys-info}

Describe the host: OS, architecture, CPUs, user, memory, and the workspace's disk.

Linux reads /proc; Darwin runs sysctl and vm_stat. Memory available is MemAvailable on Linux and free+inactive+speculative pages on Darwin. Unknown values are null.

| Output field | Description |
|---|---|
| `os` | Kernel name, lowercase (linux, darwin). |
| `os_version` | Kernel release from uname(2). |
| `arch` | Machine from uname(2): x86_64, aarch64 (Linux), arm64 (Darwin). |
| `cpus` | Logical CPUs. |
| `user` | User name. |
| `uid` | Real user id. |
| `hostname` | Host name. |
| `shell` | $SHELL, or null. |
| `memory` | {total, available} in bytes. |
| `disk` | {total, available} in bytes for the file system of the workspace root. |

Errors: `internal.unexpected`.

### `sys env` {#sys-env}

List environment variables, optionally those whose name starts with PREFIX.

A variable whose name contains a secret key word (password, secret, token, api_key, ...) as an underscore-separated word has its value replaced by [REDACTED_SECRET]; every other value still has known secret formats masked.

| Argument | Type | Default | Description |
|---|---|---|---|
| `prefix` | string |  | Case-sensitive name prefix. |

| Output field | Description |
|---|---|
| `items` | [{name, value}] sorted by name. |
| `total` | Number of items. |
| `redactions` | Values replaced because of a secret-looking name. |

Errors: `internal.unexpected`.

### `sys tools` {#sys-tools}

Find commands on PATH and report the first line of their version output.

Without names, probes the default list (git, nix, sbcl, node, npm, cargo, rustc, go, python3, make, gcc, clang, docker, rg, jq). Version is the first nonblank line of `--version` (`go version` for go), stdout first, then stderr; null when the command did not finish within --timeout.

| Argument | Type | Default | Description |
|---|---|---|---|
| `names` | string[] |  | Command names (no `/`). |
| `--timeout` | duration | `5s` | Per-command limit. |

| Output field | Description |
|---|---|
| `items` | [{name, path, version}]; path is null when not found on PATH. |
| `total` | Number of items. |

Errors: `argument.invalid`, `internal.unexpected`.

### `sys procs` {#sys-procs}

List processes whose command line contains PATTERN (case-insensitive), ordered by pid.

Linux reads /proc; Darwin runs ps. Only reads: no process is signalled. More matches than --limit give status partial, truncated true, exit code 3.

| Argument | Type | Default | Description |
|---|---|---|---|
| `pattern` | string |  | Case-insensitive substring of the command line. |
| `--limit` | integer | `50` | Maximum items (at least 1). |

| Output field | Description |
|---|---|
| `items` | [{pid, ppid, user, command, started}]; started is UTC ISO 8601. |
| `total` | Matching processes before --limit. |
| `truncated` | Present and true when items were cut at --limit. |

Errors: `environment.unavailable`, `internal.unexpected`.

### `sys ports` {#sys-ports}

List TCP sockets in LISTEN state with the owning process.

Linux reads /proc/net/tcp and tcp6 and maps socket inodes through /proc/<pid>/fd (other users' processes stay null without privileges); Darwin runs lsof. The wildcard address is 0.0.0.0 or ::.

| Output field | Description |
|---|---|
| `items` | [{port, address, protocol, pid, command}] by port. |
| `total` | Number of items. |

Errors: `environment.unavailable`, `internal.unexpected`.

### `time now` {#time-now}

Show the current time in a zone, in UTC, and as epoch milliseconds.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--tz` | string |  | IANA zone name read from the TZif database ($TZDIR, /usr/share/zoneinfo, /usr/lib/zoneinfo, /usr/share/lib/zoneinfo, /etc/zoneinfo). UTC needs no database. Default: $TZ, then the /etc/localtime link, then UTC. |

| Output field | Description |
|---|---|
| `iso8601` | Local time in the zone with its offset. |
| `utc` | The same instant in UTC (Z). |
| `epoch_ms` | The same instant in Unix epoch milliseconds. |
| `timezone` | Zone name used. |
| `utc_offset` | Offset in effect, e.g. +09:00. |
| `abbreviation` | Zone abbreviation in effect, e.g. JST, EDT. |

Errors: `argument.invalid`, `input.syntax-error`, `environment.unavailable`, `internal.unexpected`.

### `time convert` {#time-convert}

Convert a time value to ISO 8601 or epoch, optionally shifted by durations.

--add/--sub move the instant (1d = 24 hours), not the wall clock. epoch_s rounds toward the past.

| Argument | Type | Default | Description |
|---|---|---|---|
| `value`, required | time |  | ISO 8601 (2026-03-08, 2026-03-08T01:30, 2026-03-08 01:30:00.250+09:00, 20260308T013000Z) or Unix epoch: all digits, 12 or more integer digits = milliseconds, fewer = seconds (optional .fraction). `now` reads the clock. An ISO time without an offset is local time in the zone; a wall time repeated by a DST change resolves to the earlier instant, one skipped by it is read with the offset before the change. |
| `--to` | enum: `iso8601`, `epoch_ms`, `epoch_s` | `iso8601` | Output format. |
| `--add` | duration[] |  | <number>ms\|s\|m\|h\|d, repeatable. |
| `--sub` | duration[] |  | <number>ms\|s\|m\|h\|d, repeatable; the only way to go back in time. |
| `--tz` | string |  | IANA zone name read from the TZif database ($TZDIR, /usr/share/zoneinfo, /usr/lib/zoneinfo, /usr/share/lib/zoneinfo, /etc/zoneinfo). UTC needs no database. Default: $TZ, then the /etc/localtime link, then UTC. |

| Output field | Description |
|---|---|
| `input` | VALUE as given. |
| `input_format` | Detected format: iso8601, epoch_s, epoch_ms, or now. |
| `to` | Output format. |
| `result` | The converted value: a string for iso8601, an integer otherwise. |
| `timezone` | Zone used for offset-less input and iso8601 output. |

Errors: `argument.invalid`, `input.syntax-error`, `environment.unavailable`, `internal.unexpected`.

### `time diff` {#time-diff}

Compute B minus A.

| Argument | Type | Default | Description |
|---|---|---|---|
| `a`, required | time |  | ISO 8601 (2026-03-08, 2026-03-08T01:30, 2026-03-08 01:30:00.250+09:00, 20260308T013000Z) or Unix epoch: all digits, 12 or more integer digits = milliseconds, fewer = seconds (optional .fraction). `now` reads the clock. An ISO time without an offset is local time in the zone; a wall time repeated by a DST change resolves to the earlier instant, one skipped by it is read with the offset before the change. |
| `b`, required | time |  | Same formats as a. |

| Output field | Description |
|---|---|
| `diff_ms` | B minus A in milliseconds (negative when B is earlier). |
| `human` | The same as days, hours, minutes, seconds, ms with zero parts omitted, e.g. 1h23m. |

Errors: `argument.invalid`, `input.syntax-error`, `environment.unavailable`, `internal.unexpected`.


## The util context

### `util encode` {#util-encode}

Encode the input bytes as base64, URL percent-encoding, or hex.

| Argument | Type | Default | Description |
|---|---|---|---|
| `scheme`, required | enum: `base64`, `url`, `hex` |  | base64: RFC 4648 standard alphabet with padding. url: RFC 3986 percent-encoding (unreserved bytes kept). hex: lowercase pairs. |
| `--content` | string |  | Input text; commands that work on bytes use its UTF-8 encoding. |
| `--content-file` | path |  | Read the input as raw bytes from this file (at most 64 MiB). Reads are not limited to the workspace. |
| `--stdin` | flag |  | Read the input as UTF-8 text from standard input (at most 64 MiB). Standard input is never read otherwise. |

| Output field | Description |
|---|---|
| `output` | The encoded ASCII text. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `environment.io`.

### `util decode` {#util-decode}

Decode base64, URL percent-encoding, or hex; binary results are returned as hex, or written to a file with --to.

| Argument | Type | Default | Description |
|---|---|---|---|
| `scheme`, required | enum: `base64`, `url`, `hex` |  | base64: RFC 4648 standard alphabet with padding. url: RFC 3986 percent-encoding (unreserved bytes kept). hex: lowercase pairs. |
| `--content` | string |  | Input text; commands that work on bytes use its UTF-8 encoding. |
| `--content-file` | path |  | Read the input as raw bytes from this file (at most 64 MiB). Reads are not limited to the workspace. |
| `--stdin` | flag |  | Read the input as UTF-8 text from standard input (at most 64 MiB). Standard input is never read otherwise. |
| `--to` | path |  | Write the decoded bytes, as they are, to this path through the atomic write protocol (journaled; undo deletes the file). The path must not exist (refusal.exists) and must be inside the workspace or the mktemp area. |
| `--dry-run` | flag |  | With --to: validate and show the change without writing. |
| `--tx` | string |  | With --to: stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `bytes` | Decoded byte count. |
| `output` | The decoded bytes as text, when they are valid UTF-8 (not with --to). |
| `binary` | true when the decoded bytes are not valid UTF-8; `output` is then absent (not with --to). |
| `output_hex` | The decoded bytes as lowercase hex, present only with binary:true (not with --to). |
| `changes` | With --to: the standard write output, one created file. |
| `op_id` | With --to: the journal op (undo it to remove the file); tx and tx_op with --tx, dry_run with --dry-run. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `environment.io`, `input.syntax-error`, `refusal.exists`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.not-a-file`, `environment.busy`.

### `util redact` {#util-redact}

Mask known secret formats in the input text.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--content` | string |  | Input text; commands that work on bytes use its UTF-8 encoding. |
| `--content-file` | path |  | Read the input as raw bytes from this file (at most 64 MiB). Reads are not limited to the workspace. |
| `--stdin` | flag |  | Read the input as UTF-8 text from standard input (at most 64 MiB). Standard input is never read otherwise. |

| Output field | Description |
|---|---|
| `text` | The input with each secret replaced by [REDACTED_SECRET]. Invalid UTF-8 in --content-file becomes U+FFFD. |
| `redactions` | Number of masked regions. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `environment.io`.

### `util tokens` {#util-tokens}

Measure the input: approximate tokens, characters, bytes, lines, words, longest line.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--content` | string |  | Input text; commands that work on bytes use its UTF-8 encoding. |
| `--content-file` | path |  | Read the input as raw bytes from this file (at most 64 MiB). Reads are not limited to the workspace. |
| `--stdin` | flag |  | Read the input as UTF-8 text from standard input (at most 64 MiB). Standard input is never read otherwise. |

| Output field | Description |
|---|---|
| `approx_tokens` | ceiling(chars / 4); not tied to any model's tokenizer. |
| `chars` | Unicode scalar values (invalid UTF-8 in --content-file counts one U+FFFD per invalid sequence). |
| `bytes` | Input size in bytes. |
| `lines` | Newline count, plus one for an unterminated last line. |
| `words` | Runs of non-whitespace separated by ASCII whitespace. |
| `max_line_chars` | Longest line in characters, excluding CR LF. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `environment.io`.

### `util calc` {#util-calc}

Evaluate integer, decimal, and rational arithmetic with arbitrary precision.

| Argument | Type | Default | Description |
|---|---|---|---|
| `expression` | string |  | Integers, decimals (1.5), + - * / % **, parentheses, min max abs floor ceil round. ** binds tighter than unary minus and is right-associative; its exponent must be an integer. % takes the divisor's sign. round rounds half away from zero. No variables, assignment, or other functions. At most 4096 characters, 64 nesting levels, and 65536 bits per intermediate value. Exactly one of expression or --stdin. An expression that starts with - must follow -- (aitools util calc -- -2**2). |
| `--stdin` | flag |  | Read the expression from standard input. |
| `--decimals` | integer | `10` | Fractional digits of result, 0 to 1000, rounded half away from zero; trailing zeros are dropped. |

| Output field | Description |
|---|---|
| `input` | The evaluated expression. |
| `result` | The value in decimal, as a string so big integers stay exact. |
| `exact` | The exact value as N/D; present only when the value is not an integer. |

Errors: `argument.invalid`, `input.syntax-error`, `input.not-utf8`, `environment.io`.

### `util uuid` {#util-uuid}

Generate RFC 9562 UUIDs from the OS cryptographic random source.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--kind` | enum: `v4`, `v7` | `v4` | v4: random. v7: Unix-millisecond timestamp plus a counter; values from one call strictly increase. |
| `--count` | integer | `1` | 1 to 1000. |

| Output field | Description |
|---|---|
| `values` | Lowercase 8-4-4-4-12 UUID strings. |

Errors: `argument.invalid`.

### `util random` {#util-random}

Generate uniformly random strings over hex, alnum, or base64url.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--length` | integer | `32` | 1 to 4096. |
| `--alphabet` | enum: `hex`, `alnum`, `base64url` | `hex` | hex: 0-9a-f. alnum: A-Za-z0-9. base64url: A-Za-z0-9-_. |
| `--count` | integer | `1` | 1 to 1000. |

| Output field | Description |
|---|---|
| `values` | The generated strings. |

Errors: `argument.invalid`.


## Composition root

### `schema` {#schema}

List implemented commands, or show one command's full schema.

With no arguments, list every implemented command as {name, summary}. With command words (a group word followed by its subcommand, e.g. `schema json get`), return those commands' full detail. `--all` returns every command's detail.

| Argument | Type | Default | Description |
|---|---|---|---|
| `commands` | string, repeatable |  | Command names as typed, e.g. `read` or `json get`. |
| `--all` | flag |  | Return every command's full detail. |

| Output field | Description |
|---|---|
| `commands` | Array of {name, summary}, or of full detail objects. |

Errors: `input.not-found`, `argument.invalid`.

### `batch` {#batch}

Run several aitools invocations from one JSON input, optionally as one tx.

--stdin is a JSON array of argv arrays without the leading aitools, e.g. [["read","a.lisp"],["edit","a.lisp","--old","x","--new","y"]]. Each element is parsed and validated exactly as a standalone call, with batch's --root and --lock-timeout placed before it. Elements run in order; after the first failure the rest are skipped unless --continue-on-error. Without --atomic each write commits on its own: undo the successful elements' op_id values to reverse a partial batch. --atomic runs tx begin, every element with --tx <tx>, then tx commit, as one op; a failing element or a conflicting commit aborts the tx and writes nothing. An element may not be a batch, may not read --stdin (batch has consumed it; pass --content or --stdin-data), and under --atomic may not carry --tx. On failure the answer is an error with the first failing element's error.code and exit code, and results[] in error.diagnostics.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--stdin`, required | flag |  | Read the argv arrays from standard input. |
| `--continue-on-error` | flag |  | Keep running after a failed element; not with --atomic. |
| `--atomic` | flag |  | All elements in one tx, committed at the end, or nothing; not with --continue-on-error. |

| Output field | Description |
|---|---|
| `results` | One envelope per element, in order; {status:"skipped",argv} for an element not run after a failure. |
| `tx` | --atomic: the tx the elements were staged in. |
| `changes` | --atomic: the committed tx's changes, in the standard write output shape. |
| `op_id` | --atomic: the single journal op of the whole batch (undo reverses it). |

Errors: `argument.invalid`, `input.syntax-error`, `refusal.target-changed`, `environment.busy`.

<!-- END GENERATED: commands -->

## Not provided

The binary does not register the following. Passing an unregistered option
fails with `argument.invalid`; an unregistered command name fails with
`argument.invalid` and a repair from the
[correspondence table](../guide/agents.md#shell-commands-and-their-aitools-replacements).

| Not provided | Use instead |
|---|---|
| `search --encoding <name>` | `read` and `table read` take `--encoding`. |
| `json select --stdin` | `json select` reads a file argument only. |
| A benchmark suite | None is committed. See [Benchmarks](benchmarks.md) for what was measured. |

## Schemas match the parser

Every registered command's schema lists exactly the options its parser
accepts. The `*known-drift*` list in
`t/integration/cli-schema-drift-test.lisp` is empty, and the drift test holds
every command to that rule.
