# The search context

[Command index](index.md#command-index).

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

