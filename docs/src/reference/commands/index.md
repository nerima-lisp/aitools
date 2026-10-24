# Commands

This reference lists every command the binary registers. Each context page
lists its commands' arguments, output fields, and declared error codes. The
same information is available at run time:

```sh
aitools schema                 # every command's name and summary
aitools schema json get read   # full detail for the named commands
aitools schema --all           # full detail for every command
aitools read --help            # full detail for one command
```

The command index and context pages are generated from the registered command
schemas, so they match `aitools schema --all`. To regenerate them after a
change, see
[Regenerating the reference](../../project/development.md#regenerating-the-reference).
Options and commands that are not provided are listed under
[Not provided](#not-provided).

## Global options

These options belong to the application, not to one command. They are
accepted before or after the command name.

| Option | Description |
|---|---|
| `--root <dir>` | Workspace root. Without it, aitools uses the enclosing git root, then the working directory. |
| `--lock-timeout <duration>` | How long a write, or recovery before a command, waits for the workspace lock. Default `10s`. |
| `--trace` | Adds command, child-process, and store IO spans under `meta.trace`. |
| `--version` | Prints `{"schema_version":2,"status":"ok","command":"version","name":"aitools","version":...}`. |
| `--help` | After a command name, prints that command's schema detail object. Without a command, prints the command list. |

`--help` output is the bare schema object (`name`, `summary`, `description`,
`args`, `output_fields`, `error_codes`) for a command, or
`{"schema_version":2,"status":"ok","command":"schema","commands":[...]}`
without one. It is not wrapped in the success envelope that `aitools schema`
uses.

A relative path argument resolves against the current working directory,
as in a shell. `--root` selects the workspace (the write boundary, the state
directory, the ignore rules); it is not the base for relative paths. Paths in
write output are relative to the workspace root. `read`, `info`, `check`,
and `diff` report a file inside the root relative to it and a file outside
the root by its absolute real path; see [JSON output](../json-schema.md#reads).

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
- **Input** comes from command-specific sources such as `--content`,
  `--content-file`, or `--stdin`. aitools reads standard input only when
  `--stdin` is given. For content-input writes, `--stdin-data <text>` supplies
  the same payload inline; the journal records this as `--stdin`. Every
  command that accepts `--stdin-data` also accepts `--stdin`, but not the
  reverse: `search`, the `util` commands, and `batch` read `--stdin` and have
  no `--stdin-data`.
- **Writes** accept `--dry-run` (validate and show the diff, write nothing)
  and `--tx <tx>` (stage the write in a transaction), except where the
  argument table omits them. Every write returns the shape described in
  [JSON output](../json-schema.md#write-output).
- **Limits** are command-specific: `read --max-lines` and list-like commands
  such as `search` or `find` use `--limit`; `json get --max-bytes` limits a
  rendered JSON value. When more data remains, the response becomes
  `status:"partial"` with exit code 3 and a `next_commands` entry where the
  command supports paging.
- **Durations** are `<n>ms`, `<n>s`, `<n>m`, `<n>h`, or `<n>d`. **Sizes** are
  `<n>`, `<n>KiB`, `<n>MiB`, or `<n>GiB`. `<n>` is written in ASCII digits,
  optionally with a decimal fraction; a sign, whitespace, or a digit from
  another script fails with `argument.invalid`.

The meaning and exit code of each error code are in
[Errors and exit codes](../errors.md).

## YAML and TOML write formatting

YAML and TOML write commands serialize the whole document through the format's
value serializer.
Comments are dropped, with a `format.comments-dropped` warning when the input
contains comments.
The `yaml fmt` and `toml fmt` commands emit UTF-8 without a BOM, use LF line
endings, and end with a newline.
A second run of either formatter is a byte-for-byte no-op.
`toml fmt` emits each table's value entries first, then its child tables, then
its arrays of tables.
It preserves source order within each of those groups.
On the first format pass, this changes any input interleaving among those
groups to the fixed output order.

## Command index

<!-- BEGIN GENERATED: commands -->
| Command | Context | Summary |
|---|---|---|
| [`search`](search.md#search) | search | Search file contents for a regular expression; results are grouped into blocks with context. |
| [`find`](search.md#find) | search | List files and directories by name or path pattern; ls, find, tree, and du in one. |
| [`overview`](search.md#overview) | search | Summarize the workspace: root, git state, languages, build files, and top-level entries. |
| [`code outline`](search.md#code-outline) | search | List the definitions in one source file with their line ranges. |
| [`code defs`](search.md#code-defs) | search | Find where a name is defined across the workspace's source files. |
| [`code refs`](search.md#code-refs) | search | Find the lines that mention a name as a whole identifier. |
| [`read`](inspect.md#read) | inspect | Read a file's lines with line numbers, a selector, and a line limit. |
| [`info`](inspect.md#info) | inspect | Describe a path: location, kind, size, lines, words, encoding, mode, hash, and digests. |
| [`check`](inspect.md#check) | inspect | Check a JSON file's syntax or a Lisp file's delimiter balance. |
| [`diff`](inspect.md#diff) | inspect | Compare two files or directories, or show an op's full diff. |
| [`json get`](inspect.md#json-get) | inspect | Get the value at a JSON pointer, its keys, length, or raw string. |
| [`json select`](inspect.md#json-select) | inspect | Filter, sort, and project the elements of a JSON array. |
| [`json diff`](inspect.md#json-diff) | inspect | List the add, remove, and replace operations between two JSON files. |
| [`yaml get`](inspect.md#yaml-get) | inspect | Get a value from a YAML document. |
| [`yaml select`](inspect.md#yaml-select) | inspect | Filter, sort, and project a YAML array. |
| [`yaml diff`](inspect.md#yaml-diff) | inspect | List value changes between two YAML files. |
| [`toml get`](inspect.md#toml-get) | inspect | Get a value from a TOML document. |
| [`toml select`](inspect.md#toml-select) | inspect | Filter, sort, and project a TOML array. |
| [`toml diff`](inspect.md#toml-diff) | inspect | List value changes between two TOML files. |
| [`table read`](inspect.md#table-read) | inspect | Read rows of a CSV, TSV, JSONL, JSON, whitespace, separator, or line table. |
| [`table agg`](inspect.md#table-agg) | inspect | Group table rows and count, sum, average, or take min, max, or distinct counts. |
| [`archive list`](inspect.md#archive-list) | inspect | List the members of a zip, tar, tar.gz, or gz archive. |
| [`archive read`](inspect.md#archive-read) | inspect | Read one archive member in read's shape, with selectors and a line limit. |
| [`snapshot create`](inspect.md#snapshot-create) | inspect | Record every workspace file's size, mtime, and content hash for a later snapshot diff. |
| [`snapshot diff`](inspect.md#snapshot-diff) | inspect | List files added, removed, or modified since a snapshot. |
| [`edit`](edit.md#edit) | edit | Replace text selected by --old or a selector; --new '' deletes it. |
| [`insert`](edit.md#insert) | edit | Insert content at the start or end of a file, or before/after the lines a selector picks. |
| [`replace`](edit.md#replace) | edit | Regex (or --fixed) replace across files or within one file's selection, with $1/${name} replacement templates. |
| [`apply`](edit.md#apply) | edit | Apply a unified diff from --stdin to the workspace, all files or none. |
| [`transform`](edit.md#transform) | edit | Apply line operations (sort, unique, wrap, comment, ...) to a file or a selection. |
| [`move-lines`](edit.md#move-lines) | edit | Move selected lines to another place in the same file or into another file. |
| [`write`](edit.md#write) | edit | Create a file from --content, --content-file (repeatable, concatenated) or --stdin. |
| [`split`](edit.md#split) | edit | Split a file into numbered pieces by line count, before matching lines, or by bytes. |
| [`transcode`](edit.md#transcode) | edit | Convert a file between utf-8, shift_jis (CP932), euc-jp, iso-8859-1, utf-16le and utf-16be. |
| [`move`](edit.md#move) | edit | Move or rename a file or directory; never merges into an existing directory. |
| [`copy`](edit.md#copy) | edit | Copy a file, or a directory with --recursive, byte for byte. |
| [`rm`](edit.md#rm) | edit | Remove one file, symlink, or empty directory; journaled and undoable. |
| [`delete`](edit.md#delete) | edit | Delete a file, a symlink, or an empty directory. |
| [`mkdir`](edit.md#mkdir) | edit | Create a directory and its parents; an existing directory is a no-op. |
| [`chmod`](edit.md#chmod) | edit | Set or clear the executable bits, or set an octal mode. |
| [`link`](edit.md#link) | edit | Create a symlink LINK pointing at TARGET inside the workspace. |
| [`touch`](edit.md#touch) | edit | Create an empty file, or set an existing file's modification time. |
| [`mktemp`](edit.md#mktemp) | edit | Create a temporary file or directory in the workspace's state tmp/ area (not journaled). |
| [`json set`](edit.md#json-set) | edit | Set the value at a JSON Pointer (/- appends to an array). |
| [`json delete`](edit.md#json-delete) | edit | Delete the value at a JSON Pointer. |
| [`json merge`](edit.md#json-merge) | edit | Apply an RFC 7386 JSON Merge Patch read from --stdin. |
| [`json patch`](edit.md#json-patch) | edit | Apply an RFC 6902 JSON Patch read from --stdin; all operations or none. |
| [`json fmt`](edit.md#json-fmt) | edit | Re-indent a JSON file, keeping key order unless --sort-keys. |
| [`table set`](edit.md#table-set) | edit | Set one cell of a CSV or TSV file by data row and column name. |
| [`archive extract`](edit.md#archive-extract) | edit | Extract a zip, tar, tar.gz or gz archive after validating every entry. |
| [`archive create`](edit.md#archive-create) | edit | Create a zip, tar, tar.gz or gz archive from workspace files (ignored files excluded). |
| [`yaml set`](edit.md#yaml-set) | edit | Set a JSON value in a YAML document at a JSON Pointer; string values need JSON quotes. |
| [`yaml delete`](edit.md#yaml-delete) | edit | Delete a value from a YAML document at a JSON Pointer. |
| [`yaml merge`](edit.md#yaml-merge) | edit | Apply an RFC 7386 JSON Merge Patch from --stdin to a YAML document. |
| [`yaml patch`](edit.md#yaml-patch) | edit | Apply an RFC 6902 JSON Patch from --stdin to a YAML document; all operations or none. |
| [`yaml fmt`](edit.md#yaml-fmt) | edit | Format a YAML document; a second fmt is a byte-for-byte no-op. |
| [`toml set`](edit.md#toml-set) | edit | Set a JSON value in a TOML document at a JSON Pointer; string values need JSON quotes. |
| [`toml delete`](edit.md#toml-delete) | edit | Delete a value from a TOML document at a JSON Pointer. |
| [`toml merge`](edit.md#toml-merge) | edit | Apply an RFC 7386 JSON Merge Patch from --stdin to a TOML document. |
| [`toml patch`](edit.md#toml-patch) | edit | Apply an RFC 6902 JSON Patch from --stdin to a TOML document; all operations or none. |
| [`toml fmt`](edit.md#toml-fmt) | edit | Format a TOML document. Value entries precede child tables and arrays of tables; order within each group is preserved, groups may reorder, and a second fmt is a byte-for-byte no-op. |
| [`history`](journal.md#history) | journal | List journaled operations (op_id, command, paths, time), newest first. |
| [`undo`](journal.md#undo) | journal | Revert one journaled op, checking first that none of its paths changed since. |
| [`tx begin`](journal.md#tx-begin) | journal | Open a transaction: --tx writes stack up in it without touching the working tree. |
| [`tx status`](journal.md#tx-status) | journal | List open transactions, or show one tx's operations, paths, drift and stale reads. |
| [`tx diff`](journal.md#tx-diff) | journal | Show what committing a tx would change, as write-output changes without an op_id. |
| [`tx drop`](journal.md#tx-drop) | journal | Undo one tx operation and every later one, restoring the tx state before it. |
| [`tx rebase`](journal.md#tx-rebase) | journal | Re-apply a tx's operations onto files that changed outside it, moving its base forward. |
| [`tx commit`](journal.md#tx-commit) | journal | Write a whole tx to the working tree as one journaled op, or report its conflicts. |
| [`tx abort`](journal.md#tx-abort) | journal | Discard a tx without touching the working tree. |
| [`run`](process.md#run) | process | Run a program without a shell and report its exit, timing, and trimmed output. |
| [`wait`](process.md#wait) | process | Block until one condition holds: a file line, an open port, a bg log line, a bg exit, or a duration. |
| [`signal`](process.md#signal) | process | Signal same-UID processes selected by PID or command-line pattern. |
| [`bg start`](process.md#bg-start) | process | Start a program detached from aitools, logging stdout and stderr to the workspace state directory. |
| [`bg logs`](process.md#bg-logs) | process | Read a bg process's log: the last lines, or the lines from a byte offset. |
| [`bg status`](process.md#bg-status) | process | List the bg processes aitools started in this workspace, or one of them. |
| [`bg stop`](process.md#bg-stop) | process | Stop a bg process: SIGTERM to its process group, SIGKILL if it outlives --grace. |
| [`git status`](vcs.md#git-status) | vcs | Show the branch, upstream distance, and staged, unstaged, and untracked paths. |
| [`git log`](vcs.md#git-log) | vcs | List the newest commits, optionally only those touching a path. |
| [`git diff`](vcs.md#git-diff) | vcs | Show changed files with line counts and, in hunks mode, their hunks. |
| [`git blame`](vcs.md#git-blame) | vcs | Show who last changed each line of a work-tree file. |
| [`git show`](vcs.md#git-show) | vcs | Read a file as stored at a revision, in read's shape. |
| [`org outline`](org.md#org-outline) | org | List Org headings and named or unnamed source and example blocks. |
| [`org query`](org.md#org-query) | org | Find Org headings that match an Org match expression. |
| [`org export`](org.md#org-export) | org | Export an Org file as HTML, GFM, or ASCII text. |
| [`org todo set`](org.md#org-todo-set) | org | Change the TODO keyword on one matching heading. |
| [`org property set`](org.md#org-property-set) | org | Set one property on one matching heading. |
| [`org property delete`](org.md#org-property-delete) | org | Delete one property from one matching heading. |
| [`org planning set`](org.md#org-planning-set) | org | Set one planning timestamp on one matching heading. |
| [`org planning delete`](org.md#org-planning-delete) | org | Delete one planning timestamp from one matching heading. |
| [`org clock in`](org.md#org-clock-in) | org | Start a clock on one matching heading. |
| [`org clock out`](org.md#org-clock-out) | org | Stop the running clock on one matching heading. |
| [`org agenda`](org.md#org-agenda) | org | List Org agenda entries in an inclusive date range. |
| [`org clock report`](org.md#org-clock-report) | org | Summarize clock time for one heading and its descendants. |
| [`org table align`](org.md#org-table-align) | org | Align one numbered Org table. |
| [`org table sort`](org.md#org-table-sort) | org | Sort the standard rows of one numbered Org table. |
| [`org table recalculate`](org.md#org-table-recalculate) | org | Recalculate formulas for one numbered Org table. |
| [`sys info`](env.md#sys-info) | env | Describe the host: OS, architecture, CPUs, user, memory, and the workspace's disk. |
| [`sys env`](env.md#sys-env) | env | List environment variables, optionally those whose name starts with PREFIX. |
| [`sys tools`](env.md#sys-tools) | env | Find commands on PATH and report the first line of their version output. |
| [`sys procs`](env.md#sys-procs) | env | List processes whose command line contains PATTERN (case-insensitive), ordered by pid. |
| [`sys metrics`](env.md#sys-metrics) | env | Sample CPU, memory, and process metrics once without an interactive display. |
| [`sys network`](env.md#sys-network) | env | Read cumulative network interface counters, ordered by name. |
| [`sys disk`](env.md#sys-disk) | env | Read capacity of the filesystem containing PATH (default: workspace root). |
| [`sys ports`](env.md#sys-ports) | env | List TCP sockets in LISTEN state with the owning process. |
| [`sys snapshot`](env.md#sys-snapshot) | env | Collect host info, metrics, network, disk, and listening TCP ports in one non-atomic response. |
| [`time now`](env.md#time-now) | env | Show the current time in a zone, in UTC, and as epoch milliseconds. |
| [`time convert`](env.md#time-convert) | env | Convert a time value to ISO 8601 or epoch, optionally shifted by durations. |
| [`time diff`](env.md#time-diff) | env | Compute B minus A. |
| [`util encode`](util.md#util-encode) | util | Encode the input bytes as base64, URL percent-encoding, or hex. |
| [`util decode`](util.md#util-decode) | util | Decode base64, URL percent-encoding, or hex; binary results are returned as hex, or written to a file with --to. |
| [`util redact`](util.md#util-redact) | util | Mask known secret formats in the input text. |
| [`util tokens`](util.md#util-tokens) | util | Measure the input: approximate tokens, characters, bytes, lines, words, longest line. |
| [`util calc`](util.md#util-calc) | util | Evaluate integer, decimal, and rational arithmetic with arbitrary precision. |
| [`util uuid`](util.md#util-uuid) | util | Generate RFC 9562 UUIDs from the OS cryptographic random source. |
| [`util random`](util.md#util-random) | util | Generate uniformly random strings over hex, alnum, or base64url. |
| [`schema`](#schema) | composition root | List implemented commands, or show one command's full schema. |
| [`batch`](#batch) | composition root | Run several aitools invocations from one JSON input, optionally as one tx. |

## Composition root

### `schema` {#schema}

List implemented commands, or show one command's full schema.

With no arguments, list every implemented command as {name, summary}. With command words (a group path followed by its subcommand token, e.g. schema org todo set), return those commands' full detail. `--all` returns every command's detail.

| Argument | Type | Default | Description |
|---|---|---|---|
| `commands` | string, repeatable |  | Command names as typed, such as read, json get, or org todo set. |
| `--all` | flag |  | Return every command's full detail. |

| Output field | Description |
|---|---|
| `commands` | Array of {name, summary}, or of full detail objects. |

Errors: `input.not-found`, `argument.invalid`.

### `batch` {#batch}

Run several aitools invocations from one JSON input, optionally as one tx.

--stdin is a JSON array of argv arrays without the leading aitools, e.g. [["read","a.lisp"],["edit","a.lisp","--old","x","--new","y"]]]. Each element is parsed and validated exactly as a standalone call, with batch's --root and --lock-timeout placed before it. Elements run in order; after the first failure the rest are skipped unless --continue-on-error. Without --atomic each write commits on its own: undo the successful elements' op_id values to reverse a partial batch. --atomic runs tx begin, every element with --tx <tx>, then tx commit, as one op; a failing element or a conflicting commit aborts the tx and writes nothing. An element may not be a batch, may not read --stdin (batch has consumed it; pass --content or --stdin-data), and under --atomic may not carry --tx. On failure the answer is an error with the first failing element's error.code and exit code, and results[] in error.diagnostics.

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
| `next_commands` | Commands to show full diffs when a committed change diff is truncated. |

Errors: `argument.invalid`, `input.syntax-error`, `refusal.target-changed`, `environment.busy`, `input.not-found`, `environment.io`, `internal.unexpected`, `input.unsupported-language`, `input.unsupported-format`, `selection.no-match`, `selection.ambiguous`, `refusal.not-a-file`, `input.unsupported-value`, `refusal.too-large`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.exists`, `selection.count-mismatch`, `input.not-utf8`, `environment.unavailable`, `environment.timeout`.

<!-- END GENERATED: commands -->

## Not provided

The binary does not register the following. Passing an unregistered option
fails with `argument.invalid`; an unregistered command name fails with
`argument.invalid` and a repair from the
[correspondence table](../../guide/agents.md#shell-commands-and-their-aitools-replacements).

| Not provided | Use instead |
|---|---|
| `search --encoding <name>` | `read` and `table read` take `--encoding`. |
| `json select --stdin` | `json select` reads a file argument only. |
| A timing benchmark suite | None is committed. See [Benchmarks](../benchmarks.md) for what was measured. |

## Schemas match the parser

Every registered command's schema lists exactly the options its parser
accepts. The `*known-drift*` list in
`t/integration/cli-schema-drift-test.lisp` is empty, and the drift test holds
every command to that rule.
