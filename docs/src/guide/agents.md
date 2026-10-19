# Using aitools from an agent

aitools is meant to be the only tool an agent calls for file and text work.
This page describes the loop an agent runs against it: discover a command,
call it, branch on the exit code, and follow the `next_commands` and
`repairs` the output hands back.

## Discover commands with `schema`

Start with the command list, then ask for the detail of the commands you
plan to use:

```sh
aitools schema                  # [{name, summary}] for every command
aitools schema edit read        # arguments, output fields, error codes
aitools schema json get         # a group command, as typed
```

`schema` without arguments returns only names and summaries, so it stays
short. The detail object lists each argument's type and default, each output
field, and the error codes the command declares. The
[Commands](../reference/commands.md) page shows the same data.

## Branch on the exit code first

| Exit code | What to do |
|---|---|
| 0 | Use the result on standard output. |
| 3 | The result on standard output is incomplete (`status:"partial"`). Follow `next_commands` when present, or narrow the request. For `run`, check the child outcome before considering a retry; see [Run an external program](#run-an-external-program). |
| 2 | The file is not in the state you assumed. Read it again, then retry with a fresh selection, hash, or count. |
| 1 | Read `error.code`; treat `error.repairs` as suggestions and review a command before running it. |

Standard output carries success; standard error carries the error envelope.
Each is exactly one JSON object.

## Paths

A relative path argument resolves against the current working directory,
as in a shell. `--root` selects the workspace (the write boundary, the state
directory, and the ignore rules) and does not change what a relative path
means. Write output gives paths relative to the workspace root. `read`,
`info`, `check`, and `diff` give a file inside the root relative to it and a
file outside the root as its absolute real path.

Built-in reads are not restricted to the workspace. Built-in writes outside it, through a
symlink that leaves it, or into the workspace's `.git/` fail with
`refusal.outside-workspace`. `archive extract` entry paths and
`copy --recursive` children go through the same boundary, so an archive entry
or nested directory that resolves into `.git/` is refused. Extracted files
get the entry's mode masked with `0755`: never group- or other-writable, and
never setuid, setgid, or sticky. The state
directory lives outside the root, so writes there are refused too. The one
exception is the temporary area that `aitools mktemp` creates; writes below
the path it returns are allowed.

## Read in bounded pieces

Every read has a limit, and a cut result says how to continue:

```console
$ aitools read nums.txt --max-lines 3
{"schema_version":2,"status":"partial","command":"read","mode":"text","path":"nums.txt","start_line":1,"lines":["1","2","3"],"total_lines":100,"hash":"93d4e5c7...","truncated":true,"encoding_errors":0,"approx_tokens":2,"next_commands":["aitools read nums.txt --range 4:6"]}
```

`approx_tokens` estimates the size of the returned text as
`ceil(characters / 4)` (a multi-byte UTF-8 character counts once),
independent of any model's tokenizer. Use it to decide whether to read more
or to narrow the range.

## Write with guards

A write states what it expects the file to look like, and fails without
writing when the file differs.

1. Select by content where you can. `edit --old` first searches for an exact
   match. Only if none exists does it compare whole lines, ignoring leading
   and trailing whitespace on each line. The selected strategy must find
   exactly one place; zero or several matches fail with exit code 2 and
   `candidates`. Check the returned `strategy` (`exact` or `whitespace`);
   the whitespace strategy also adjusts the replacement's indentation.
2. When you select by position (`--range`, `--symbol`), pass the file's
   current hash: `aitools info <path>` returns it as `hash`, and so does
   `aitools read`. Without `--expect-hash`, the write fails with
   `argument.invalid` and a repair that runs `aitools info <path>`.
3. When a selection can match many lines (`--match`, `replace`), pass
   `--expect-count`. Run the command with `--dry-run` first: a dry run
   needs no `--expect-count`, writes nothing, and reports the count as
   `expect_count` next to the diff. Then run it without `--dry-run` and with
   `--expect-count <that count>`. A write without `--expect-count` fails
   with `argument.invalid` and a repair that is that `--dry-run`.
4. Keep the `op_id` from the output. `aitools undo <op_id>` reverts that
   operation, and refuses with exit code 2 if a later change touched the
   same paths.

```console
$ aitools edit hello.lisp --old 'Hello' --new 'Hi'
{"schema_version":2,"status":"ok","command":"edit","changes":[{"path":"hello.lisp","action":"modified","hash_before":"de2187a5...","hash_after":"5f132d76...","diff":"@@ -1,2 +1,2 @@\n (defun greet (name)\n-  (format t \"Hello, ~a\" name))\n+  (format t \"Hi, ~a\" name))\n"}],"op_id":"op-20260926T011541Z-4a125e73","strategy":"exact"}
```

Hashes above are shortened for the page; real output carries the full
SHA-256 hex digest.

Never write text copied from output that contains `[REDACTED_SECRET]`.
aitools masks known secret formats in JSON output, and a guarded write whose input
contains the mask text fails with `refusal.redacted-input`. To change nearby
nonsecret text, use `edit --old` and `--new` that exclude the secret, leaving
the existing secret untouched. If the requested change requires the secret
itself, stop and ask for an authorized way to supply it; rereading masked
output does not recover the value.

## Remove a file or tree

Use `rm` for journaled removal of one explicit path. Without `--recursive`,
it removes only a file, symlink, or empty directory. A recursive removal
requires a preview and an expected entry count:

```sh
aitools rm build/scratch --recursive --dry-run
```

Inspect the selected paths and returned count. Repeat the command without
`--dry-run`, adding `--expect-count` with that count. The count includes the
root entry, all nested directories, files, and symlinks. Symlinks are removed
without following their targets. The default limits are 1,000 entries and
64 MiB of regular-file content; `--max-entries` and `--max-bytes` override
them. These limits bound the selected deletion, not process memory.

Workspace, home, and filesystem roots, version-control metadata, and the
state directory are protected. Keep the returned `op_id` to undo the deletion.
Stop external writers to the target tree before running the operation.
Neither a dry run nor the journal reserves the tree against other processes.
If an operation fails after its durable commit, recovery attempts to finish
that committed operation; it does not automatically roll it back. Recovery
refuses a replaced directory or an inaccessible permission checkpoint and
keeps the intent record. In particular, a crash after setting mode `000`
but before syncing it can require operator intervention; aitools does not
temporarily widen permissions to reopen the directory. Inspect diagnostics
before retrying or changing the affected paths.

The journal and undo apply to workspace targets. The `mktemp` scratch area
is not journaled or undoable, and a failure there can leave partial changes.

## Group related writes in a transaction

To group several writes as one journal operation, or check their combined
result before it reaches the files, open a transaction:

```sh
aitools tx begin --name refactor        # returns {"tx": "tx-..."}
aitools edit a.lisp --old ... --new ... --tx tx-...
aitools read b.lisp --tx tx-...         # sees staged content; records the read
aitools tx diff tx-...                  # what commit would change
aitools tx commit tx-...                # one op_id for everything
```

If another process changed a file first, `tx commit` fails with exit code 2
and repairs that name `tx rebase` or a re-read. See
[Transactions](../reference/transactions.md) for the full model.

## Read a resource snapshot

Use `sys snapshot` to collect host information, CPU and memory metrics,
interface counters, filesystem capacity, and listening TCP sockets together:

```sh
aitools sys snapshot --process-limit 20 --network-limit 100 --port-limit 100
```

The disk section uses the filesystem containing an optional positional path,
or the workspace root by default. The info section always uses the workspace.

Inspect each entry in `sections`: `status` distinguishes complete, partial,
and unavailable observations; `reason` explains missing data. The response
includes observation times and `atomic: false`. It is a sequence of reads,
not an atomic or exhaustive view of the machine. A partial section makes the
response partial (exit 3); if every section is unavailable, it fails (exit 1)
and retains section diagnostics.

Item limits bound output, not acquisition time or memory. External source
commands have individual timeouts, but there is no overall deadline; socket
owner scans and OS calls are not time-bounded.

Use `sys metrics` for a noninteractive, JSON alternative to `top`:

```sh
aitools sys metrics --limit 10
```

The command samples system CPU over one second and returns memory totals and
processes sorted by CPU usage. `cpu.busy_percent` uses all logical CPUs as
100%; a process's `cpu_percent` uses one CPU as 100% and can exceed that.
Process CPU is a lifetime average on Linux and a decaying average on macOS,
not the system sampling interval. Read `process_cpu_basis` before comparing
process values with system CPU. Memory and RSS values are bytes, and
`memory.available_basis` identifies how available memory was calculated.

Process entries expose executable names, not arguments. If `--limit` cuts the
list, the result includes the total process count, `truncated: true`, and
partial status (exit code 3). Missing or malformed metric sources produce
`environment.unavailable`, not fabricated zero readings. The one-second
interval does not bound total command duration.

For individual sources, use `sys network --limit 100` and `sys disk PATH`.
Network values are cumulative interface byte, packet, and error counters,
not transfer rates or connectivity tests. Counters can reset. Disk values
describe the filesystem containing the existing path, not every mount,
device health, or disk I/O. Available bytes mean capacity available to
unprivileged users; total minus available is not necessarily used space.
Unsupported, denied, failed, or malformed sources return
`environment.unavailable` rather than zero readings.

See [State inspection](state-inspection.md) for workspace and execution
state, which the host snapshot does not include.

## Run an external program

Use a built-in command when you need structured selections, guarded edits,
or undo. Use `run` when you need an installed program's own behavior:

```sh
aitools run -- awk 'END { print NR }' README.md
aitools run -- sed -n '1,10p' README.md
aitools run -- grep -n 'aitools' README.md
```

Arguments after `--` reach the child unchanged. Shell operators, wildcards,
and variable references are not expanded by aitools. To use shell syntax,
explicitly select a shell and quote the whole program:

```sh
aitools run -- sh -c 'grep "aitools" README.md | sort -u'
```

Only do this with a shell program you trust; do not interpolate untrusted
text into it. Prefer direct arguments for variable input.

`run` captures non-interactive output and does not allocate a terminal or PTY.

By default, `run` closes stdin and inherits the caller's directory and environment.
Choose one input source: `--stdin` forwards piped input, `--stdin-data TEXT`
sends UTF-8 text without adding a newline, and `--stdin-file PATH` sends regular-file
bytes. FIFOs, devices, and directories are rejected; use `--stdin` for piped input.
Inline input can appear in shell history; use a file or pipe for sensitive data.
`--cwd PATH` changes the child's directory only. Relative input-file paths still
resolve from the caller's directory. Repeat `--env KEY=VALUE` to overlay inherited
variables; the last value wins, and empty values are allowed.

```sh
aitools run --stdin-data 'hello' -- sed 's/hello/world/'
aitools run --stdin-file README.md -- wc -l
aitools run --cwd docs --env LC_ALL=C -- find . -name '*.md'
```

These input, directory, and environment options apply to `run`, not `bg start`.

Check the child's `exit_code`, `signal`, and `timed_out` separately: a child
failure or timeout does not make aitools fail. For example, `grep` returns 1
for no matches while aitools returns 0. If `--grep` results exceed
`--grep-limit`, aitools returns 3 with `status:"partial"`; the child has
already run, so do not blindly retry a command that can have side effects.
The default output keeps the first 50 and last 150 lines of each stream.
Head/tail trimming and capture caps do not change the aitools exit code.
Inspect `truncated` and `capture_capped` before treating output as complete.

External programs are not sandboxed by `--root`. They retain filesystem and
network access, and their writes bypass aitools guards, transactions, and
undo. Captured JSON output masks known secret formats; `--stdout-to` writes
raw, unmasked stdout to a new file and is not journaled.

## Shell commands and their aitools replacements

Choose by the operation, not just the executable name. For example,
`sed -n` or `perl -ne` used to print selected lines belongs with `read` or
`search`, not an editing command. Arbitrary sed, Perl, awk, and jq programs
can retain their exact behavior through `run` when no built-in operation
matches. `transform` changes a file in place; it is not a drop-in replacement
for a shell filter that only prints output.

An agent that calls a shell command name by habit gets `argument.invalid`
with `repairs` naming the aitools command to use instead:

<!-- BEGIN GENERATED: correspondence -->
| Shell command | aitools command | What it does |
|---|---|---|
| `cat`, `head`, `tail`, `nl` | `aitools read` | Read a file with line numbers and range control. |
| `grep`, `rg`, `egrep`, `fgrep` | `aitools search` | Search file contents by pattern. |
| `ls`, `fd`, `find`, `tree`, `du` | `aitools find` | List or find files. |
| `diff`, `cmp`, `comm` | `aitools diff` | Compare files. |
| `sed`, `perl` | `aitools edit` | Replace a single occurrence by exact old/new text. |
| `sed`, `perl` | `aitools replace` | Replace every occurrence of a pattern, with a required match count. |
| `awk`, `cut` | `aitools table read` | Read delimited or whitespace-separated tabular text. |
| `sort`, `uniq`, `tac`, `shuf` | `aitools transform` | Reorder, dedupe, reverse, or shuffle lines in place. |
| `tr`, `dos2unix`, `expand`, `unexpand`, `fold`, `fmt` | `aitools transform` | Apply a line-level text transform. |
| `iconv`, `nkf` | `aitools transcode` | Convert a file's text encoding. |
| `cp` | `aitools copy` | Copy a file, or a directory with --recursive. |
| `mv` | `aitools move` | Move or rename a file or directory. |
| `rm` | `aitools rm` | Remove one target with journal and undo; trees need --recursive and --expect-count. |
| `rmdir` | `aitools delete` | Delete a file, a symlink, or an empty directory. |
| `mkdir` | `aitools mkdir` | Create a directory and its parents. |
| `chmod` | `aitools chmod` | Set executable bits or an octal mode. |
| `ln` | `aitools link` | Create a symbolic link inside the workspace. |
| `touch` | `aitools touch` | Create an empty file or set a file's modification time. |
| `mktemp` | `aitools mktemp` | Create a temporary file or directory in the workspace's state tmp/ area. |
| `jq` | `aitools json get` | Read, query, or edit JSON. |
| `tar`, `unzip`, `zcat`, `gzip`, `zip` | `aitools archive list` | List, read, extract, or create an archive. |
| `git` | `aitools git status` | Read-only git status, log, diff, blame, or show. |
| `uname`, `whoami`, `hostname`, `nproc` | `aitools sys info` | Read system information. |
| `env` | `aitools sys env` | Read environment variables, with secrets masked. |
| `which` | `aitools sys tools` | Check whether an external tool is available. |
| `ps` | `aitools sys procs` | List processes (read-only). |
| `top` | `aitools sys metrics` | Read one noninteractive system metrics snapshot. |
| `lsof` | `aitools sys ports` | List listening TCP sockets and their owning processes. |
| `df` | `aitools sys disk` | Read capacity of the filesystem containing a path, not every mount. |
| `netstat` | `aitools sys network` | Read cumulative interface counters, not rates or connectivity. |
| `netstat` | `aitools sys ports` | List listening TCP sockets, not every connection or route. |
| `date` | `aitools time now` | Read or convert the current or a given time. |
| `base64`, `xxd` | `aitools util encode` | Encode or decode bytes. |
| `bc`, `expr` | `aitools util calc` | Evaluate an arithmetic expression. |
| `uuidgen` | `aitools util uuid` | Generate a UUID. |
| `uuid` | `aitools util uuid` | `uuid` is a group subcommand name; the group prefix is required. |
| `openssl` | `aitools util random` | Generate cryptographically random bytes. |
<!-- END GENERATED: correspondence -->

A shell command with several rows (`sed`, `perl`) returns every row's repair,
in table order. A name the table does not list returns the repair
`aitools schema`. Names that are also aitools commands or groups (`find`,
`diff`, `mkdir`, `chmod`, `mktemp`, `touch`, `git`) run the aitools command
and never reach this lookup; their rows only document the mapping.
