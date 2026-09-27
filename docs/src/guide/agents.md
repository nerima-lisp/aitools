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
| 3 | The result on standard output is a valid prefix (`status:"partial"`). Run a command from `next_commands` to continue, or narrow the request. |
| 2 | The file is not in the state you assumed. Read it again, then retry with a fresh selection, hash, or count. |
| 1 | Read `error.code` and run a command from `error.repairs`. |

Standard output carries success; standard error carries the error envelope.
Each is exactly one JSON object.

## Paths

A relative path argument resolves against the current working directory,
as in a shell. `--root` selects the workspace (the write boundary, the state
directory, and the ignore rules) and does not change what a relative path
means. Write output gives paths relative to the workspace root. `read`,
`info`, `check`, and `diff` give a file inside the root relative to it and a
file outside the root as its absolute real path.

Reads are not restricted to the workspace. Writes outside it, through a
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
{"schema_version":1,"status":"partial","command":"read","mode":"text","path":"nums.txt","start_line":1,"lines":["1","2","3"],"total_lines":100,"hash":"93d4e5c7...","truncated":true,"encoding_errors":0,"approx_tokens":2,"next_commands":["aitools read nums.txt --range 4:6"]}
```

`approx_tokens` estimates the size of the returned text as
`ceil(characters / 4)` (a multi-byte UTF-8 character counts once),
independent of any model's tokenizer. Use it to decide whether to read more
or to narrow the range.

## Write with guards

A write states what it expects the file to look like, and fails without
writing when the file differs.

1. Select by content where you can. `edit --old` must match exactly one
   place; zero or several matches fail with exit code 2 and `candidates`.
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
{"schema_version":1,"status":"ok","command":"edit","changes":[{"path":"hello.lisp","action":"modified","hash_before":"de2187a5...","hash_after":"5f132d76...","diff":"@@ -1,2 +1,2 @@\n (defun greet (name)\n-  (format t \"Hello, ~a\" name))\n+  (format t \"Hi, ~a\" name))\n"}],"op_id":"op-20260926T011541Z-4a125e73","strategy":"exact"}
```

Hashes above are shortened for the page; real output carries the full
SHA-256 hex digest.

Never write text copied from output that contains `[REDACTED_SECRET]`.
aitools masks known secret formats in every output, and a write whose input
contains the mask text fails with `refusal.redacted-input`. Read the value
from its source file instead of copying it through output.

## Group related writes in a transaction

When several writes must land together, or you want to check the combined
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

## Shell commands and their aitools replacements

An agent that calls a shell command name by habit gets `argument.invalid`
with `repairs` naming the aitools command to use instead. The repairs come
from `aitools.data:*correspondence-table*`
(`data/domain/protocol/correspondence-table-data.lisp`), and the table below
is generated from the same data:

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
| `cp`, `mv`, `rm`, `rmdir` | `aitools copy` | Copy, move, or delete a file or empty directory. |
| `mkdir`, `chmod`, `ln`, `touch`, `mktemp` | `aitools mkdir` | Create a directory, change mode, link, touch, or make a temp path. |
| `jq` | `aitools json get` | Read, query, or edit JSON. |
| `tar`, `unzip`, `zcat`, `gzip`, `zip` | `aitools archive list` | List, read, extract, or create an archive. |
| `git` | `aitools git status` | Read-only git status, log, diff, blame, or show. |
| `uname`, `whoami`, `hostname`, `nproc` | `aitools sys info` | Read system information. |
| `env` | `aitools sys env` | Read environment variables, with secrets masked. |
| `which` | `aitools sys tools` | Check whether an external tool is available. |
| `ps`, `lsof` | `aitools sys procs` | List processes or listening ports. |
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

The data covers command names only. The mapping of individual shell idioms
(`sed -n '/A/,/B/p'`, `sort | uniq -c`) to specific aitools options is not in
this data; the end-to-end tests under `t/e2e/` hold it, running the built
binary for each idiom and comparing the result with the output of the shell
command it replaces. The table above is regenerated by
`docs/tools/generate-reference.lisp`; see
[Regenerating the reference](../project/development.md#regenerating-the-reference).
