# Migrating session commands

Historical Codex and Claude Code command names identify migration candidates,
not interchangeable invocations. Check the arguments, input, output, exit
status, and side effects before replacing a command. Use `aitools schema`
with command words to inspect the current contract.

This catalog was derived from locally recorded Codex and Claude Code tool
calls using command-position extractors. Raw session logs and the local
audit results are not distributed with the release; the catalog is migration
guidance, not reproducible evidence for each historical invocation.

## Native candidates

Uses outside the stated subset need external execution.

| Detected names | Candidate and limits |
|---|---|
| `cat head nl tail strings xxd od` | `read`: bounded line windows, strings, or hex, with JSON output and masking; not a raw byte stream. |
| `rg grep egrep` | `search`: check regex, ignore, context, limits, and exit semantics. |
| `find fd ls tree du` | `find`: listings, filters, and hierarchy; byte totals are not allocated disk blocks. |
| `stat file` | `info`: supported metadata and MIME inference, not all platform fields. |
| `sed awk cut perl` | `read`, `search`, `table read`, `replace`, `transform`: line or column selection and supported transformations, not arbitrary programs. |
| `sort uniq tr fmt fold iconv` | `transform`, `transcode`: supported operations and encodings. File updates are not stdin filters. |
| `jq` | `json get`, `json select`, `json set`, `json delete`, `json merge`, `json patch`, `json fmt`: JSON Pointer and limited filters, not the jq language. |
| `diff cmp comm` | `diff`: supported text and line-set comparisons; check ordering, duplicate handling, binary input, and exit codes. |
| `cp mv` | `copy`, `move`: workspace-scoped; check symlink treatment and existing destinations. Move does not merge existing directories. |
| `rm rmdir` | `rm`, `delete`: journaled, guarded deletion. Recursive execution requires `--expect-count`, including the target root, and enforces protected paths and size limits. Inspect `--recursive --dry-run` first to obtain the count without deletion. Undo refuses an inaccessible mode `000` tree rather than temporarily broadening permissions. |
| `mkdir chmod ln touch mktemp` | `mkdir`, `chmod`, `link`, `touch`, [`mktemp`](../reference/commands/edit.md#mktemp): parent creation, supported permission modes, symlinks rather than hardlinks, and temporary entries in the workspace's state `tmp/` area (not journaled). Each `mktemp` invocation attempts to remove entries whose modification time is more than seven days old, including directory contents. |
| `apply_patch patch` | `apply`: unified diff only, not Codex apply_patch syntax or every patch option. |
| `tar zip unzip gzip gunzip zcat` | `archive list`, `archive read`, `archive extract`, `archive create`: supported formats and operations, not arbitrary compression, streaming, or metadata preservation. |
| `base64 wc` | `util encode`, `util decode`, `util tokens`: different input/output contracts; line counting includes an unterminated final line, unlike `wc -l`. |
| `bc expr` | `util calc`: supported numeric expressions and functions, not variables, assignments, or string expressions. |
| `uuidgen` | `util uuid`: cryptographic UUID v4 or v7. |
| `openssl` | `util random`: cryptographic random strings in supported alphabets; not TLS, key management, or digests. |
| `uname whoami nproc id hostname vm_stat` | `sys info`: supported OS, user, CPU, and memory fields, not groups or every vm_stat counter. |
| `env which` | `sys env`, `sys tools`: masking and PATH executable lookup; tools runs version probes and does not resolve shell aliases/functions. |
| `ps pgrep` | `sys procs`: case-insensitive command-line substring matching, PID order, and limits; not pgrep's full selection language. |
| `top` | `sys metrics`: noninteractive CPU/memory/process samples; read the reported CPU time bases and units. |
| `df` | `sys disk`: one filesystem containing a path, with total and nonprivileged available bytes; not all mounts or disk I/O. |
| `netstat` | `sys network`, `sys ports`: cumulative interface counters and TCP listeners; not routes, UDP, or established connections. |
| `lsof` | `sys ports`: TCP listeners and available owners, not all open files, UDP, or established connections. |
| `git` | `git status`, `git log`, `git diff`, `git blame`, `git show`: supported read-only queries, not Git writes. |
| `date` | `time now`, `time convert`, `time diff`. |
| `sleep` | [`wait --duration`](../reference/commands/process.md#wait): JSON output; `--duration` sets the requested delay, while `--timeout` limits the wait (default 60 seconds). Set a timeout longer than the duration for longer delays. |
| `kill` | `bg stop`: managed job IDs only, not arbitrary PIDs or signals. |

## External argv

The remaining detected executable names can be requested through `run` when
installed, executable under OS permissions, and allowed by the caller's
execution policy. This list is not an aitools allowlist:

```text
basename bash brew cargo cc clang code curl defaults deno dig
direnv dirname diskutil docker emacs fish gh gtimeout
home-manager jj just kubectl launchctl make man nc nice nix
nix-instantiate nix-shell nix-store nixfmt node nohup npm npx
nslookup open orca osascript paredit ping pip pip3
pkill plutil pnpm psql python python3 readlink realpath rsync ruby
rustc sbcl security sh shellcheck sips sqlite3 ssh sudo swift sysctl
tee terraform time timeout tmux treefmt tsc wget xargs yq zsh
```

[`run`](../reference/commands/process.md#run) passes arguments after `--` as argv
without interpreting shell syntax. It is neither a sandbox nor journaled
execution. The default stdin is `/dev/null`; explicit stdin,
child environment overrides, and child working directory have their own
options. These do not change the parent shell. Interactive authentication,
TTY requirements, GUI behavior, and external writes need separate checks.

Once the child starts, wrapper exit zero does not establish child success.
Inspect the child result fields `exit_code`, `signal`, and `timed_out`;
exceeding `run --grep-limit` can produce wrapper partial exit 3, distinct from
the child's exit status. Output limits, masking, and ANSI stripping also mean
the JSON result is not a byte-preserving pipeline stage.

## Shell-dependent operations

These detected names can depend on the calling shell's state:

```text
alias cd command disown echo eval exec exit export false printf pwd
return set source test true type unset wait
```

Some standalone uses have executable or native equivalents. Shell variables,
functions, aliases, job tables, and parent-shell changes do not. Pipelines,
redirects, globs, substitutions, loops, heredocs, and `&&/||` are syntax rather
than argv. Use an explicitly chosen, trusted shell script where necessary;
do not pass historical command strings to a shell without review. A child
shell cannot modify its caller's state. Executable names such as `time` and
`direnv` also have shell-dependent uses.

## Tool actions and extraction limits

File-tool operations such as Read, Write, Edit, Glob, and Grep have native
file-operation candidates, but tool-specific edit formats and state differ.
Agent delegation, approvals, conversation state, notebooks, browser actions,
Web/MCP connectors, and existing interactive-session input are not replaced
by launching a CLI command.

The audit extracts a fixed set of command names at command positions and some
literal shell or JavaScript commands; it is not a complete shell interpreter.
Read failures and parse failures can exclude inputs. Unresolved substitutions
and wrappers can remain unclassified. The JavaScript extractor matches quoted
`cmd` or `command` fields without parsing JavaScript or proving execution:
comments and unused objects can produce false positives, while variables and
template literals can be missed. The Claude extractor excludes an entire call
containing `<<`, including quoted text and here-strings. Duplicate tool-call
IDs are collapsed under the assumption that they identify the same call.

Remaining native gaps include
general open-file queries, disk I/O and device/mount inventories, DNS and
connectivity checks, arbitrary signal selection and jq/awk/sed/yq programs,
hardlinks, version-control writes, and service/container/database operations.
See [State inspection](state-inspection.md) for observation boundaries and
partial or unavailable results.
