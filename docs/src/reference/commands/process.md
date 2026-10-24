# The process context

[Command index](index.md#command-index).

### `run` {#run}

Run a program without a shell and report its exit, timing, and trimmed output.

Runs argv directly (no shell), in its own process group. By default stdin is /dev/null and the child inherits the current directory and environment. Choose one stdin source, use --cwd for the child's directory, and repeat --env to overlay environment entries. External programs are not sandboxed, and their writes are not journaled or undoable by aitools. Exit code is 0 when the program started, including on timeout, except that exceeding --grep-limit returns partial (exit 3); the child's own status is exit_code/signal. ANSI escapes are removed and \r-redrawn progress lines keep only their last state unless --no-strip-ansi. Known secret formats in the output are masked before --grep sees it.

| Argument | Type | Default | Description |
|---|---|---|---|
| `argv` (positional, required) | string |  | Program and arguments, after --. |
| `--stdin` | flag |  | Forward aitools standard input to the child; mutually exclusive with --stdin-data and --stdin-file. |
| `--stdin-data` | string |  | Send this text as UTF-8 stdin without adding a newline. Mutually exclusive with --stdin and --stdin-file; shell history can retain this argument. |
| `--stdin-file` | string |  | Send a regular file's bytes as stdin; FIFOs, devices, and directories are rejected. Relative paths use aitools' current directory, not --cwd. Mutually exclusive with --stdin and --stdin-data. |
| `--cwd` | string |  | Working directory for the child only; --root remains the aitools workspace boundary, not a process sandbox. |
| `--env` | string, repeatable |  | KEY=VALUE to overlay on the inherited environment; last value wins. Empty values are allowed. |
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

### `signal` {#signal}

Signal same-UID processes selected by PID or command-line pattern.

PID targets require an expected command-line substring or start identity. Pattern targets require an exact expected count. All selected targets are checked before sending; identity and safety are rechecked immediately before each signal. A process can still change between check and kill(2).

| Argument | Type | Default | Description |
|---|---|---|---|
| `--pid` | integer |  | Target PID (at least 2). |
| `--pattern` | regex |  | Match the full command line. |
| `--expect-command` | string |  | Required command-line substring with --pid. |
| `--expect-start` | string |  | Exact host start identity with --pid: seconds.microseconds on Darwin, start ticks on Linux. |
| `--expect-count` | integer |  | Exact number of pattern matches. |
| `--signal` | string | `TERM` | TERM, KILL, HUP, INT, USR1, USR2, or QUIT. |
| `--grace` | duration |  | With TERM, wait then send KILL if still alive. |

| Output field | Description |
|---|---|
| `items` | [{pid,signal}] for processes signalled. |
| `total` | Number of selected processes. |

Errors: `argument.invalid`, `input.syntax-error`, `selection.count-mismatch`, `refusal.target-changed`, `environment.io`.

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

A listing with invalid or disappeared records is partial (exit 3); omitted identifies those records without exposing their contents. A specific ID retains input.not-found or environment.io errors.

| Argument | Type | Default | Description |
|---|---|---|---|
| `id` (positional) | string |  | Only this bg ID. |

| Output field | Description |
|---|---|
| `items` | [{id,name,pid,argv,running,exit_code,signal,started}]; started is UTC RFC 3339. |
| `total` | Number of valid returned items. |
| `observed_total` | Number of record IDs observed before reading; equals total plus omitted count, not a count of host processes. |
| `omitted` | [{id,reason}] excluded from a listing; reason is invalid-record or record-unavailable. Empty when complete. |

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

