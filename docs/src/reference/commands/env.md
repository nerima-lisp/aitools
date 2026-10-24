# The env context

[Command index](index.md#command-index).

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

### `sys metrics` {#sys-metrics}

Sample CPU, memory, and process metrics once without an interactive display.

CPU uses a one-second interval across all logical CPUs (0..100%; Linux excludes iowait from busy). Linux reads /proc; Darwin samples Mach host_statistics CPU counters and runs sysctl and vm_stat; both use ps for processes. Each subprocess has a 60-second timeout. Process CPU uses a different time basis: lifetime average on Linux, decaying average over up to about one minute on Darwin; 100% represents one CPU and values may exceed 100%. Commands are executable names, not argument vectors. Any unavailable or malformed source is an error. More processes than --limit give partial, truncated true, exit 3.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--limit` | integer | `20` | Maximum process items (at least 1), sorted by CPU descending then PID ascending. |

| Output field | Description |
|---|---|
| `sample_interval_ms` | Requested CPU sampling interval, 1000 milliseconds; total command duration can be longer. |
| `cpu` | {busy_percent, logical_cpus, basis: interval_all_cpus}; interval-average busy percentage across all CPUs. |
| `memory` | {total_bytes, available_bytes, available_basis}; Linux MemAvailable or free+buffers+cached, Darwin free+inactive+speculative pages. |
| `process_cpu_basis` | lifetime_average (Linux) or decaying_average (Darwin); not the system CPU sampling interval. |
| `items` | [{pid, command, cpu_percent, rss_bytes}]; RSS in bytes, process CPU in percent of one logical CPU. |
| `total` | Process count before --limit. |
| `truncated` | Present and true when processes were cut at --limit. |

Errors: `environment.unavailable`, `internal.unexpected`.

### `sys network` {#sys-network}

Read cumulative network interface counters, ordered by name.

Linux reads /proc/net/dev; Darwin runs trusted absolute-path netstat -ibn and selects link rows only. These cumulative OS counters are not sampled rates, connectivity checks, routes, DNS, or a complete network inventory. Counters may reset. Missing, denied, failed, unsupported, or malformed sources fail with environment.unavailable. Darwin source timeout is 60 seconds. Output is bounded by --limit; overflow gives partial and exit 3.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--limit` | integer | `100` | Maximum interfaces, 1..1000. |

| Output field | Description |
|---|---|
| `source` | OS counter source. |
| `basis` | cumulative_interface_counters. |
| `units` | Bytes, packets, and error events. |
| `items` | [{name, rx_bytes, rx_packets, rx_errors, tx_bytes, tx_packets, tx_errors}]. |
| `total` | Interface count before limit. |
| `limit` | Requested output limit. |
| `truncated` | Whether items were omitted. |

Errors: `argument.invalid`, `environment.unavailable`, `internal.unexpected`.

### `sys disk` {#sys-disk}

Read capacity of the filesystem containing PATH (default: workspace root).

One statfs query, not a disk I/O sample or an inventory of all mounts or devices. Available bytes use blocks available to an unprivileged user, not all free blocks; do not infer used bytes as total minus available. Missing paths, permission failures, unsupported OS, and acquisition failures return environment.unavailable.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` | string |  | Existing path on the filesystem; default workspace root. |

| Output field | Description |
|---|---|
| `path` | Queried path. |
| `source` | statfs. |
| `scope` | filesystem_containing_path. |
| `units` | bytes. |
| `total_bytes` | Filesystem capacity in bytes. |
| `available_bytes` | Capacity available to unprivileged users in bytes. |
| `available_basis` | unprivileged_available_blocks. |
| `truncated` | Always false: one filesystem result. |

Errors: `environment.unavailable`, `internal.unexpected`.

### `sys ports` {#sys-ports}

List TCP sockets in LISTEN state with the owning process.

Linux reads /proc/net/tcp and tcp6 and maps socket inodes through /proc/<pid>/fd (other users' processes stay null without privileges); Darwin runs lsof. One missing Linux family source returns partial (exit 3), even with no observed listeners; both missing returns environment.unavailable. Missing does not distinguish disabled support from permission or read failure. The wildcard address is 0.0.0.0 or ::.

| Output field | Description |
|---|---|
| `items` | [{port, address, protocol, pid, command}] by port. |
| `total` | Number of observed items; not a complete host count when partial. |
| `coverage` | Linux only: {ipv4, ipv6}, each ok or source_unavailable; socket table availability, not owner visibility. |
| `source_unavailable` | Missing Linux socket-table paths, present only when partial. |

Errors: `environment.unavailable`, `internal.unexpected`.

### `sys snapshot` {#sys-snapshot}

Collect host info, metrics, network, disk, and listening TCP ports in one non-atomic response.

Sequential host observations, not all system state. Each section retains data and a status (ok, partial, unavailable) with a reason. Missing required info values, unknown socket owners, omitted items, or unavailable sections yield partial (exit 3); all unavailable fails (exit 1). Shell is optional. Existing source commands retain their 60-second per-command timeout; there is no overall deadline or memory bound. Output limits do not bound acquisition: socket owner scans and OS calls have no timeout, and subprocess output is fully captured. Disk covers PATH only, not all mounts or I/O; network counters are cumulative, not connectivity; ports cover TCP LISTEN only. Metrics takes a one-second CPU sample. Unlike batch, this fixed read-only composition continues after unavailable sections.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` | string |  | Disk filesystem path; default workspace root. Info still describes the workspace filesystem. |
| `--process-limit` | integer | `20` | Maximum process items, 1..1000. |
| `--network-limit` | integer | `100` | Maximum interface items, 1..1000. |
| `--port-limit` | integer | `100` | Maximum listening socket items, 1..1000. |

| Output field | Description |
|---|---|
| `scope` | host, not exhaustive system state. |
| `atomic` | Always false. |
| `started_at_epoch_ms` | Wall-clock Unix epoch milliseconds; not a monotonic duration. |
| `finished_at_epoch_ms` | Wall-clock Unix epoch milliseconds. |
| `sections` | Named info/metrics/network/disk/ports objects with status, data, reason, and epoch-ms observation times. |
| `limits` | Requested output item limits. |
| `source_timeout_seconds` | Per external source command timeout, not overall timeout. |
| `collection_bounds` | Acquisition and memory limitations. |

Errors: `argument.invalid`, `environment.unavailable`, `internal.unexpected`.

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

