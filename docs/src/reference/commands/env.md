# The env context

[Command index](index.md#command-index).

### `sys info` {#sys-info}

Describe the host: OS, architecture, CPUs, user, uptime, memory, and the workspace's disk.

Linux reads /proc; Darwin reads uptime through the kern.boottime sysctl MIB and uses sysctl and vm_stat for CPU and memory. Unknown `cpus`, `memory.total`, and `memory.available` values are null; an unavailable uptime source returns environment.unavailable.

| Output field | Description |
|---|---|
| `os` | Kernel name, lowercase (linux, darwin). |
| `os_version` | Kernel release from uname(2). |
| `arch` | Machine from uname(2): x86_64, aarch64 (Linux), arm64 (Darwin). |
| `cpus` | Logical CPUs. |
| `user` | User name. |
| `uid` | Real user id. |
| `hostname` | Host name. |
| `uptime_seconds` | Elapsed seconds since boot from /proc/uptime on Linux or kern.boottime on Darwin. |
| `shell` | $SHELL, or null. |
| `memory` | {total, available} in bytes. |
| `disk` | {total, available} in bytes for the file system of the workspace root. |

Errors: `environment.unavailable`, `internal.unexpected`.

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

Sample CPU, memory, load average, and process metrics once without an interactive display.

CPU uses a one-second interval across all logical CPUs (0..100%; Linux excludes iowait from busy). Linux reads /proc; Darwin samples Mach CPU counters, reads vm.loadavg through a sysctl MIB, and runs sysctl and vm_stat for CPU count and memory; both use ps for processes. Each subprocess has a 60-second timeout by default; --timeout changes this limit. Process CPU uses a different time basis: lifetime average on Linux, decaying average over up to about one minute on Darwin; 100% represents one CPU and values may exceed 100%. Commands are executable names, not argument vectors. Any unavailable or malformed CPU, load-average, memory, or process source is an error. More processes than --limit give partial, truncated true, exit 3.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--limit` | integer | `20` | Maximum process items (at least 1), sorted by CPU descending then PID ascending. |
| `--timeout` | duration | `60s` | Per-subprocess source limit; default 60 seconds. |

| Output field | Description |
|---|---|
| `sample_interval_ms` | Requested CPU sampling interval, 1000 milliseconds; total command duration can be longer. |
| `cpu` | {busy_percent, logical_cpus, basis: interval_all_cpus}; interval-average busy percentage across all CPUs. |
| `load_average` | {one_minute, five_minutes, fifteen_minutes}; system load averages over 1, 5, and 15 minutes. |
| `memory` | {total_bytes, available_bytes, available_basis}; Linux MemAvailable or free+buffers+cached, Darwin free+inactive+speculative pages. |
| `process_cpu_basis` | lifetime_average (Linux) or decaying_average (Darwin); not the system CPU sampling interval. |
| `items` | [{pid, command, cpu_percent, rss_bytes}]; RSS in bytes, process CPU in percent of one logical CPU. |
| `total` | Process count before --limit. |
| `truncated` | Present and true when processes were cut at --limit. |

Errors: `argument.invalid`, `environment.unavailable`, `internal.unexpected`.

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

Snapshot sections are collected in sequence, so the response is not an atomic view of host state. Each section has a status, data, and reason. Partial sections include collected data; unavailable sections have null data. The info section is partial when any of os, os_version, arch, cpus, user, uid, hostname, memory.total, memory.available, disk.total, or disk.available is null. A truncated list, unknown socket owner, unavailable source, or uncollected section also makes the result partial (exit 3); all unavailable fails (exit 1). The 600-second default overall monotonic deadline covers every section, including socket-owner scans. Use --timeout to change it. When a deadline check stops collection, deadline_reached is true. Sections left uncollected by that check are unavailable with a structured reason object containing code environment.timeout and a message. A source failure recorded before that check keeps its own reason. External commands are limited to 60 seconds or the remaining budget, whichever is less; timed-out children are killed and reaped. Local operations are checked at their boundaries and are not interrupted while running. A bounded call already in progress may finish after the budget; the post-call check then stops collection. Directory and ancestor walks check between entries. Files over 16 MiB make their section unavailable with reason source_file_unreadable. Process, network, and port lists have a truncated flag when a configured limit omits items. Captured subprocess output has no memory bound. Disk reports capacity for the filesystem containing the selected path, not all mounts or I/O. Network counters are cumulative, not connectivity; ports cover TCP LISTEN only. Metrics takes a one-second CPU sample. Collection continues after a section becomes unavailable.

| Argument | Type | Default | Description |
|---|---|---|---|
| `path` | string |  | Disk filesystem path; default workspace root. Info still describes the workspace filesystem. |
| `--process-limit` | integer | `20` | Maximum process items, 1..1000. |
| `--network-limit` | integer | `100` | Maximum interface items, 1..1000. |
| `--port-limit` | integer | `100` | Maximum listening socket items, 1..1000. |
| `--timeout` | duration | `600s` | Maximum overall monotonic collection duration; default 600 seconds. |

| Output field | Description |
|---|---|
| `scope` | host, not exhaustive system state. |
| `atomic` | Always false. |
| `started_at_epoch_ms` | Wall-clock Unix epoch milliseconds; not a monotonic duration. |
| `finished_at_epoch_ms` | Wall-clock Unix epoch milliseconds. |
| `sections` | Named info/metrics/network/disk/ports objects with status, data, reason, and epoch-ms observation times. Partial sections retain collected data; unavailable sections have null data. Reason is null when no reason applies, a string for simple reasons, or an object for structured details. Each data object uses the fields documented for its standalone sys command. |
| `limits` | Requested output item limits. |
| `source_timeout_seconds` | Per-external-command timeout in seconds (60). |
| `overall_timeout_ms` | Configured overall monotonic budget in milliseconds; default 600000. |
| `deadline_reached` | True when collection stopped at the overall deadline. |
| `collection_bounds` | Summary of collection timing and captured-output limits. |

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

