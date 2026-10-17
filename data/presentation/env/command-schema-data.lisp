;;;; data/presentation/env/command-schema-data.lisp
;;;;
;;;; Schema text for the `sys` and `time` groups,
;;;; one entry per command keyed by its full dispatch name.
;;;; AITOOLS.ENV.PRESENTATION builds each COMMAND-SCHEMA from this table; the
;;;; cl-cli option definitions live beside the handlers.
(in-package #:aitools.data)

(defparameter *env-tz-arg*
  '(:name "--tz" :type "string"
    :description "IANA zone name read from the TZif database ($TZDIR, /usr/share/zoneinfo, /usr/lib/zoneinfo, /usr/share/lib/zoneinfo, /etc/zoneinfo). UTC needs no database. Default: $TZ, then the /etc/localtime link, then UTC."))

(defparameter *env-time-value-description*
  "ISO 8601 (2026-03-08, 2026-03-08T01:30, 2026-03-08 01:30:00.250+09:00, 20260308T013000Z) or Unix epoch: all digits, 12 or more integer digits = milliseconds, fewer = seconds (optional .fraction). `now` reads the clock. An ISO time without an offset is local time in the zone; a wall time repeated by a DST change resolves to the earlier instant, one skipped by it is read with the offset before the change.")

(defparameter *env-time-error-codes*
  '("argument.invalid" "input.syntax-error" "environment.unavailable" "internal.unexpected"))

(defparameter *env-command-schemas*
  `((:name "sys.info"
     :summary "Describe the host: OS, architecture, CPUs, user, memory, and the workspace's disk."
     :description "Linux reads /proc; Darwin runs sysctl and vm_stat. Memory available is MemAvailable on Linux and free+inactive+speculative pages on Darwin. Unknown values are null."
     :output-fields ((:name "os" :description "Kernel name, lowercase (linux, darwin).")
                     (:name "os_version" :description "Kernel release from uname(2).")
                     (:name "arch" :description "Machine from uname(2): x86_64, aarch64 (Linux), arm64 (Darwin).")
                     (:name "cpus" :description "Logical CPUs.")
                     (:name "user" :description "User name.")
                     (:name "uid" :description "Real user id.")
                     (:name "hostname" :description "Host name.")
                     (:name "shell" :description "$SHELL, or null.")
                     (:name "memory" :description "{total, available} in bytes.")
                     (:name "disk" :description "{total, available} in bytes for the file system of the workspace root."))
     :error-codes ("internal.unexpected"))
    (:name "sys.snapshot"
     :summary "Collect host info, metrics, network, disk, and listening TCP ports in one non-atomic response."
     :description "Sequential host observations, not all system state. Each section retains data and a status (ok, partial, unavailable) with a reason. Missing required info values, unknown socket owners, omitted items, or unavailable sections yield partial (exit 3); all unavailable fails (exit 1). Shell is optional. Existing source commands retain their 60-second per-command timeout; there is no overall deadline or memory bound. Output limits do not bound acquisition: socket owner scans and OS calls have no timeout, and subprocess output is fully captured. Disk covers PATH only, not all mounts or I/O; network counters are cumulative, not connectivity; ports cover TCP LISTEN only. Metrics takes a one-second CPU sample. Unlike batch, this fixed read-only composition continues after unavailable sections."
     :args ((:name "path" :type "string" :description "Disk filesystem path; default workspace root. Info still describes the workspace filesystem.")
            (:name "--process-limit" :type "integer" :default 20 :description "Maximum process items, 1..1000.")
            (:name "--network-limit" :type "integer" :default 100 :description "Maximum interface items, 1..1000.")
            (:name "--port-limit" :type "integer" :default 100 :description "Maximum listening socket items, 1..1000."))
     :output-fields ((:name "scope" :description "host, not exhaustive system state.")
                     (:name "atomic" :description "Always false.")
                     (:name "started_at_epoch_ms" :description "Wall-clock Unix epoch milliseconds; not a monotonic duration.")
                     (:name "finished_at_epoch_ms" :description "Wall-clock Unix epoch milliseconds.")
                     (:name "sections" :description "Named info/metrics/network/disk/ports objects with status, data, reason, and epoch-ms observation times.")
                     (:name "limits" :description "Requested output item limits.")
                     (:name "source_timeout_seconds" :description "Per external source command timeout, not overall timeout.")
                     (:name "collection_bounds" :description "Acquisition and memory limitations."))
     :error-codes ("argument.invalid" "environment.unavailable" "internal.unexpected"))
    (:name "sys.network"
     :summary "Read cumulative network interface counters, ordered by name."
     :description "Linux reads /proc/net/dev; Darwin runs trusted absolute-path netstat -ibn and selects link rows only. These cumulative OS counters are not sampled rates, connectivity checks, routes, DNS, or a complete network inventory. Counters may reset. Missing, denied, failed, unsupported, or malformed sources fail with environment.unavailable. Darwin source timeout is 60 seconds. Output is bounded by --limit; overflow gives partial and exit 3."
     :args ((:name "--limit" :type "integer" :default 100 :description "Maximum interfaces, 1..1000."))
     :output-fields ((:name "source" :description "OS counter source.")
                     (:name "basis" :description "cumulative_interface_counters.")
                     (:name "units" :description "Bytes, packets, and error events.")
                     (:name "items" :description "[{name, rx_bytes, rx_packets, rx_errors, tx_bytes, tx_packets, tx_errors}].")
                     (:name "total" :description "Interface count before limit.")
                     (:name "limit" :description "Requested output limit.")
                     (:name "truncated" :description "Whether items were omitted."))
     :error-codes ("argument.invalid" "environment.unavailable" "internal.unexpected"))
    (:name "sys.disk"
     :summary "Read capacity of the filesystem containing PATH (default: workspace root)."
     :description "One statfs query, not a disk I/O sample or an inventory of all mounts or devices. Available bytes use blocks available to an unprivileged user, not all free blocks; do not infer used bytes as total minus available. Missing paths, permission failures, unsupported OS, and acquisition failures return environment.unavailable."
     :args ((:name "path" :type "string" :description "Existing path on the filesystem; default workspace root."))
     :output-fields ((:name "path" :description "Queried path.")
                     (:name "source" :description "statfs.")
                     (:name "scope" :description "filesystem_containing_path.")
                     (:name "units" :description "bytes.")
                     (:name "total_bytes" :description "Filesystem capacity in bytes.")
                     (:name "available_bytes" :description "Capacity available to unprivileged users in bytes.")
                     (:name "available_basis" :description "unprivileged_available_blocks.")
                     (:name "truncated" :description "Always false: one filesystem result."))
     :error-codes ("environment.unavailable" "internal.unexpected"))
    (:name "sys.env"
     :summary "List environment variables, optionally those whose name starts with PREFIX."
     :description "A variable whose name contains a secret key word (password, secret, token, api_key, ...) as an underscore-separated word has its value replaced by [REDACTED_SECRET]; every other value still has known secret formats masked."
     :args ((:name "prefix" :type "string" :description "Case-sensitive name prefix."))
     :output-fields ((:name "items" :description "[{name, value}] sorted by name.")
                     (:name "total" :description "Number of items.")
                     (:name "redactions" :description "Values replaced because of a secret-looking name."))
     :error-codes ("internal.unexpected"))
    (:name "sys.tools"
     :summary "Find commands on PATH and report the first line of their version output."
     :description "Without names, probes the default list (git, nix, sbcl, node, npm, cargo, rustc, go, python3, make, gcc, clang, docker, rg, jq). Version is the first nonblank line of `--version` (`go version` for go), stdout first, then stderr; null when the command did not finish within --timeout."
     :args ((:name "names" :type "string[]" :description "Command names (no `/`).")
            (:name "--timeout" :type "duration" :default "5s" :description "Per-command limit."))
     :output-fields ((:name "items" :description "[{name, path, version}]; path is null when not found on PATH.")
                     (:name "total" :description "Number of items."))
     :error-codes ("argument.invalid" "internal.unexpected"))
    (:name "sys.procs"
     :summary "List processes whose command line contains PATTERN (case-insensitive), ordered by pid."
     :description "Linux reads /proc; Darwin runs ps. Only reads: no process is signalled. More matches than --limit give status partial, truncated true, exit code 3."
     :args ((:name "pattern" :type "string" :description "Case-insensitive substring of the command line.")
            (:name "--limit" :type "integer" :default 50 :description "Maximum items (at least 1)."))
     :output-fields ((:name "items" :description "[{pid, ppid, user, command, started}]; started is UTC ISO 8601.")
                     (:name "total" :description "Matching processes before --limit.")
                     (:name "truncated" :description "Present and true when items were cut at --limit."))
     :error-codes ("environment.unavailable" "internal.unexpected"))
    (:name "sys.metrics"
     :summary "Sample CPU, memory, and process metrics once without an interactive display."
     :description "CPU uses a one-second interval across all logical CPUs (0..100%; Linux excludes iowait from busy). Linux reads /proc; Darwin samples Mach host_statistics CPU counters and runs sysctl and vm_stat; both use ps for processes. Each subprocess has a 60-second timeout. Process CPU uses a different time basis: lifetime average on Linux, decaying average over up to about one minute on Darwin; 100% represents one CPU and values may exceed 100%. Commands are executable names, not argument vectors. Any unavailable or malformed source is an error. More processes than --limit give partial, truncated true, exit 3."
     :args ((:name "--limit" :type "integer" :default 20 :description "Maximum process items (at least 1), sorted by CPU descending then PID ascending."))
     :output-fields ((:name "sample_interval_ms" :description "Requested CPU sampling interval, 1000 milliseconds; total command duration can be longer.")
                     (:name "cpu" :description "{busy_percent, logical_cpus, basis: interval_all_cpus}; interval-average busy percentage across all CPUs.")
                     (:name "memory" :description "{total_bytes, available_bytes, available_basis}; Linux MemAvailable or free+buffers+cached, Darwin free+inactive+speculative pages.")
                     (:name "process_cpu_basis" :description "lifetime_average (Linux) or decaying_average (Darwin); not the system CPU sampling interval.")
                     (:name "items" :description "[{pid, command, cpu_percent, rss_bytes}]; RSS in bytes, process CPU in percent of one logical CPU.")
                     (:name "total" :description "Process count before --limit.")
                     (:name "truncated" :description "Present and true when processes were cut at --limit."))
     :error-codes ("environment.unavailable" "internal.unexpected"))
    (:name "sys.ports"
     :summary "List TCP sockets in LISTEN state with the owning process."
     :description "Linux reads /proc/net/tcp and tcp6 and maps socket inodes through /proc/<pid>/fd (other users' processes stay null without privileges); Darwin runs lsof. One missing Linux family source returns partial (exit 3), even with no observed listeners; both missing returns environment.unavailable. Missing does not distinguish disabled support from permission or read failure. The wildcard address is 0.0.0.0 or ::."
     :output-fields ((:name "items" :description "[{port, address, protocol, pid, command}] by port.")
                     (:name "total" :description "Number of observed items; not a complete host count when partial.")
                     (:name "coverage" :description "Linux only: {ipv4, ipv6}, each ok or source_unavailable; socket table availability, not owner visibility.")
                     (:name "source_unavailable" :description "Missing Linux socket-table paths, present only when partial."))
     :error-codes ("environment.unavailable" "internal.unexpected"))
    (:name "time.now"
     :summary "Show the current time in a zone, in UTC, and as epoch milliseconds."
     :args ,(list *env-tz-arg*)
     :output-fields ((:name "iso8601" :description "Local time in the zone with its offset.")
                     (:name "utc" :description "The same instant in UTC (Z).")
                     (:name "epoch_ms" :description "The same instant in Unix epoch milliseconds.")
                     (:name "timezone" :description "Zone name used.")
                     (:name "utc_offset" :description "Offset in effect, e.g. +09:00.")
                     (:name "abbreviation" :description "Zone abbreviation in effect, e.g. JST, EDT."))
     :error-codes ,*env-time-error-codes*)
    (:name "time.convert"
     :summary "Convert a time value to ISO 8601 or epoch, optionally shifted by durations."
     :description "--add/--sub move the instant (1d = 24 hours), not the wall clock. epoch_s rounds toward the past."
     :args ,(list (list :name "value" :type "time" :required t :description *env-time-value-description*)
                  '(:name "--to" :type "enum" :default "iso8601" :choices ("iso8601" "epoch_ms" "epoch_s")
                    :description "Output format.")
                  '(:name "--add" :type "duration[]" :description "<number>ms|s|m|h|d, repeatable.")
                  '(:name "--sub" :type "duration[]" :description "<number>ms|s|m|h|d, repeatable; the only way to go back in time.")
                  *env-tz-arg*)
     :output-fields ((:name "input" :description "VALUE as given.")
                     (:name "input_format" :description "Detected format: iso8601, epoch_s, epoch_ms, or now.")
                     (:name "to" :description "Output format.")
                     (:name "result" :description "The converted value: a string for iso8601, an integer otherwise.")
                     (:name "timezone" :description "Zone used for offset-less input and iso8601 output."))
     :error-codes ,*env-time-error-codes*)
    (:name "time.diff"
     :summary "Compute B minus A."
     :args ,(list (list :name "a" :type "time" :required t :description *env-time-value-description*)
                  (list :name "b" :type "time" :required t :description "Same formats as a."))
     :output-fields ((:name "diff_ms" :description "B minus A in milliseconds (negative when B is earlier).")
                     (:name "human" :description "The same as days, hours, minutes, seconds, ms with zero parts omitted, e.g. 1h23m."))
     :error-codes ,*env-time-error-codes*))
  "One plist per `sys`/`time` command: :NAME (full dispatch name), :SUMMARY,
optional :DESCRIPTION, :ARGS, :OUTPUT-FIELDS, :ERROR-CODES, in
COMMAND-SCHEMA terms.")

(export '(*env-tz-arg* *env-time-value-description* *env-time-error-codes* *env-command-schemas*))
