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
    (:name "sys.ports"
     :summary "List TCP sockets in LISTEN state with the owning process."
     :description "Linux reads /proc/net/tcp and tcp6 and maps socket inodes through /proc/<pid>/fd (other users' processes stay null without privileges); Darwin runs lsof. The wildcard address is 0.0.0.0 or ::."
     :output-fields ((:name "items" :description "[{port, address, protocol, pid, command}] by port.")
                     (:name "total" :description "Number of items."))
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
