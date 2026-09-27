;;;; data/presentation/process/command-schema-data.lisp
;;;;
;;;; Schema text for `run`, `wait`, and the `bg` group, one entry per
;;;; command. AITOOLS.PROCESS.PRESENTATION builds each
;;;; COMMAND-SCHEMA from this table; the cl-cli option definitions live beside
;;;; the handlers.
(in-package #:aitools.data)

(defparameter *process-command-schemas*
  '((:name "run"
     :summary "Run a program without a shell and report its exit, timing, and trimmed output."
     :description "Runs argv directly (no shell), stdin at /dev/null, in its own process group. Exit code is 0 whenever the program started, including on timeout; the child's own status is exit_code/signal. ANSI escapes are removed and \\r-redrawn progress lines keep only their last state unless --no-strip-ansi. Known secret formats in the output are masked before --grep sees it."
     :args ((:name "argv" :kind "positional" :type "string" :required t :description "Program and arguments, after --.")
            (:name "--timeout" :type "duration" :default "120s" :description "SIGTERM to the process group when exceeded, SIGKILL 1s later; timed_out is then true.")
            (:name "--head" :type "integer" :default 50 :description "Leading lines kept per stream.")
            (:name "--tail" :type "integer" :default 150 :description "Trailing lines kept per stream.")
            (:name "--grep" :type "regex" :description "Report every matching line of the full output (before head/tail trimming) as matches[{n,text}].")
            (:name "--grep-limit" :type "integer" :default 50 :description "Matches reported per stream; more makes the result partial (exit 3).")
            (:name "--no-strip-ansi" :type "flag" :description "Keep ANSI escapes and \\r redraws.")
            (:name "--stdout-to" :type "string" :description "Write stdout, unmodified, to this new file (inside the workspace or the mktemp area; never overwrites; not journaled). stdout then reports {path,bytes}."))
     :output-fields ((:name "exit_code" :description "The child's exit code, or null when a signal ended it.")
                     (:name "signal" :description "The signal that ended the child, or null.")
                     (:name "timed_out" :description "True when --timeout ended the child.")
                     (:name "duration_ms" :description "Wall time from start to exit.")
                     (:name "stdout" :description "{head,tail,total_lines,truncated,matches?,total_matches?}; head and tail never overlap. With --stdout-to, {path,bytes}.")
                     (:name "stderr" :description "Same shape as stdout.")
                     (:name "redactions" :description "Secrets masked across both streams.")
                     (:name "capture_capped" :description "Present and true when a stream exceeded 64 Mi characters; later output was not captured."))
     :error-codes ("argument.invalid" "input.syntax-error" "refusal.outside-workspace" "refusal.exists"
                   "environment.unavailable" "environment.io"))
    (:name "wait"
     :summary "Block until one condition holds: a file line, an open port, a bg log line, a bg exit, or a duration."
     :args ((:name "--file" :type "string" :description "File to watch; with --pattern.")
            (:name "--pattern" :type "regex" :description "Line pattern for --file or --bg.")
            (:name "--port" :type "integer" :description "TCP port that must accept a connection on 127.0.0.1 or ::1.")
            (:name "--bg" :type "string" :description "bg ID; with --pattern (log line) or --exit.")
            (:name "--exit" :type "flag" :description "With --bg: wait for the process to end.")
            (:name "--duration" :type "duration" :description "Wait this long.")
            (:name "--timeout" :type "duration" :default "60s" :description "Give up after this long with environment.timeout."))
     :output-fields ((:name "matched" :description "Always true on success.")
                     (:name "elapsed_ms" :description "Time until the condition held.")
                     (:name "line" :description "The first matching line (pattern conditions).")
                     (:name "redactions" :description "Secrets masked in line (pattern conditions).")
                     (:name "port" :description "The port that accepted (--port).")
                     (:name "exit_code" :description "Exit code, or null (--bg --exit).")
                     (:name "signal" :description "Ending signal, or null (--bg --exit).")
                     (:name "duration_ms" :description "The duration waited (--duration)."))
     :error-codes ("argument.invalid" "input.syntax-error" "input.not-found" "environment.timeout"
                   "environment.unavailable" "environment.io"))
    (:name "bg.start"
     :summary "Start a program detached from aitools, logging stdout and stderr to the workspace state directory."
     :description "The process gets its own session and keeps running after aitools exits. A small sh supervisor records its exit status; argv reaches it only as \"$@\" and is never parsed by a shell."
     :args ((:name "argv" :kind "positional" :type "string" :required t :description "Program and arguments, after --.")
            (:name "--name" :type "string" :description "Label shown by bg status (1-64 characters)."))
     :output-fields ((:name "id" :description "bg ID (bg-<n>) for bg logs/status/stop and wait --bg.")
                     (:name "name" :description "The --name label, or null.")
                     (:name "pid" :description "PID of the supervisor, which leads the process's session and process group.")
                     (:name "log" :description "Absolute path of the combined stdout/stderr log."))
     :error-codes ("argument.invalid" "environment.unavailable" "environment.busy" "environment.io"))
    (:name "bg.logs"
     :summary "Read a bg process's log: the last lines, or the lines from a byte offset."
     :description "Without --from, the last --tail lines. With --from, up to --tail lines starting at that byte offset; next_offset continues exactly after the last returned line. The read position is kept by the caller only."
     :args ((:name "id" :kind "positional" :type "string" :required t :description "bg ID.")
            (:name "--tail" :type "integer" :default 100 :description "Lines returned.")
            (:name "--from" :type "integer" :description "Byte offset to read from, usually a previous next_offset.")
            (:name "--grep" :type "regex" :description "Only lines matching this pattern.")
            (:name "--no-strip-ansi" :type "flag" :description "Keep ANSI escapes and \\r redraws."))
     :output-fields ((:name "id" :description "bg ID.")
                     (:name "running" :description "Whether the process is still running.")
                     (:name "lines" :description "Log lines.")
                     (:name "truncated" :description "True when lines were left out (status partial, exit 3).")
                     (:name "next_offset" :description "Byte offset for the next --from.")
                     (:name "redactions" :description "Secrets masked in lines."))
     :error-codes ("argument.invalid" "input.syntax-error" "input.not-found" "environment.unavailable" "environment.io"))
    (:name "bg.status"
     :summary "List the bg processes aitools started in this workspace, or one of them."
     :args ((:name "id" :kind "positional" :type "string" :description "Only this bg ID."))
     :output-fields ((:name "items" :description "[{id,name,pid,argv,running,exit_code,signal,started}]; started is UTC RFC 3339.")
                     (:name "total" :description "Number of items."))
     :error-codes ("input.not-found" "environment.unavailable" "environment.io"))
    (:name "bg.stop"
     :summary "Stop a bg process: SIGTERM to its process group, SIGKILL if it outlives --grace."
     :args ((:name "id" :kind "positional" :type "string" :required t :description "bg ID.")
            (:name "--grace" :type "duration" :default "5s" :description "Time between SIGTERM and SIGKILL."))
     :output-fields ((:name "id" :description "bg ID.")
                     (:name "stopped" :description "False when the process had already ended.")
                     (:name "exit_code" :description "Exit code, or null.")
                     (:name "signal" :description "Ending signal (15 or 9 when stopped here), or null."))
     :error-codes ("argument.invalid" "input.not-found" "environment.unavailable" "environment.io")))
  "One plist per process-context command: :NAME, :SUMMARY, :DESCRIPTION,
:ARGS, :OUTPUT-FIELDS, :ERROR-CODES, in COMMAND-SCHEMA terms.")

(export '(*process-command-schemas*))
