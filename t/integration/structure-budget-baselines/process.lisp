(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      '(:context "process"
        :complex-functions
        (
          ("AITOOLS.PROCESS.APPLICATION" "SIGNAL-COMMAND/K" 5 "packages/feature/process/src/application/signal-flow.lisp:42"
               (("AITOOLS.PROCESS.APPLICATION" "packages/feature/process/src/application/signal-flow.lisp" "SIGNAL-COMMAND/K")
                ("AITOOLS.PROCESS.APPLICATION" "packages/feature/process/src/application/signal-flow.lisp" "%SIGNAL-TARGET-SIGNAL")
                ("AITOOLS.PROCESS.APPLICATION" "packages/feature/process/src/application/signal-flow.lisp" "%SIGNAL-TARGET-RESULT")))
          ("AITOOLS.PROCESS.INFRASTRUCTURE" "%DARWIN-ARGV" 6 "packages/feature/process/src/infrastructure/signal-host.lisp:27"
               (("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host.lisp" "%DARWIN-ARGV")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%DARWIN-PARSE-ARGV")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%DARWIN-ARGV-FIELDS")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%DARWIN-ARGV-LAYOUT")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%DARWIN-SKIP-ARGV-PADDING")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%DARWIN-QUERY-ARGV-BYTES")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%DARWIN-QUERY-ARGV-SIZE")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%JOIN-ARGV")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%NUL-FIELD")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%LE")))
          ("AITOOLS.PROCESS.INFRASTRUCTURE" "%HOST-PROCESS-INFO" 5 "packages/feature/process/src/infrastructure/signal-host.lisp:127"
               (("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host.lisp" "%HOST-PROCESS-INFO")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host.lisp" "%LINUX-PROCESS-INFO")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host.lisp" "%LINUX-FILE-BYTES")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host.lisp" "%LINUX-TEXT")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%LINUX-PROCESS-IDENTITY")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%LINUX-COMMAND-LINE")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%LINUX-STAT-FIELDS")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%LINUX-STATUS-UIDS")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%DARWIN-PROCESS-IDENTITY")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%DARWIN-COMMAND-LINE")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%JOIN-ARGV")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%NUL-FIELD")
                ("AITOOLS.PROCESS.INFRASTRUCTURE" "packages/feature/process/src/infrastructure/signal-host-helpers.lisp" "%LE")))
        )
        :generated-exemptions
        nil
))
