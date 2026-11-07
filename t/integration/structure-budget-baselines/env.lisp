(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      '(:context "env"
        :complex-functions
        (
          ("AITOOLS.ENV.APPLICATION" "%LINUX-SOCKET-OWNERS" 5 "packages/feature/env/src/application/sys-flows.lisp:340"
               (("AITOOLS.ENV.APPLICATION" "packages/feature/env/src/application/sys-port-flows.lisp" "%LINUX-SOCKET-OWNERS")
                ("AITOOLS.ENV.APPLICATION" "packages/feature/env/src/application/sys-port-flows.lisp" "%LINUX-SOCKET-INODE")
                ("AITOOLS.ENV.APPLICATION" "packages/feature/env/src/application/sys-port-flows.lisp" "%RECORD-LINUX-SOCKET-OWNER")
                ("AITOOLS.ENV.APPLICATION" "packages/feature/env/src/application/sys-port-flows.lisp" "%LINUX-PID-SOCKET-OWNERS")))
          ("AITOOLS.ENV.APPLICATION" "RESOLVE-ZONE/K" 5 "packages/feature/env/src/application/time-flows.lisp:24"
               (("AITOOLS.ENV.APPLICATION" "packages/feature/env/src/application/time-zone-flows.lisp" "RESOLVE-ZONE/K")
                ("AITOOLS.ENV.APPLICATION" "packages/feature/env/src/application/time-zone-flows.lisp" "%PARSE-ZONE-OCTETS")
                ("AITOOLS.ENV.APPLICATION" "packages/feature/env/src/application/time-zone-flows.lisp" "%DECODE-ZONE-FILE")
                ("AITOOLS.ENV.APPLICATION" "packages/feature/env/src/application/time-zone-flows.lisp" "%ZONE-IN-DIRECTORY")
                ("AITOOLS.ENV.APPLICATION" "packages/feature/env/src/application/time-zone-flows.lisp" "%FIND-ZONE-INFO")))
        )
        :generated-exemptions
        nil
))
