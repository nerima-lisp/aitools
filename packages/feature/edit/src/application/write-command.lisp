(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
 (run-write-command/k ports
                      write-plan
                      &key
                      root
                      lock-timeout
                      dry-run
                      tx
                      display-argv
                      on-ok
                      on-error)
 "Run WRITE-PLAN (a WRITE-PLAN) as one write command and call exactly one of
ON-OK (fields) with the write-output alist (`changes`, then `op_id`, or
`tx`/`tx_op` for --tx, or `dry_run`; the plan extra fields; then
`next_commands` when a diff was cut) or ON-ERROR (code message &key
candidates diagnostics conflicts repairs), repairs always present.

ROOT is the global --root, LOCK-TIMEOUT the global --lock-timeout text,
DRY-RUN and TX are the command --dry-run and --tx, DISPLAY-ARGV the words
the user typed after the program name (for repair commands; defaults to the
recorded argv). A --dry-run writes nothing, so it needs no --expect-count:
it reports the count it selected as `expect_count` instead."
 (let* ((command (write-plan-command write-plan))
        (host (edit-ports-workspace-host ports))
        (first-target (first (write-plan-targets write-plan)))
        (display-path (and first-target (write-target-path first-target)))
        (argv
         (or display-argv
             (let ((paths
                    (mapcar (function write-target-path)
                            (write-plan-targets write-plan))))
               (options-argv command
                             (funcall (write-plan-record-positionals write-plan)
                                      paths)
                             (%record-options write-plan paths)))))
        (command-line (command-line argv)))
   (aitools.kernel.domain:flow-call
    (%inspect-write-plan/k write-plan lock-timeout dry-run argv display-path)
    (:on-ok (lock-timeout-ms hash-entries expect-count)
            (aitools.workspace.application:call-with-resolved-root/k host
                                                                     :root
                                                                     root
                                                                     :on-error
                                                                     (lambda
                                                                         (reason
                                                                          path)
                                                                       (aitools.kernel.domain:flow-continue
                                                                        :on-error
                                                                        (if (eq
                                                                             reason
                                                                             :not-found)
                                                                            "input.not-found"
                                                                            "argument.invalid")
                                                                        (format
                                                                         nil
                                                                         (concatenate
                                                                          'string
                                                                          "workspace root ~A "
                                                                          "~:[is not a directory~;does not exist~]")
                                                                         path
                                                                         (eq
                                                                          reason
                                                                          :not-found))
                                                                        :repairs
                                                                        (list
                                                                         (repair
                                                                          "inspect-schema"
                                                                          "Pass an existing directory as --root."
                                                                          "aitools schema"))))
                                                                     :on-resolved
                                                                     (lambda
                                                                         (workspace-root)
                                                                       (aitools.kernel.domain:flow-call
                                                                        (%run-resolved
                                                                         ports
                                                                         write-plan
                                                                         workspace-root
                                                                         hash-entries
                                                                         expect-count
                                                                         lock-timeout-ms
                                                                         dry-run
                                                                         tx
                                                                         command-line)
                                                                        (:on-ok
                                                                         (fields)
                                                                         (aitools.kernel.domain:flow-continue
                                                                          :on-ok
                                                                          fields))
                                                                        (:on-error
                                                                         (code
                                                                          message
                                                                          &key
                                                                          candidates
                                                                          diagnostics
                                                                          conflicts
                                                                          repairs
                                                                          path)
                                                                         (aitools.kernel.domain:flow-continue
                                                                          :on-error
                                                                          code
                                                                          message
                                                                          :candidates
                                                                          candidates
                                                                          :diagnostics
                                                                          diagnostics
                                                                          :conflicts
                                                                          conflicts
                                                                          :repairs
                                                                          (or
                                                                           repairs
                                                                           (default-repairs
                                                                            code
                                                                            command
                                                                            argv
                                                                            (or
                                                                             path
                                                                             display-path)))))))))
    (:on-error (code message repairs)
               (aitools.kernel.domain:flow-continue :on-error
                                                    code
                                                    message
                                                    :repairs
                                                    (or repairs
                                                        (default-repairs code
                                                                         command
                                                                         argv
                                                                         display-path)))))))
