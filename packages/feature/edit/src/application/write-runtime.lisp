(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
 (%run-resolved ports
                write-plan
                workspace-root
                hash-entries
                expect-count
                lock-timeout-ms
                dry-run
                tx
                command-line
                &key
                on-ok
                on-error)
 (let* ((host (edit-ports-workspace-host ports))
        (workspace-store
         (funcall (edit-ports-open-store ports)
                  (aitools.workspace.application:workspace-root-real workspace-root)))
        (temporary-root
         (let ((tmp
                (aitools.store.domain:tmp-directory
                 (aitools.store.application:store-state-directory workspace-store))))
           (or (aitools.workspace.application:resolve-real-path host tmp) tmp)))
        (targets (write-plan-targets write-plan))
        (command (write-plan-command write-plan))
        (mktemp-area-policy (find-command-mktemp-area-policy command))
        (refusal-policy
         (getf mktemp-area-policy
               (if tx :tx-refusal :non-tx-refusal))))
   (aitools.kernel.domain:flow-call
    (%resolve-targets host workspace-root targets temporary-root)
    (:on-resolved (resolved-paths area)
                  (if (and (eq area :temporary) refusal-policy)
                      (aitools.kernel.domain:flow-continue
                       :on-error
                       (getf refusal-policy :error-code)
                       (getf refusal-policy :error-message))
                    (let* ((store
                            (if (eq area :temporary)
                                (funcall (edit-ports-open-store ports)
                                         temporary-root)
                              workspace-store))
                           (paths
                            (if (eq area :temporary)
                                (mapcar
                                 (lambda (real)
                                   (aitools.kernel.domain:path-relative-to
                                    temporary-root
                                    real))
                                 resolved-paths)
                              resolved-paths))
                           (argv
                            (options-argv command
                                          (funcall
                                           (write-plan-record-positionals
                                            write-plan)
                                           paths)
                                          (%record-options write-plan paths)))
                           (extra '()))
                      (aitools.kernel.domain:flow-call
                       (%run-write-operation/k ports
                                               store
                                               write-plan
                                               workspace-root
                                               workspace-store
                                               hash-entries
                                               paths
                                               area
                                               temporary-root
                                               command-line
                                               expect-count
                                               lock-timeout-ms
                                               dry-run
                                               tx
                                               argv
                                               extra)
                       (:on-ok (result)
                               (aitools.kernel.domain:flow-continue :on-ok
                                                                    result))
                       (:on-error (code message &key candidates diagnostics conflicts repairs path)
           (aitools.kernel.domain:flow-continue :on-error
                                                code
                                                message
                                                :candidates
                                                candidates
                                                :diagnostics
                                                diagnostics
                                                :conflicts
                                                conflicts
                                                :repairs
                                                repairs
                                                :path
                                                path))
))))
    (:on-error (code message &key path)
               (if path
                   (aitools.kernel.domain:flow-continue :on-error
                                                        code
                                                        message
                                                        :path
                                                        path)
                 (aitools.kernel.domain:flow-continue :on-error code message))))))
