(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
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
                         extra
                         &key
                         on-ok
                         on-error)
 (labels ((rejected (code message &key candidates diagnostics conflicts repairs path)
            (if path
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
                                                     path)
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
                                                     repairs)))
          (busy ()
            (aitools.kernel.domain:flow-continue :on-error
                                                 "environment.busy"
                                                 (concatenate 'string
                                                              "the workspace lock "
                                                              "could not be acquired within "
                                                              "--lock-timeout")))
          (tx-not-found ()
            (aitools.kernel.domain:flow-continue :on-error
                                                 "input.not-found"
                                                 (format nil "no open tx ~A" tx)
                                                 :repairs
                                                 (%tx-not-found-repairs)))
          (finish (results &rest keys)
            (aitools.kernel.domain:flow-continue :on-ok
                                                 (%with-extra-fields
                                                  (apply #'aitools.store.domain:write-result-fields
                                                         results
                                                         keys)
                                                  extra))))
   (handler-case (cond
                   ((and tx dry-run)
                     (aitools.kernel.domain:flow-call
                      (%dry-run-in-tx/k store
                                        tx
                                        ports
                                        write-plan
                                        workspace-root
                                        workspace-store
                                        hash-entries
                                        paths
                                        area
                                        temporary-root
                                        command-line
                                        expect-count
                                        dry-run
                                        tx)
                      (:on-planned (results validation-extra)
                                   (setf extra validation-extra)
                                   (finish results :dry-run t))
                      (:on-rejected (code message &key candidates diagnostics conflicts repairs path)
                                    (rejected code
                                              message
                                              :candidates candidates
                                              :diagnostics diagnostics
                                              :conflicts conflicts
                                              :repairs repairs
                                              :path path))
                      (:on-not-found () (tx-not-found)))) (tx
                      (aitools.store.application:tx-stage/k store
                                                            tx
                                                            argv
                                                            (lambda (view commit reject)
                                                              (aitools.kernel.domain:flow-call
                                                               (%validate-write/k view
                                                                                  ports
                                                                                  write-plan
                                                                                  workspace-root
                                                                                  workspace-store
                                                                                  hash-entries
                                                                                  paths
                                                                                  area
                                                                                  temporary-root
                                                                                  command-line
                                                                                  expect-count
                                                                                  dry-run
                                                                                  tx)
                                                               (:on-committed (requests validation-extra)
                                                                              (setf extra validation-extra)
                                                                              (funcall commit requests))
                                                               (:on-rejected
                                                                (code message &key candidates diagnostics conflicts repairs path)
                                                                (funcall reject
                                                                         code
                                                                         message
                                                                         :candidates candidates
                                                                         :diagnostics diagnostics
                                                                         :conflicts conflicts
                                                                         :repairs (or repairs
                                                                                      (and path
                                                                                           (default-repairs code
                                                                                                            (write-plan-command write-plan)
                                                                                                            argv
                                                                                                            path)))))))
                                                            :replayable
                                                            (write-plan-replayable write-plan)
                                                            :lock-timeout-ms
                                                            lock-timeout-ms
                                                            :on-staged
                                                            (lambda (tx-op results)
                                                              (finish results :tx tx :tx-op tx-op))
                                                            :on-rejected
                                                            #'rejected
                                                            :on-not-found
                                                            #'tx-not-found
                                                            :on-busy
                                                            #'busy)) (t
                      (aitools.store.application:commit-changes/k store
                                                                  argv
                                                                  (lambda (commit reject)
                                                                    (aitools.kernel.domain:flow-call
                                                                     (%validate-write/k
                                                                      (aitools.store.application:disk-view
                                                                       store)
                                                                      ports
                                                                      write-plan
                                                                      workspace-root
                                                                      workspace-store
                                                                      hash-entries
                                                                      paths
                                                                      area
                                                                      temporary-root
                                                                      command-line
                                                                      expect-count
                                                                      dry-run
                                                                      tx)
                                                                     (:on-committed (requests validation-extra)
                                                                                    (setf extra validation-extra)
                                                                                    (funcall commit requests))
                                                                     (:on-rejected
                                                                      (code message &key candidates diagnostics conflicts repairs path)
                                                                      (funcall reject
                                                                               code
                                                                               message
                                                                               :candidates candidates
                                                                               :diagnostics diagnostics
                                                                               :conflicts conflicts
                                                                               :repairs (or repairs
                                                                                            (and path
                                                                                                 (default-repairs code
                                                                                                                  (write-plan-command write-plan)
                                                                                                                  argv
                                                                                                                  path)))))))
                                                                  :lock-timeout-ms
                                                                  lock-timeout-ms
                                                                  :dry-run
                                                                  dry-run
                                                                  :on-committed
                                                                  (lambda (op-id results)
                                                                    (if dry-run
                                                                        (finish results :dry-run t)
                                                                        (finish results :op-id op-id)))
                                                                  :on-rejected
                                                                  #'rejected
                                                                  :on-busy
                                                                  #'busy))) (aitools.store.application:store-io-error
                     (condition)
                     (aitools.kernel.domain:flow-continue :on-error
                                     "environment.io"
                                     (princ-to-string condition))
) (aitools.store.domain:store-format-error
                     (condition)
                     (aitools.kernel.domain:flow-continue :on-error
                                                          "environment.io"
                                                          (princ-to-string condition)
                                                          :repairs
                                                          (%damaged-state-repairs tx))))))
