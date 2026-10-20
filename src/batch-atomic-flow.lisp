(in-package #:aitools/cli)

(aitools.kernel.domain:define-flow
    (%atomic-commit-failed abort-tx tx committed results globals &key on-error)
  "ABORT-TX determines whether a failed commit was aborted cleanly or recovery must complete it."
  (declare (type function abort-tx on-error))
  (multiple-value-bind (abort-code aborted) (funcall abort-tx)
    (if (zerop abort-code)
        (aitools.kernel.domain:flow-call
            (%fail-with-envelope
             committed
             (format nil "tx ~A could not commit, so it was aborted and nothing was written: ~A"
                     tx (%member committed "error" "message"))
             results
             :repairs
             (list
              (repair
               "retry"
               "Re-read the conflicting paths, then run the batch again."
               (%batch-command-line globals "--stdin" "--atomic"))))
          (:on-error (code message &key candidates diagnostics conflicts repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates
             :diagnostics diagnostics
             :conflicts conflicts
             :repairs repairs)))
        (aitools.kernel.domain:flow-call
            (%fail-with-envelope
             committed
             (format nil "~{~A~}"
                     (list "tx " tx
                           " could not commit and could not be aborted ("
                           (%member aborted "error" "message")
                           "); some files may already be written and recovery will complete "
                           "the committed op on the next command: "
                           (%member committed "error" "message")))
             results
             :repairs
             (list
              (repair
               "inspect-state"
               "Inspect the transaction and journal before retrying."
               (format nil "aitools~{ ~A~} tx status ~A" globals tx))))
          (:on-error (code message &key candidates diagnostics conflicts repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates
             :diagnostics diagnostics
             :conflicts conflicts
             :repairs repairs))))))

(aitools.kernel.domain:define-flow
    (%batch-atomic app registry argvs globals &key on-ok on-error)
  (declare (type function on-ok on-error))
  (multiple-value-bind (code begun)
      (%dispatch-captured app registry
                          (append globals (list "tx" "begin" "--name" "batch")))
    (if (/= code 0)
        (aitools.kernel.domain:flow-call
            (%fail-with-envelope
             begun
             (format nil "batch --atomic could not begin its tx: ~A"
                     (%member begun "error" "message"))
             nil)
          (:on-error (code message &key candidates diagnostics conflicts repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates
             :diagnostics diagnostics
             :conflicts conflicts
             :repairs repairs)))
        (let ((tx (%member begun "tx")))
          (flet ((abort-tx ()
                   (%dispatch-captured app registry
                                       (append globals (list "tx" "abort" tx)))))
            (multiple-value-bind (results failure)
                (%run-elements app registry argvs globals tx nil)
              (if failure
                  (multiple-value-bind (abort-code aborted) (abort-tx)
                    (if (zerop abort-code)
                        (aitools.kernel.domain:flow-call
                            (%fail-with-envelope
                             (third failure)
                             (format nil
                                     "~A; tx ~A was aborted and nothing was written"
                                     (%element-failure-message failure (length argvs))
                                     tx)
                             results)
                          (:on-error (code message &key candidates diagnostics conflicts repairs)
                            (aitools.kernel.domain:flow-continue
                             :on-error code message
                             :candidates candidates
                             :diagnostics diagnostics
                             :conflicts conflicts
                             :repairs repairs)))
                        (aitools.kernel.domain:flow-call
                            (%fail-with-envelope
                             (third failure)
                             (format nil
                                     "~A; tx ~A could not be aborted: ~A; staged changes may remain in the transaction"
                                     (%element-failure-message failure (length argvs))
                                     tx
                                     (%member aborted "error" "message"))
                             results
                             :repairs
                             (append
                              (%repair-plists (third failure))
                              (list
                               (repair
                                "inspect-state"
                                "Inspect the transaction and journal before retrying."
                                (format nil "aitools~{ ~A~} tx status ~A"
                                        globals tx)))))
                          (:on-error (code message &key candidates diagnostics conflicts repairs)
                            (aitools.kernel.domain:flow-continue
                             :on-error code message
                             :candidates candidates
                             :diagnostics diagnostics
                             :conflicts conflicts
                             :repairs repairs)))))
                  (multiple-value-bind (code committed)
                      (%dispatch-captured app registry
                                          (append globals (list "tx" "commit" tx)))
                    (if (/= code 0)
                        (aitools.kernel.domain:flow-call
                            (%atomic-commit-failed
                             (function abort-tx) tx committed results globals)
                          (:on-error (code message &key candidates diagnostics conflicts repairs)
                            (aitools.kernel.domain:flow-continue
                             :on-error code message
                             :candidates candidates
                             :diagnostics diagnostics
                             :conflicts conflicts
                             :repairs repairs)))
                        (aitools.kernel.domain:flow-continue
                         :on-ok
                         (append
                          (list (cons "results" results) (cons "tx" tx))
                          (loop for key in (list "changes" "op_id" "next_commands")
                                for value = (%member committed key)
                                when value collect (cons key value)))))))))))))
