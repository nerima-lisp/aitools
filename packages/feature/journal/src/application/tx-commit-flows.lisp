;;;; packages/feature/journal/src/application/tx-commit-flows.lisp
;;;;
;;;; `tx commit` and `tx abort` use the store's tx API for locking,
;;;; validation, and transaction state.
(in-package #:aitools.journal.application)

(aitools.kernel.domain:define-flow
    (tx-commit-flow ports context tx-id &key ignore-stale-reads
                    (max-diff-lines aitools.store.domain:+default-max-diff-lines+)
                    on-ok on-partial on-error)
  "Write the whole tx through the store commit as one op, or report the
write and read conflicts (exit 2) and write nothing."
  (declare (type function on-ok on-error) (ignore on-partial))
  (let ((flag (and ignore-stale-reads "--ignore-stale-reads")))
    (aitools.kernel.domain:flow-call
        (%call-with-store ports context "tx.commit")
      (:on-error (code message &key candidates diagnostics conflicts repairs)
        (aitools.kernel.domain:flow-continue
         :on-error code message
         :candidates candidates
         :diagnostics diagnostics
         :conflicts conflicts
         :repairs repairs))
      (:on-store (store root timeout)
        (declare (ignore root))
        (aitools.kernel.domain:flow-call
          (aitools.store.application:tx-commit/k
           store tx-id (remove nil (list "tx" "commit" tx-id flag))
           :ignore-stale-reads ignore-stale-reads
           :lock-timeout-ms timeout)
        (:on-committed (op-id results)
          (aitools.kernel.domain:flow-continue
           :on-ok (aitools.store.domain:write-result-fields
                   results :op-id op-id :max-diff-lines max-diff-lines)))
        (:on-rejected (code message &key conflicts &allow-other-keys)
          (if (string= code "refusal.target-changed")
              (aitools.kernel.domain:flow-continue
               :on-error code message
               :conflicts (%conflicts-json conflicts)
               :repairs (commit-conflict-repairs tx-id conflicts :globals (%globals context)))
              (aitools.kernel.domain:flow-call
                (%report-rejection "tx.commit" code message)
                (:on-error (error-code error-message &key repairs)
                  (aitools.kernel.domain:flow-continue
                   :on-error error-code error-message
                   :repairs repairs)))))
        (:on-not-found ()
          (aitools.kernel.domain:flow-call
            (%report-tx-not-found context tx-id)
            (:on-error (code message &key repairs)
              (aitools.kernel.domain:flow-continue
               :on-error code message
               :repairs repairs))))
        (:on-busy ()
          (aitools.kernel.domain:flow-call
            (%report-busy context (list "tx" "commit" tx-id flag))
            (:on-error (code message &key repairs)
              (aitools.kernel.domain:flow-continue
               :on-error code message
               :repairs repairs)))))))))

(aitools.kernel.domain:define-flow
    (tx-abort-flow ports context tx-id &key on-ok on-partial on-error)
  "Delete the tx without touching the working tree; `discarded`
lists its write-set paths."
  (declare (type function on-ok on-error) (ignore on-partial))
  (aitools.kernel.domain:flow-call
      (%call-with-store ports context "tx.abort")
    (:on-error (code message &key candidates diagnostics conflicts repairs)
      (aitools.kernel.domain:flow-continue
       :on-error code message
       :candidates candidates
       :diagnostics diagnostics
       :conflicts conflicts
       :repairs repairs))
    (:on-store (store root timeout)
      (declare (ignore root))
      (aitools.kernel.domain:flow-call
        (aitools.store.application:tx-abort/k
         store tx-id :lock-timeout-ms timeout)
        (:on-aborted (paths)
          (aitools.kernel.domain:flow-continue
           :on-ok (list (cons "tx" tx-id) (cons "discarded" paths))))
        (:on-not-found ()
          (aitools.kernel.domain:flow-call
            (%report-tx-not-found context tx-id)
            (:on-error (code message &key repairs)
              (aitools.kernel.domain:flow-continue
               :on-error code message
               :repairs repairs))))
        (:on-busy ()
          (aitools.kernel.domain:flow-call
            (%report-busy context (list "tx" "abort" tx-id))
            (:on-error (code message &key repairs)
              (aitools.kernel.domain:flow-continue
               :on-error code message
               :repairs repairs))))))))
