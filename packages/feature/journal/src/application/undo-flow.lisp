;;;;
;;;; `undo <op_id>` over the store's UNDO-OP/K: the op id is
;;;; required so that one agent never undoes another's op by accident, and a
;;;; path changed since the op makes the whole undo a conflict (exit 2).
(in-package #:aitools.journal.application)

(aitools.kernel.domain:define-flow
    (undo-flow ports context op-id &key dry-run
               (max-diff-lines aitools.store.domain:+default-max-diff-lines+)
               on-ok on-partial on-error)
  (declare (type function on-ok on-error) (ignore on-partial))
  (cond
    ((or (null op-id) (zerop (length op-id)))
     (aitools.kernel.domain:flow-continue
      :on-error "argument.invalid"
      "undo needs the op_id of the operation to undo; it never picks one itself"
      :repairs (list (%history-repair context))))
    ((not (aitools.store.domain:valid-op-id-p op-id))
     (aitools.kernel.domain:flow-continue
      :on-error "input.not-found" (format nil "~S is not an op id (op-YYYYMMDDTHHMMSSZ-xxxxxxxx)" op-id)
      :repairs (list (%history-repair context))))
    (t
     (aitools.kernel.domain:flow-call
         (%call-with-store ports context "undo")
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
          (aitools.store.application:undo-op/k
           store op-id (list "undo" op-id)
           :lock-timeout-ms timeout :dry-run dry-run)
        (:on-committed (new-op-id results)
          (aitools.kernel.domain:flow-continue
           :on-ok (%fields-with (aitools.store.domain:write-result-fields
                                 results :op-id new-op-id :dry-run dry-run
                                 :max-diff-lines max-diff-lines)
                                (cons "undoes" op-id))))
        (:on-rejected (code message &key conflicts &allow-other-keys)
          (cond
            ((string= code "input.not-found")
             (aitools.kernel.domain:flow-continue
              :on-error code message
              :repairs (list (%history-repair context))))
            ((string= code "refusal.target-changed")
             (let ((path (aitools.store.domain:conflict-path (first conflicts))))
               (aitools.kernel.domain:flow-continue
                :on-error code message
                :conflicts (%conflicts-json conflicts)
                :repairs (list
                          (repair
                            "inspect-later-ops"
                            "See which later operations changed the path; undo those first if they should go too."
                            (%aitools context "history" path))
                          (repair "inspect-current" "Show the path's current state."
                                  (%aitools context "info" path))))))
            (t
             (aitools.kernel.domain:flow-call
               (%report-rejection "undo" code message)
               (:on-error (error-code error-message &key repairs)
                 (aitools.kernel.domain:flow-continue
                  :on-error error-code error-message
                  :repairs repairs))))))
        (:on-busy ()
          (aitools.kernel.domain:flow-call
            (%report-busy context (list "undo" op-id (and dry-run "--dry-run")))
            (:on-error (code message &key repairs)
              (aitools.kernel.domain:flow-continue
               :on-error code message
               :repairs repairs))))))))))
