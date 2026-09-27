;;;; packages/feature/journal/src/application/undo-flow.lisp
;;;;
;;;; `undo <op_id>` over the store's UNDO-OP/K: the op id is
;;;; required so that one agent never undoes another's op by accident, and a
;;;; path changed since the op makes the whole undo a conflict (exit 2).
(in-package #:aitools.journal.application)

(defun undo-flow (ports context op-id &key dry-run (max-diff-lines aitools.store.domain:+default-max-diff-lines+)
                                        on-ok on-partial on-error)
  (declare (type function on-ok on-error) (ignore on-partial))
  (cond
    ((or (null op-id) (zerop (length op-id)))
     (funcall on-error "argument.invalid"
              "undo needs the op_id of the operation to undo; it never picks one itself"
              :repairs (list (%history-repair context))))
    ((not (aitools.store.domain:valid-op-id-p op-id))
     (funcall on-error "input.not-found" (format nil "~S is not an op id (op-YYYYMMDDTHHMMSSZ-xxxxxxxx)" op-id)
              :repairs (list (%history-repair context))))
    (t
     (%call-with-store
      ports context "undo" on-error
      (lambda (store root timeout)
        (declare (ignore root))
        (aitools.store.application:undo-op/k
         store op-id (list "undo" op-id)
         :lock-timeout-ms timeout
         :dry-run dry-run
         :on-committed
         (lambda (new-op-id results)
           (funcall on-ok
                    (%fields-with (aitools.store.domain:write-result-fields
                                   results :op-id new-op-id :dry-run dry-run :max-diff-lines max-diff-lines)
                                  (cons "undoes" op-id))))
         :on-rejected
         (lambda (code message &key conflicts &allow-other-keys)
           (cond
             ((string= code "input.not-found")
              (funcall on-error code message :repairs (list (%history-repair context))))
             ((string= code "refusal.target-changed")
              (let ((path (aitools.store.domain:conflict-path (first conflicts))))
                (funcall on-error code message
                         :conflicts (%conflicts-json conflicts)
                         :repairs (list (repair "inspect-later-ops"
                                                "See which later operations changed the path; undo those first if they should go too."
                                                (%aitools context "history" path))
                                        (repair "inspect-current" "Show the path's current state."
                                                (%aitools context "info" path))))))
             (t (%report-rejection on-error "undo" code message))))
         :on-busy
         (lambda ()
           (%report-busy on-error context "undo" op-id (and dry-run "--dry-run")))))))))
