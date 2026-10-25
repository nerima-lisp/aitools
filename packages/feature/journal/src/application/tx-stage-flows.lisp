;;;;
;;;; `tx drop` and `tx rebase` use the store's tx API. Rebase applies staged
;;;; operations through the registered journal replayers.
(in-package #:aitools.journal.application)

(defun %parse-tx-op (text)
  (and text
       (plusp (length text))
       (every (lambda (char) (char<= #\0 char #\9)) text)
       (< (length text) 10)
       (parse-integer text)))

(aitools.kernel.domain:define-flow
    (tx-drop-flow ports context tx-id tx-op &key on-ok on-partial on-error)
  "Undo TX-OP (a decimal string) and every later op of the tx."
  (declare (type function on-ok on-error) (ignore on-partial))
  (let ((number (%parse-tx-op tx-op)))
    (if (null number)
        (aitools.kernel.domain:flow-continue
         :on-error "argument.invalid" (format nil "tx_op ~S is not an operation number" tx-op)
         :repairs (list (repair "list-ops" "List the tx's operations and their numbers."
                                (%aitools context "tx" "status" tx-id))))
        (aitools.kernel.domain:flow-call
            (%call-with-store ports context "tx.drop")
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
            (aitools.store.application:tx-drop/k
             store tx-id number :lock-timeout-ms timeout)
          (:on-dropped (dropped)
            (aitools.kernel.domain:flow-continue
             :on-ok (list (cons "tx" tx-id) (cons "dropped" dropped))))
          (:on-not-found ()
            (aitools.kernel.domain:flow-continue
             :on-error "input.not-found" (format nil "no op ~A in tx ~A" tx-op tx-id)
             :repairs (list (repair "list-ops" "List the tx's operations and their numbers."
                                    (%aitools context "tx" "status" tx-id))
                            (%tx-list-repair context))))
          (:on-busy ()
            (aitools.kernel.domain:flow-call
              (%report-busy context (list "tx" "drop" tx-id tx-op))
              (:on-error (code message &key repairs)
                (aitools.kernel.domain:flow-continue
                 :on-error code message
                 :repairs repairs))))))))))

(aitools.kernel.domain:define-flow
    (%replay record view &key on-commit on-reject)
  (let* ((argv (aitools.store.domain:tx-op-record-argv record))
         (replayer (find-tx-replayer argv)))
    (if replayer
        (funcall replayer
                 argv
                 view
                 (lambda (requests)
                   (aitools.kernel.domain:flow-continue
                    :on-commit requests))
                 (lambda (code message &rest keys)
                   (declare (ignore keys))
                   (aitools.kernel.domain:flow-continue
                    :on-reject code message)))
        (aitools.kernel.domain:flow-continue
         :on-reject "refusal.target-changed"
         (format nil "no replayer is registered for ~A" (argv-command-line argv))))))

(aitools.kernel.domain:define-flow
    (tx-rebase-flow ports context tx-id &key on-ok on-partial on-error)
  "Re-apply the tx's ops onto the current disk for drifted paths.
A position-based op, an op whose command registered no replayer, or a
replay that no longer applies is a conflict (exit 2) and leaves the tx as
it was."
  (declare (type function on-ok on-error) (ignore on-partial))
  (aitools.kernel.domain:flow-call
      (%call-with-store ports context "tx.rebase")
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
      (aitools.store.application:tx-rebase/k
       store tx-id
       (lambda (record view commit reject)
         (aitools.kernel.domain:flow-call
           (%replay record view)
           (:on-commit (requests)
             (funcall commit requests))
           (:on-reject (&rest rejection)
             (apply reject rejection))))
       :lock-timeout-ms timeout)
    (:on-rebased (paths)
      (aitools.kernel.domain:flow-continue
       :on-ok (list (cons "tx" tx-id) (cons "rebased" paths))))
    (:on-conflict (conflicts)
      (aitools.kernel.domain:flow-continue
       :on-error "refusal.target-changed"
       (format nil "tx ~A has an operation that cannot be re-applied to the changed files" tx-id)
       :conflicts (%conflicts-json conflicts)
       :repairs (list (repair "inspect-tx"
                              "See which operation touched the conflicting path; drop it and later ones with tx drop."
                              (%aitools context "tx" "status" tx-id))
                      (repair "abort" "Discard the tx and start over from the current files."
                              (%aitools context "tx" "abort" tx-id)))))
    (:on-not-found ()
      (aitools.kernel.domain:flow-call
        (%report-tx-not-found context tx-id)
        (:on-error (code message &key repairs)
          (aitools.kernel.domain:flow-continue
           :on-error code message
           :repairs repairs))))
    (:on-busy ()
      (aitools.kernel.domain:flow-call
        (%report-busy context (list "tx" "rebase" tx-id))
        (:on-error (code message &key repairs)
          (aitools.kernel.domain:flow-continue
           :on-error code message
           :repairs repairs))))))))
