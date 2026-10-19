;;;; packages/feature/journal/src/application/tx-flows.lisp
;;;;
;;;; The `tx` group over the store's tx API. Locking,
;;;; conflict detection and the commit are the store's; these flows
;;;; resolve the workspace, pick the replayer for `tx rebase`, and shape the
;;;; results and repairs of docs/src/reference/transactions.md.
(in-package #:aitools.journal.application)

(aitools.kernel.domain:define-flow
    (%report-tx-not-found context tx-id &key on-error)
  (declare (type function on-error))
  (aitools.kernel.domain:flow-continue :on-error "input.not-found" (format nil "no open tx ~A" tx-id)
           :repairs (list (%tx-list-repair context))))

(aitools.kernel.domain:define-flow
    (tx-begin-flow ports context &key name on-ok on-partial on-error)
  (declare (type function on-ok on-error) (ignore on-partial))
  (aitools.kernel.domain:flow-call (%call-with-store ports context "tx.begin") (:on-error (code message &key candidates diagnostics conflicts repairs) (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs)) (:on-store (store root timeout) (declare (ignore root))
     (aitools.store.application:tx-begin/k
      store :name name :lock-timeout-ms timeout
      :on-begun (lambda (tx-id name created)
                  (declare (ignore created))
                  (aitools.kernel.domain:flow-continue :on-ok (list (cons "tx" tx-id) (cons "name" (or name (json-null))))))
      :on-busy (lambda () (aitools.kernel.domain:flow-call (%report-busy context (list "tx" "begin" (and name (list "--name" name)))) (:on-error (code message &key repairs) (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))))))))

(defun %tx-summary (status)
  (json-object "tx" (aitools.store.application:tx-status-id status)
               "name" (or (aitools.store.application:tx-status-name status) (json-null))
               "created" (aitools.store.application:tx-status-created status)
               "ops" (length (aitools.store.application:tx-status-ops status))
               "paths" (length (aitools.store.application:tx-status-paths status))
               "drift" (aitools.store.application:tx-status-drift status)
               "stale_reads" (aitools.store.application:tx-status-stale-reads status)))

(defun %tx-detail-fields (status)
  (list (cons "tx" (aitools.store.application:tx-status-id status))
        (cons "name" (or (aitools.store.application:tx-status-name status) (json-null)))
        (cons "created" (aitools.store.application:tx-status-created status))
        (cons "ops" (mapcar (lambda (record)
                              (json-object "tx_op" (aitools.store.domain:tx-op-record-tx-op record)
                                           "command" (argv-command-line
                                                      (aitools.store.domain:tx-op-record-argv record))
                                           "paths" (aitools.store.domain:tx-op-record-paths record)))
                            (aitools.store.application:tx-status-ops status)))
        (cons "paths" (mapcar (lambda (entry)
                                (json-object "path" (aitools.store.domain:tx-path-path entry)
                                             "action" (tx-path-action entry)
                                             "base" (state-hash (aitools.store.domain:tx-path-base entry))
                                             "staged" (state-hash (aitools.store.domain:tx-path-staged entry))))
                              (aitools.store.application:tx-status-paths status)))
        (cons "drift" (aitools.store.application:tx-status-drift status))
        (cons "stale_reads" (aitools.store.application:tx-status-stale-reads status))))

(aitools.kernel.domain:define-flow
    (tx-status-flow ports context tx-id &key on-ok on-partial on-error)
  "Without TX-ID, every open tx as {tx, name, created, ops, paths,
drift, stale_reads} (`ops` and `paths` are counts); with it, that tx's ops,
paths, drift and stale reads."
  (declare (type function on-ok on-error) (ignore on-partial))
  (aitools.kernel.domain:flow-call (%call-with-store ports context "tx.status") (:on-error (code message &key candidates diagnostics conflicts repairs) (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs)) (:on-store (store root timeout) (declare (ignore root timeout))
     (if (null tx-id)
         (let ((statuses (aitools.store.application:tx-list store)))
           (aitools.kernel.domain:flow-continue :on-ok (list (cons "items" (mapcar #'%tx-summary statuses))
                                (cons "total" (length statuses)))))
         (aitools.store.application:tx-status/k
          store tx-id
          :on-status (lambda (status) (aitools.kernel.domain:flow-continue :on-ok (%tx-detail-fields status)))
          :on-not-found (lambda () (aitools.kernel.domain:flow-call (%report-tx-not-found context tx-id) (:on-error (code message &key repairs) (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs)))))))))

(aitools.kernel.domain:define-flow
    (tx-diff-flow ports context tx-id &key (max-diff-lines aitools.store.domain:+default-max-diff-lines+)
                                           on-ok on-partial on-error)
  "The tx's changes from `base` to `staged` in the write-result shape, without
an op id. A cut diff points at the same command with a limit that fits."
  (declare (type function on-ok on-error) (ignore on-partial))
  (aitools.kernel.domain:flow-call (%call-with-store ports context "tx.diff") (:on-error (code message &key candidates diagnostics conflicts repairs) (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs)) (:on-store (store root timeout) (declare (ignore root timeout))
     (aitools.store.application:tx-diff/k
      store tx-id
      :on-diff (lambda (results)
                 (multiple-value-bind (changes longest)
                     (aitools.store.domain:changes->json results :max-diff-lines max-diff-lines)
                   (aitools.kernel.domain:flow-continue :on-ok (append (list (cons "tx" tx-id) (cons "changes" changes))
                                          (when longest
                                            (list (cons "next_commands"
                                                        (list (%aitools context "tx" "diff" tx-id "--max-diff-lines"
                                                                        (princ-to-string longest))))))))))
      :on-not-found (lambda () (aitools.kernel.domain:flow-call (%report-tx-not-found context tx-id) (:on-error (code message &key repairs) (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))))))))

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
        (aitools.kernel.domain:flow-continue :on-error "argument.invalid" (format nil "tx_op ~S is not an operation number" tx-op)
                 :repairs (list (repair "list-ops" "List the tx's operations and their numbers."
                                        (%aitools context "tx" "status" tx-id))))
        (aitools.kernel.domain:flow-call (%call-with-store ports context "tx.drop") (:on-error (code message &key candidates diagnostics conflicts repairs) (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs)) (:on-store (store root timeout) (declare (ignore root))
           (aitools.store.application:tx-drop/k
            store tx-id number :lock-timeout-ms timeout
            :on-dropped (lambda (dropped)
                          (aitools.kernel.domain:flow-continue :on-ok (list (cons "tx" tx-id) (cons "dropped" dropped))))
            :on-not-found (lambda ()
                            (aitools.kernel.domain:flow-continue :on-error "input.not-found" (format nil "no op ~A in tx ~A" tx-op tx-id)
                                     :repairs (list (repair "list-ops" "List the tx's operations and their numbers."
                                                            (%aitools context "tx" "status" tx-id))
                                                    (%tx-list-repair context))))
            :on-busy (lambda () (aitools.kernel.domain:flow-call (%report-busy context (list "tx" "drop" tx-id tx-op)) (:on-error (code message &key repairs) (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))))))))))

(aitools.kernel.domain:define-flow
    (%replay record view &key on-commit on-reject)
  (let* ((argv (aitools.store.domain:tx-op-record-argv record))
         (replayer (find-tx-replayer argv)))
    (if replayer
        (funcall replayer
         argv
         view
         (lambda (requests)
           (aitools.kernel.domain:flow-continue :on-commit requests))
         (lambda (code message &rest keys)
           (declare (ignore keys))
           (aitools.kernel.domain:flow-continue :on-reject code message)))
        (aitools.kernel.domain:flow-continue :on-reject "refusal.target-changed"
                 (format nil "no replayer is registered for ~A" (argv-command-line argv))))))

(aitools.kernel.domain:define-flow
    (tx-rebase-flow ports context tx-id &key on-ok on-partial on-error)
  "Re-apply the tx's ops onto the current disk for drifted paths.
A position-based op, an op whose command registered no replayer, or a
replay that no longer applies is a conflict (exit 2) and leaves the tx as
it was."
  (declare (type function on-ok on-error) (ignore on-partial))
  (aitools.kernel.domain:flow-call (%call-with-store ports context "tx.rebase") (:on-error (code message &key candidates diagnostics conflicts repairs) (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs)) (:on-store (store root timeout) (declare (ignore root))
     (aitools.store.application:tx-rebase/k
      store tx-id (lambda (record view commit reject) (aitools.kernel.domain:flow-call (%replay record view) (:on-commit (requests) (funcall commit requests)) (:on-reject (&rest rejection) (apply reject rejection)))) :lock-timeout-ms timeout
      :on-rebased (lambda (paths) (aitools.kernel.domain:flow-continue :on-ok (list (cons "tx" tx-id) (cons "rebased" paths))))
      :on-conflict (lambda (conflicts)
                     (aitools.kernel.domain:flow-continue :on-error "refusal.target-changed"
                              (format nil "tx ~A has an operation that cannot be re-applied to the changed files" tx-id)
                              :conflicts (%conflicts-json conflicts)
                              :repairs (list (repair "inspect-tx" "See which operation touched the conflicting path; drop it and later ones with tx drop."
                                                     (%aitools context "tx" "status" tx-id))
                                             (repair "abort" "Discard the tx and start over from the current files."
                                                     (%aitools context "tx" "abort" tx-id)))))
      :on-not-found (lambda () (aitools.kernel.domain:flow-call (%report-tx-not-found context tx-id) (:on-error (code message &key repairs) (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))))
      :on-busy (lambda () (aitools.kernel.domain:flow-call (%report-busy context (list "tx" "rebase" tx-id)) (:on-error (code message &key repairs) (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))))))))

(aitools.kernel.domain:define-flow
    (tx-commit-flow ports context tx-id &key ignore-stale-reads
                                             (max-diff-lines aitools.store.domain:+default-max-diff-lines+)
                                             on-ok on-partial on-error)
  "Write the whole tx through the store commit as one op, or report the
write and read conflicts (exit 2) and write nothing."
  (declare (type function on-ok on-error) (ignore on-partial))
  (let ((flag (and ignore-stale-reads "--ignore-stale-reads")))
    (aitools.kernel.domain:flow-call (%call-with-store ports context "tx.commit") (:on-error (code message &key candidates diagnostics conflicts repairs) (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs)) (:on-store (store root timeout) (declare (ignore root))
       (aitools.store.application:tx-commit/k
        store tx-id (remove nil (list "tx" "commit" tx-id flag))
        :ignore-stale-reads ignore-stale-reads
        :lock-timeout-ms timeout
        :on-committed (lambda (op-id results)
                        (aitools.kernel.domain:flow-continue :on-ok (aitools.store.domain:write-result-fields
                                        results :op-id op-id :max-diff-lines max-diff-lines)))
        :on-rejected (lambda (code message &key conflicts &allow-other-keys)
                       (if (string= code "refusal.target-changed")
                           (aitools.kernel.domain:flow-continue :on-error code message
                                    :conflicts (%conflicts-json conflicts)
                                    :repairs (commit-conflict-repairs tx-id conflicts :globals (%globals context)))
                           (aitools.kernel.domain:flow-call (%report-rejection "tx.commit" code message) (:on-error (error-code error-message &key repairs) (aitools.kernel.domain:flow-continue :on-error error-code error-message :repairs repairs)))))
        :on-not-found (lambda () (aitools.kernel.domain:flow-call (%report-tx-not-found context tx-id) (:on-error (code message &key repairs) (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))))
        :on-busy (lambda () (aitools.kernel.domain:flow-call (%report-busy context (list "tx" "commit" tx-id flag)) (:on-error (code message &key repairs) (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs)))))))))

(aitools.kernel.domain:define-flow
    (tx-abort-flow ports context tx-id &key on-ok on-partial on-error)
  "Delete the tx without touching the working tree; `discarded`
lists its write-set paths."
  (declare (type function on-ok on-error) (ignore on-partial))
  (aitools.kernel.domain:flow-call (%call-with-store ports context "tx.abort") (:on-error (code message &key candidates diagnostics conflicts repairs) (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs)) (:on-store (store root timeout) (declare (ignore root))
     (aitools.store.application:tx-abort/k
      store tx-id :lock-timeout-ms timeout
      :on-aborted (lambda (paths) (aitools.kernel.domain:flow-continue :on-ok (list (cons "tx" tx-id) (cons "discarded" paths))))
      :on-not-found (lambda () (aitools.kernel.domain:flow-call (%report-tx-not-found context tx-id) (:on-error (code message &key repairs) (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))))
      :on-busy (lambda () (aitools.kernel.domain:flow-call (%report-busy context (list "tx" "abort" tx-id)) (:on-error (code message &key repairs) (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))))))))
