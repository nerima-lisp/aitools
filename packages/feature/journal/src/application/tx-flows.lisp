;;;; packages/feature/journal/src/application/tx-flows.lisp
;;;;
;;;; `tx begin`, `tx status`, and `tx diff` resolve the workspace's store
;;;; and shape transaction summaries, details, and bounded diffs.
(in-package #:aitools.journal.application)

(aitools.kernel.domain:define-flow
    (%report-tx-not-found context tx-id &key on-error)
  (declare (type function on-error))
  (aitools.kernel.domain:flow-continue
   :on-error "input.not-found" (format nil "no open tx ~A" tx-id)
   :repairs (list (%tx-list-repair context))))

(aitools.kernel.domain:define-flow
    (tx-begin-flow ports context &key name on-ok on-partial on-error)
  (declare (type function on-ok on-error) (ignore on-partial))
  (aitools.kernel.domain:flow-call
      (%call-with-store ports context "tx.begin")
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
        (aitools.store.application:tx-begin/k
         store :name name :lock-timeout-ms timeout)
        (:on-begun (tx-id name created)
          (declare (ignore created))
          (aitools.kernel.domain:flow-continue
           :on-ok (list (cons "tx" tx-id) (cons "name" (or name (json-null))))))
        (:on-busy ()
          (aitools.kernel.domain:flow-call
            (%report-busy context (list "tx" "begin" (and name (list "--name" name))))
            (:on-error (code message &key repairs)
              (aitools.kernel.domain:flow-continue
               :on-error code message
               :repairs repairs))))))))

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
  (aitools.kernel.domain:flow-call
      (%call-with-store ports context "tx.status")
    (:on-error (code message &key candidates diagnostics conflicts repairs)
      (aitools.kernel.domain:flow-continue
       :on-error code message
       :candidates candidates
       :diagnostics diagnostics
       :conflicts conflicts
       :repairs repairs))
    (:on-store (store root timeout)
      (declare (ignore root timeout))
      (if (null tx-id)
          (let ((statuses (aitools.store.application:tx-list store)))
            (aitools.kernel.domain:flow-continue
             :on-ok (list (cons "items" (mapcar #'%tx-summary statuses))
                          (cons "total" (length statuses)))))
          (aitools.kernel.domain:flow-call
   (aitools.store.application:tx-status/k store tx-id)
   (:on-status (status)
     (aitools.kernel.domain:flow-continue
                         :on-ok (%tx-detail-fields status)))
   (:on-not-found ()
     (aitools.kernel.domain:flow-call
                               (%report-tx-not-found context tx-id)
                             (:on-error (code message &key repairs)
                               (aitools.kernel.domain:flow-continue
                                :on-error code message
                                :repairs repairs)))))))))

(aitools.kernel.domain:define-flow
    (tx-diff-flow ports context tx-id &key (max-diff-lines nil max-diff-lines-supplied-p)
                  on-ok on-partial on-error)
  "The tx's changes from `base` to `staged` in the write-result shape, without
an op id. A cut diff points at the same command with a limit that fits."
  (declare (type function on-ok on-error) (ignore on-partial))
  (unless max-diff-lines-supplied-p
    (setf max-diff-lines aitools.store.domain:+default-max-diff-lines+))
  (aitools.kernel.domain:flow-call
    (%call-with-store ports context "tx.diff")
    (:on-error (code message &key candidates diagnostics conflicts repairs)
      (aitools.kernel.domain:flow-continue
       :on-error code message
       :candidates candidates
       :diagnostics diagnostics
       :conflicts conflicts
       :repairs repairs))
    (:on-store (store root timeout)
      (declare (ignore root timeout))
      (aitools.kernel.domain:flow-call
   (aitools.store.application:tx-diff/k store tx-id)
   (:on-diff (results)
     (multiple-value-bind (changes longest)
                      (aitools.store.domain:changes->json results :max-diff-lines max-diff-lines)
                    (aitools.kernel.domain:flow-continue
                     :on-ok (append (list (cons "tx" tx-id) (cons "changes" changes))
                                    (when longest
                                      (list (cons "next_commands"
                                                  (list (%aitools context "tx" "diff" tx-id "--max-diff-lines"
                                                                  (princ-to-string longest))))))))))
   (:on-not-found ()
     (aitools.kernel.domain:flow-call
                           (%report-tx-not-found context tx-id)
                         (:on-error (code message &key repairs)
                           (aitools.kernel.domain:flow-continue
                            :on-error code message
                            :repairs repairs))))))))

