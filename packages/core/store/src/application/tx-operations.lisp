(in-package #:aitools.store.application)

(aitools.kernel.domain:define-flow
    (tx-drop/k store tx-id tx-op &key (lock-timeout-ms +default-lock-timeout-ms+)
                                      on-dropped on-not-found on-busy)
  "Undo TX-OP and later ops; ON-DROPPED gets their numbers ascending.
ON-NOT-FOUND means the tx or op is absent."
  (declare (type function on-dropped on-not-found on-busy))
  (aitools.kernel.domain:flow-call
      (%run-under-tx-locks
       store tx-id lock-timeout-ms
       (lambda ()
         (multiple-value-bind (meta index ops) (%load-tx store tx-id)
           (declare (ignore meta))
           (if (not (and (integerp tx-op) (<= 1 tx-op (tx-index-last-tx-op index))))
               (lambda () (aitools.kernel.domain:flow-continue :on-not-found))
               (let ((dropped (loop for n from tx-op to (tx-index-last-tx-op index) collect n)))
                 (%save-tx store tx-id (tx-drop-index index ops tx-op) (subseq ops 0 (1- tx-op))
                           :index-first t)
                 (collect-garbage store)
                 (lambda () (aitools.kernel.domain:flow-continue :on-dropped dropped)))))))
    (:on-busy ()
      (aitools.kernel.domain:flow-continue :on-busy))
    (:on-not-found ()
      (aitools.kernel.domain:flow-continue :on-not-found))))

(aitools.kernel.domain:define-flow
    (tx-rebase/k store tx-id replay &key (lock-timeout-ms +default-lock-timeout-ms+)
                                         on-rebased on-conflict on-not-found on-busy)
  "Move drifted paths' bases to current disk and reapply dependent ops in
recorded order. Replay applies to ops touching drift or a path an earlier
replay rewrote; other ops keep their recorded after states. REPLAY receives
(record view commit reject) and reruns argv against the rebuilt tx.
ON-REBASED receives drifted paths (empty if none). ON-CONFLICT receives
conflicts when an op is not replayable or its replay rejects; the tx remains
unchanged. ON-NOT-FOUND and ON-BUSY receive no arguments."
  (declare (type function replay on-rebased on-conflict on-not-found on-busy))
  (aitools.kernel.domain:flow-call
      (%run-under-tx-locks
       store tx-id lock-timeout-ms
       (lambda ()
         (multiple-value-bind (meta index ops) (%load-tx store tx-id)
           (declare (ignore meta))
           (let ((drift (tx-drift-paths index (lambda (path) (workspace-state store path)))))
             (if (null drift)
                 (lambda () (aitools.kernel.domain:flow-continue :on-rebased '()))
                 (let ((dirty (copy-list drift))
                       (rebuilt (make-tx-index))
                       (records '())
                       (conflict-p nil)
                       (conflicts nil))
                   (flet ((conflicts-for (record)
                            (loop for path in (tx-op-record-paths record)
                                  for entry = (tx-index-find index path)
                                  when (and entry (member path dirty :test #'string=))
                                    collect (make-conflict :path path :kind :write
                                                           :base (tx-path-base entry)
                                                           :current (workspace-state store path)))))
                     (loop for record in ops while (not conflict-p)
                           do (let ((replayed (intersection (tx-op-record-paths record) dirty :test #'string=))
                                    (view (%make-store-view store rebuilt)))
                                (cond
                                  ((null replayed)
                                   (multiple-value-bind (previous after)
                                       (%stage-states store rebuilt (tx-op-record-after record))
                                     (push (make-tx-op-record
                                            :tx-op (tx-op-record-tx-op record)
                                            :argv (tx-op-record-argv record)
                                            :paths (tx-op-record-paths record)
                                            :previous previous :after after
                                            :replayable (tx-op-record-replayable record)
                                            :time (tx-op-record-time record))
                                           records)))
                                  ((not (tx-op-record-replayable record))
                                   (setf conflict-p t conflicts (conflicts-for record)))
                                  (t
                                   (let ((replay-result
                                          (let* ((replay-tag (gensym "REPLAYED-"))
                                                 (replay-outcome
                                                   (catch replay-tag
                                                     (funcall replay record view
                                                              (lambda (requests)
                                                                (aitools.kernel.domain:flow-call
                                                                    (%view-plan/k view requests)
                                                                  (:on-planned (planned)
                                                                    (throw replay-tag (list :planned planned)))
                                                                  (:on-rejected (code message)
                                                                    (declare (ignore code message))
                                                                    (throw replay-tag (list :rejected nil)))))
                                                              (lambda (&rest rejection)
                                                                (declare (ignore rejection))
                                                                (throw replay-tag (list :rejected nil))))
                                                     (error
 "tx-rebase/k: REPLAY returned without calling COMMIT or REJECT"))))
                                            (if (eq (first replay-outcome) :rejected)
                                                :rejected
                                                (second replay-outcome)))))
                                     (if (or (eq replay-result :rejected)
                                             (%reject-directory-moves replay-result))
                                         (setf conflict-p t conflicts (conflicts-for record))
                                         (multiple-value-bind (previous after)
                                             (%stage-results store rebuilt replay-result)
                                           (push (make-tx-op-record
                                                  :tx-op (tx-op-record-tx-op record)
                                                  :argv (tx-op-record-argv record)
                                                  :paths (mapcar (function car) previous)
                                                  :previous previous :after after
                                                  :replayable t
                                                  :time (tx-op-record-time record))
                                                 records)
                                           (dolist (path (mapcar (function car) previous))
                                             (pushnew path dirty :test (function string=))))))))))
                   (if conflict-p
                       (lambda () (aitools.kernel.domain:flow-continue :on-conflict conflicts))
                       (progn
                         (setf (tx-index-last-tx-op rebuilt) (tx-index-last-tx-op index)
                               (tx-index-ops-generation rebuilt) (1+ (tx-index-ops-generation index)))
                         (%save-tx store tx-id rebuilt (nreverse records))
                         (let ((old (%tx-file store tx-id (tx-ops-file-name (tx-index-ops-generation index)))))
                           (%ignoring-vanished store old (lambda () (%io store unlink old))))
                         (collect-garbage store)
                         (lambda () (aitools.kernel.domain:flow-continue :on-rebased drift)))))))))))
    (:on-busy ()
      (aitools.kernel.domain:flow-continue :on-busy))
    (:on-not-found ()
      (aitools.kernel.domain:flow-continue :on-not-found))))

