;;;; packages/core/store/src/application/tx-stage.lisp
;;;;
;;;; Writing into a tx, `tx drop`, and `tx rebase`.
;;;;
;;;; One tx op: write the blobs, replace ops.jsonl, then replace index.json.
;;;; The index rename is the op's commit point: an ops.jsonl record numbered
;;;; past the index's LAST-TX-OP is ignored and overwritten by the next op,
;;;; so an interrupted op leaves the tx as it was after the previous one.
;;;; Drop replaces the index first for the same reason. Rebase rewrites
;;;; records the index already covers, so it writes them to the next
;;;; generation's ops file and lets the index rename switch to it.
;;;;
;;;; FAULT-POINT names: :tx-after-blobs, :tx-after-ops, :tx-after-index,
;;;; each with the tx id and tx op number.
(in-package #:aitools.store.application)

(defun %touch (store index path previous)
  "Make sure PATH is in INDEX (base = staged = its disk state, the base
content kept as a blob so `tx diff` can show it after the disk drifts), and
remember its pre-op staged state in the PREVIOUS alist box once."
  (let ((entry (tx-index-find index path)))
    (unless (assoc path (car previous) :test #'string=)
      (push (cons path (and entry (tx-path-staged entry))) (car previous)))
    (unless entry
      (let ((base (workspace-state store path)))
        (when (eq (entry-state-kind base) :file)
          (write-blob store (%io store read-file (%workspace-path store path))))
        (tx-index-put index path base base)))))

(defun %set-staged (index path state)
  (setf (tx-path-staged (tx-index-find index path)) state))

(defun %stage-results (store index results)
  "Apply planned RESULTS to INDEX (destructively). Returns (values previous
after): the ops.jsonl `previous` and `after` alists, in first-touch order."
  (let ((previous (list '())))
    (dolist (result results)
      (let ((path (change-result-path result))
            (after (change-result-after result)))
        (when (change-result-from result)
          (%touch store index (change-result-from result) previous)
          (%set-staged index (change-result-from result) (absent-state)))
        (%touch store index path previous)
        ;; A mode-only change carries no content: its staged hash is one
        ;; whose blob %TOUCH or an earlier op already wrote.
        (when (and (eq (entry-state-kind after) :file) (change-result-after-content result))
          (write-blob store (change-result-after-content result)))
        (%set-staged index path after)))
    (let ((previous (reverse (car previous))))
      (values previous
              (mapcar (lambda (pair)
                        (cons (car pair) (tx-path-staged (tx-index-find index (car pair)))))
                      previous)))))

(defun %stage-states (store index pairs)
  "Apply recorded (path . state) PAIRS to INDEX; returns (values previous after)."
  (let ((previous (list '())))
    (loop for (path . state) in pairs
          do (%touch store index path previous)
             (%set-staged index path state))
    (values (reverse (car previous)) pairs)))

(defun %save-tx (store tx-id index ops &key index-first)
  "Write OPS to INDEX's ops file and then INDEX (or the reverse with
INDEX-FIRST)."
  (flet ((save-ops () (%replace-file store (%tx-file store tx-id (tx-ops-file-name (tx-index-ops-generation index)))
                                     (encode-tx-ops ops)))
         (save-index () (%replace-file store (%tx-file store tx-id "index.json") (encode-tx-index index))))
    (if index-first
        (progn (save-index)
               (fault-point :tx-after-index tx-id (tx-index-last-tx-op index))
               (save-ops)
               (fault-point :tx-after-ops tx-id (tx-index-last-tx-op index)))
        (progn (save-ops)
               (fault-point :tx-after-ops tx-id (tx-index-last-tx-op index))
               (save-index)
               (fault-point :tx-after-index tx-id (tx-index-last-tx-op index))))))

(defun %reject-directory-moves (results)
  "A directory move inside a tx is recorded per path. Callers expand a
directory move into per-file requests; a whole-directory move result would
leave the index without the children, so it is refused."
  (find-if (lambda (result)
             (and (eq (change-result-action result) :moved)
                  (eq (entry-state-kind (change-result-after result)) :directory)))
           results))

(defun tx-stage/k (store tx-id argv validate &key replayable (lock-timeout-ms +default-lock-timeout-ms+)
                                              on-staged on-rejected on-not-found on-busy)
  "Record a write command's changes in TX-ID instead of the working tree.

VALIDATE is called as (VALIDATE VIEW COMMIT REJECT) with both locks held;
VIEW reads the tx state (VIEW-READ-FILE, VIEW-PATH-STATE), so guards such as
`--expect-hash` compare against the tx. COMMIT and REJECT are as for
COMMIT-CHANGES/K. REPLAYABLE says whether `tx rebase` may re-run the op:
true only for content-based selectors, `replace`, `apply`, and json writes
ARGV must then be enough for the feature to re-run it.

Continuations, after the locks are released:
  ON-STAGED (tx-op results)  TX-OP is the new op's number (NIL when nothing
                             changed); RESULTS as for COMMIT-CHANGES/K
  ON-REJECTED (code message &rest keys)
  ON-NOT-FOUND () / ON-BUSY ()"
  (declare (type function validate on-staged on-rejected on-not-found on-busy))
  (%run-under-tx-locks
   store tx-id lock-timeout-ms
   (lambda ()
     (multiple-value-bind (meta index ops) (%load-tx store tx-id)
       (declare (ignore meta))
       (let ((view (%make-store-view store index))
             (outcome nil))
         (funcall validate view
                  (lambda (requests)
                    (setf outcome
                          (%view-plan/k
                           view requests
                           (lambda (results)
                             (cond
                               ((null results) (lambda () (funcall on-staged nil '())))
                               ((%reject-directory-moves results)
                                (lambda () (funcall on-rejected "argument.invalid"
                                                    "a directory move inside a tx must be staged per path")))
                               (t
                                (let ((tx-op (1+ (tx-index-last-tx-op index))))
                                  (multiple-value-bind (previous after) (%stage-results store index results)
                                    (fault-point :tx-after-blobs tx-id tx-op)
                                    (setf (tx-index-last-tx-op index) tx-op)
                                    (%save-tx store tx-id index
                                              (append ops (list (make-tx-op-record
                                                                 :tx-op tx-op :argv argv
                                                                 :paths (mapcar #'car previous)
                                                                 :previous previous :after after
                                                                 :replayable (and replayable t)
                                                                 :time (aitools.kernel.domain:iso8601-utc (%io store now)))))))
                                  (lambda () (funcall on-staged tx-op results))))))
                           (lambda (&rest rejection) (lambda () (apply on-rejected rejection))))))
                  (lambda (&rest rejection)
                    (setf outcome (lambda () (apply on-rejected rejection)))))
         (or outcome
             (error "tx-stage/k: VALIDATE returned without calling COMMIT or REJECT")))))
   on-busy on-not-found))

(defun tx-drop/k (store tx-id tx-op &key (lock-timeout-ms +default-lock-timeout-ms+)
                                      on-dropped on-not-found on-busy)
  "Undo TX-OP and every later op in TX-ID. ON-DROPPED
(tx-ops) with the dropped numbers ascending. ON-NOT-FOUND () when the tx or
the op does not exist."
  (declare (type function on-dropped on-not-found on-busy))
  (%run-under-tx-locks
   store tx-id lock-timeout-ms
   (lambda ()
     (multiple-value-bind (meta index ops) (%load-tx store tx-id)
       (declare (ignore meta))
       (if (not (and (integerp tx-op) (<= 1 tx-op (tx-index-last-tx-op index))))
           on-not-found
           (let ((dropped (loop for n from tx-op to (tx-index-last-tx-op index) collect n)))
             (%save-tx store tx-id (tx-drop-index index ops tx-op) (subseq ops 0 (1- tx-op))
                       :index-first t)
             (collect-garbage store)
             (lambda () (funcall on-dropped dropped))))))
   on-busy on-not-found))

(defun tx-rebase/k (store tx-id replay &key (lock-timeout-ms +default-lock-timeout-ms+)
                                         on-rebased on-conflict on-not-found on-busy)
  "Move TX-ID's `base` of every drifted path to the current disk and
re-apply the ops that depend on it, in recorded order.

An op is replayed when it touched a drifted path or a path an earlier
replayed op rewrote; every other op keeps its recorded `after` states.
REPLAY is called as (REPLAY TX-OP-RECORD VIEW COMMIT REJECT): the feature
re-runs the op from its argv against VIEW, the rebuilt tx state so far, and
calls COMMIT with the new requests or REJECT when it no longer applies.

ON-REBASED (paths) with the drifted paths whose base moved (empty when
nothing drifted). ON-CONFLICT (conflicts) when some op to replay is
position-based (not replayable) or REPLAY rejects it; the tx is left
unchanged. ON-NOT-FOUND () / ON-BUSY ()."
  (declare (type function replay on-rebased on-conflict on-not-found on-busy))
  (%run-under-tx-locks
   store tx-id lock-timeout-ms
   (lambda ()
     (block rebase
       (multiple-value-bind (meta index ops) (%load-tx store tx-id)
         (declare (ignore meta))
         (let ((drift (tx-drift-paths index (lambda (path) (workspace-state store path)))))
           (if (null drift)
               (lambda () (funcall on-rebased '()))
               (let ((dirty (copy-list drift))
                     (rebuilt (make-tx-index))
                     (records '()))
                 (flet ((conflicts-for (record)
                          (loop for path in (tx-op-record-paths record)
                                for entry = (tx-index-find index path)
                                when (and entry (member path dirty :test #'string=))
                                  collect (make-conflict :path path :kind :write
                                                         :base (tx-path-base entry)
                                                         :current (workspace-state store path)))))
                   (dolist (record ops)
                     (let ((replayed (intersection (tx-op-record-paths record) dirty :test #'string=))
                           (view (%make-store-view store rebuilt)))
                       (cond
                         ((null replayed)
                          (multiple-value-bind (previous after) (%stage-states store rebuilt (tx-op-record-after record))
                            (push (make-tx-op-record :tx-op (tx-op-record-tx-op record) :argv (tx-op-record-argv record)
                                                     :paths (tx-op-record-paths record)
                                                     :previous previous :after after
                                                     :replayable (tx-op-record-replayable record)
                                                     :time (tx-op-record-time record))
                                  records)))
                         ((not (tx-op-record-replayable record))
                          (let ((conflicts (conflicts-for record)))
                            (return-from rebase (lambda () (funcall on-conflict conflicts)))))
                         (t
                          (let ((results
                                  (block replayed
                                    (funcall replay record view
                                             (lambda (requests)
                                               (%view-plan/k view requests
                                                             (lambda (results) (return-from replayed results))
                                                             (lambda (&rest rejection)
                                                               (declare (ignore rejection))
                                                               (return-from replayed :rejected))))
                                             (lambda (&rest rejection)
                                               (declare (ignore rejection))
                                               (return-from replayed :rejected)))
                                    (error "tx-rebase/k: REPLAY returned without calling COMMIT or REJECT"))))
                            (when (or (eq results :rejected) (%reject-directory-moves results))
                              (let ((conflicts (conflicts-for record)))
                                (return-from rebase (lambda () (funcall on-conflict conflicts)))))
                            (multiple-value-bind (previous after) (%stage-results store rebuilt results)
                              (push (make-tx-op-record :tx-op (tx-op-record-tx-op record) :argv (tx-op-record-argv record)
                                                       :paths (mapcar #'car previous)
                                                       :previous previous :after after
                                                       :replayable t
                                                       :time (tx-op-record-time record))
                                    records)
                              (dolist (path (mapcar #'car previous))
                                (pushnew path dirty :test #'string=))))))))
                   (setf (tx-index-last-tx-op rebuilt) (tx-index-last-tx-op index)
                         (tx-index-ops-generation rebuilt) (1+ (tx-index-ops-generation index)))
                   (%save-tx store tx-id rebuilt (nreverse records))
                   (let ((old (%tx-file store tx-id (tx-ops-file-name (tx-index-ops-generation index)))))
                     (%ignoring-vanished store old (lambda () (%io store unlink old))))
                   (collect-garbage store)
                   (lambda () (funcall on-rebased drift)))))))))
   on-busy on-not-found))
