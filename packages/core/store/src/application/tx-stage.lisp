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
  "Record PATH's disk state and prior staged state; retain bytes for tx diff after disk drift."
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
  "Apply planned RESULTS to INDEX and return previous/after alists in first-touch order."
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
              (mapcar (lambda (pair) (cons (car pair) (tx-path-staged (tx-index-find index (car pair)))))
                      previous)))))
(defun %stage-states (store index pairs)
  "Apply recorded (path . state) PAIRS to INDEX; returns (values previous after)."
  (let ((previous (list '())))
    (loop for (path . state) in pairs
          do (%touch store index path previous) (%set-staged index path state))
    (values (reverse (car previous)) pairs)))
(defun %save-tx (store tx-id index ops &key index-first)
  "Write OPS and INDEX, committing through INDEX's rename; reverse the order with INDEX-FIRST."
  (flet ((save-ops () (%replace-file store (%tx-file store tx-id (tx-ops-file-name (tx-index-ops-generation index)))
                                     (encode-tx-ops ops)))
         (save-index () (%replace-file store (%tx-file store tx-id "index.json") (encode-tx-index index))))
    (if index-first
        (progn (save-index)
               (fault-point :tx-after-index tx-id (tx-index-last-tx-op index))
               (save-ops) (fault-point :tx-after-ops tx-id (tx-index-last-tx-op index)))
        (progn (save-ops)
               (fault-point :tx-after-ops tx-id (tx-index-last-tx-op index))
               (save-index) (fault-point :tx-after-index tx-id (tx-index-last-tx-op index))))))
(defun %reject-directory-moves (results)
  "Reject whole-directory moves; tx stores them per path, so callers expand to per-file requests."
  (find-if (lambda (result)
             (and (eq (change-result-action result) :moved)
                  (eq (entry-state-kind (change-result-after result)) :directory)))
           results))

(aitools.kernel.domain:define-flow
    (tx-stage/k store tx-id argv validate &key replayable (lock-timeout-ms +default-lock-timeout-ms+)
                                              on-staged on-rejected on-not-found on-busy)
  "Record a write in TX-ID. VALIDATE receives VIEW, COMMIT, and REJECT with
both locks held; VIEW reads staged state for guards such as --expect-hash.
REPLAYABLE means ARGV can rebuild the op during rebase and is limited to
content-based selectors, replace, apply, and json writes.
After unlock, ON-STAGED receives (tx-op results), with NIL tx-op if unchanged.
ON-REJECTED receives (code message &key candidates diagnostics conflicts
repairs path); ON-NOT-FOUND and ON-BUSY receive no arguments."
  (declare (type function validate on-staged on-rejected on-not-found on-busy))
  (aitools.kernel.domain:flow-call
      (%run-under-tx-locks
       store tx-id lock-timeout-ms
       (lambda ()
         (multiple-value-bind (meta index ops) (%load-tx store tx-id)
           (declare (ignore meta))
           (let ((view (%make-store-view store index)) (outcome nil))
             (funcall validate view
                      (lambda (requests)
                        (setf outcome
                              (aitools.kernel.domain:flow-call
                                  (%view-plan/k view requests)
                                (:on-planned (results)
                                  (cond
                                    ((null results)
                                     (lambda ()
                                       (aitools.kernel.domain:flow-continue :on-staged nil '())))
                                    ((%reject-directory-moves results)
                                     (lambda ()
                                       (aitools.kernel.domain:flow-continue
                                         :on-rejected
                                         "argument.invalid"
                                         "directory moves in tx must be staged per path")))
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
                                                                      :time (aitools.kernel.domain:iso8601-utc
                                                                             (%io store now)))))))
                                       (lambda ()
                                         (aitools.kernel.domain:flow-continue
                                          :on-staged tx-op results))))))
                                (:on-rejected (code message)
                                  (lambda ()
                                    (aitools.kernel.domain:flow-continue
                                     :on-rejected code message)))))) (lambda (code message
                                  &key (candidates nil candidates-p)
                                       (diagnostics nil diagnostics-p)
                                       (conflicts nil conflicts-p)
                                       (repairs nil repairs-p)
                                       (path nil path-p))
                        (setf outcome
                              (lambda ()
                                (%flow-continue-rejection
                                 (list* code message
                                        (%rejection-keywords
                                         :candidates candidates :candidates-p candidates-p
                                         :diagnostics diagnostics :diagnostics-p diagnostics-p
                                         :conflicts conflicts :conflicts-p conflicts-p
                                         :repairs repairs :repairs-p repairs-p
                                         :path path :path-p path-p))))))) (or outcome
                 (error "tx-stage/k: VALIDATE returned without calling COMMIT or REJECT"))))))
    (:on-busy ()
      (aitools.kernel.domain:flow-continue :on-busy))
    (:on-not-found ()
      (aitools.kernel.domain:flow-continue :on-not-found))))

