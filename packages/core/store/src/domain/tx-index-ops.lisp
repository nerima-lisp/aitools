(in-package #:aitools.store.domain)

(defun tx-index-referenced-blobs (index)
  (let ((hashes '()))
    (loop for entry being the hash-values of (tx-index-paths index)
          do (dolist (state (list (tx-path-base entry) (tx-path-staged entry)))
               (when (eq (entry-state-kind state) :file)
                 (push (entry-state-hash state) hashes))))
    hashes))

(defun tx-drift-paths (index lookup-state)
  "`drift`: write-set paths whose disk state no longer equals `base`."
  (declare (type function lookup-state))
  (loop for entry in (%sorted-tx-paths index)
        unless (entry-state-equal (tx-path-base entry) (funcall lookup-state (tx-path-path entry)))
          collect (tx-path-path entry)))

(defun tx-commit-conflicts (index reads lookup-state &key ignore-stale-reads)
  "The commit check. Write conflicts: every write-set path whose disk state
differs from `base`. Read conflicts: every recorded read whose disk state
differs from the record, unless IGNORE-STALE-READS; a path already reported
as a write conflict is not repeated as a read conflict."
  (declare (type function lookup-state))
  (let ((conflicts '()))
    (dolist (entry (%sorted-tx-paths index))
      (let ((current (funcall lookup-state (tx-path-path entry))))
        (unless (entry-state-equal (tx-path-base entry) current)
          (push (make-conflict :path (tx-path-path entry) :kind :write
                               :base (tx-path-base entry) :current current)
                conflicts))))
    (unless ignore-stale-reads
      (dolist (pair (sort (copy-list reads) #'string< :key #'car))
        (unless (find (car pair) conflicts :key #'conflict-path :test #'string=)
          (let ((current (funcall lookup-state (car pair))))
            (unless (entry-state-equal (cdr pair) current)
              (push (make-conflict :path (car pair) :kind :read :base (cdr pair) :current current)
                    conflicts))))))
    (nreverse conflicts)))

(defun tx-drop-index (index records from-tx-op)
  "`tx drop`: a copy of INDEX with every op numbered FROM-TX-OP or later
undone, newest first, by restoring each path's recorded previous staged
state (removing the path when it was not in the index before)."
  (let ((copy (copy-tx-index-deep index)))
    (dolist (record (reverse records))
      (when (>= (tx-op-record-tx-op record) from-tx-op)
        (dolist (pair (tx-op-record-previous record))
          (if (cdr pair)
              (setf (tx-path-staged (tx-index-find copy (car pair))) (cdr pair))
              (tx-index-remove copy (car pair))))))
    (setf (tx-index-last-tx-op copy) (1- from-tx-op))
    copy))

(defun tx-ops-referenced-blobs (records)
  "Blob hashes the `previous` and `after` states of RECORDS name: `tx drop`
and `tx rebase` restore them, so they must outlive the index entries that
later ops overwrote."
  (let ((hashes '()))
    (dolist (record records hashes)
      (dolist (pair (append (tx-op-record-previous record) (tx-op-record-after record)))
        (let ((state (cdr pair)))
          (when (and state (eq (entry-state-kind state) :file))
            (push (entry-state-hash state) hashes)))))))
