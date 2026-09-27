;;;; packages/core/store/src/domain/tx-model.lisp
;;;;
;;;; The tx records (docs/src/reference/transactions.md). `index.json` holds, per path, `base` (the disk
;;;; state when the tx first touched it) and `staged` (the state inside the
;;;; tx), plus LAST-TX-OP: `ops.jsonl` entries beyond it belong to an op
;;;; whose index replacement never happened and are ignored, which is what
;;;; makes "ops first, then index" a single commit point per tx op.
;;;; A rebase rewrites every record, so it writes a new ops file instead,
;;;; named by OPS-GENERATION (ops.jsonl for 0, then ops.<n>.jsonl), and the
;;;; index rename switches both at once.
;;;; `reads.json` is the read set. File states in `base` and `staged`
;;;; name blobs; the blob store keeps them until the tx is committed or
;;;; aborted.
(in-package #:aitools.store.domain)

(defstruct (tx-path (:copier nil))
  (path nil :type string :read-only t)
  (base nil :type entry-state :read-only t)
  (staged nil :type entry-state))

(defstruct (tx-index (:copier nil))
  (last-tx-op 0 :type (integer 0))
  (ops-generation 0 :type (integer 0))
  (paths (make-hash-table :test 'equal) :type hash-table :read-only t))

(defun tx-index-find (index path)
  (values (gethash path (tx-index-paths index))))

(defun tx-index-put (index path base staged)
  (setf (gethash path (tx-index-paths index)) (make-tx-path :path path :base base :staged staged)))

(defun tx-index-remove (index path)
  (remhash path (tx-index-paths index)))

(defun copy-tx-index-deep (index)
  (let ((copy (make-tx-index :last-tx-op (tx-index-last-tx-op index)
                             :ops-generation (tx-index-ops-generation index))))
    (loop for entry being the hash-values of (tx-index-paths index)
          do (tx-index-put copy (tx-path-path entry) (tx-path-base entry) (tx-path-staged entry)))
    copy))

(defun %sorted-tx-paths (index)
  (sort (loop for entry being the hash-values of (tx-index-paths index) collect entry)
        #'string< :key #'tx-path-path))

(defun tx-staged-changed-p (entry)
  "True when ENTRY's staged state differs from its base, so commit must write
it. Unlike ENTRY-STATE-EQUAL, a staged directory mode that differs from the
base's counts: a tx `chmod` of a directory is a real change. So does a
staged file mtime (a tx `touch`) that differs from the base's."
  (let ((base (tx-path-base entry)) (staged (tx-path-staged entry)))
    (or (not (entry-state-equal base staged))
        (and (eq (entry-state-kind staged) :directory)
             (entry-state-mode staged)
             (not (eql (entry-state-mode base) (entry-state-mode staged))))
        (and (eq (entry-state-kind staged) :file)
             (entry-state-mtime staged)
             (not (eql (entry-state-mtime base) (entry-state-mtime staged)))))))

(defun tx-ops-file-name (generation)
  (if (zerop generation) "ops.jsonl" (format nil "ops.~D.jsonl" generation)))

(defun encode-tx-index (index)
  (json-kit:stringify
   (apply #'json-object
          "last_tx_op" (tx-index-last-tx-op index)
          (append
           ;; Absent for generation 0, so a tx that was never rebased keeps
           ;; the index format earlier versions wrote and read.
           (unless (zerop (tx-index-ops-generation index))
             (list "ops_generation" (tx-index-ops-generation index)))
           (list "paths" (mapcar (lambda (entry)
                                   (json-object "path" (tx-path-path entry)
                                                 "base" (entry-state->json (tx-path-base entry))
                                                 "staged" (entry-state->json (tx-path-staged entry))))
                                 (%sorted-tx-paths index)))))))

(defun %json-relative-path (object key)
  (let ((path (%json-field object key :type 'string)))
    (unless (valid-relative-path-p path)
      (%format-error "tx path has the wrong shape"))
    path))

(defun %json-state-field (object key)
  (json->entry-state (%json-field object key :type 'hash-table)))

(defun decode-tx-index (text)
  (let* ((object (%parse-json text))
         (last (%json-field object "last_tx_op" :type '(integer 0)))
         (generation (or (%json-field object "ops_generation" :type '(integer 0) :required nil) 0))
         (index (make-tx-index :last-tx-op last :ops-generation generation)))
    (dolist (entry (%json-list object "paths") index)
      (tx-index-put index (%json-relative-path entry "path")
                    (%json-state-field entry "base")
                    (%json-state-field entry "staged")))))

(defstruct (tx-op-record (:copier nil))
  (tx-op nil :type (integer 1) :read-only t)
  (argv nil :type list :read-only t)
  (paths nil :type list :read-only t)
  ;; ((path . state-or-nil) ...): each path's staged state before this op,
  ;; NIL when the path was not in the index yet. `tx drop` restores these.
  (previous nil :type list :read-only t)
  ;; ((path . state) ...): the staged state this op produced. `tx rebase`
  ;; re-applies these for ops it does not replay.
  (after nil :type list :read-only t)
  (replayable nil :type boolean :read-only t)
  (time nil :type string :read-only t))

(defun %tx-op->json (record)
  (json-object "tx_op" (tx-op-record-tx-op record)
                "argv" (tx-op-record-argv record)
                "paths" (tx-op-record-paths record)
                "previous" (mapcar (lambda (pair)
                                     (json-object "path" (car pair)
                                                   "state" (if (cdr pair)
                                                               (entry-state->json (cdr pair))
                                                               json-kit:+json-null+)))
                                   (tx-op-record-previous record))
                "after" (mapcar (lambda (pair)
                                  (json-object "path" (car pair) "state" (entry-state->json (cdr pair))))
                                (tx-op-record-after record))
                "replayable" (if (tx-op-record-replayable record) t json-kit:+json-false+)
                "time" (tx-op-record-time record)))

(defun %json->tx-op (object)
  (let ((argv (%json-list object "argv"))
        (paths (%json-list object "paths"))
        (replayable (gethash "replayable" object)))
    (unless (and (every #'stringp argv) (every #'valid-relative-path-p paths))
      (%format-error "tx op argv or paths malformed"))
    (unless (or (eq replayable t) (json-kit:json-false-p replayable))
      (%format-error "tx op replayable is not a boolean"))
    (make-tx-op-record
     :tx-op (%json-field object "tx_op" :type '(integer 1))
     :argv argv
     :paths paths
     :previous (mapcar (lambda (pair)
                         (let ((state (%json-field pair "state" :type 'hash-table :required nil)))
                           (cons (%json-relative-path pair "path") (and state (json->entry-state state)))))
                       (%json-list object "previous"))
     :after (mapcar (lambda (pair)
                      (cons (%json-relative-path pair "path") (%json-state-field pair "state")))
                    (%json-list object "after"))
     :replayable (eq replayable t)
     :time (%json-field object "time" :type 'string))))

(defun encode-tx-ops (records)
  (with-output-to-string (out)
    (dolist (record records)
      (write-string (json-kit:stringify (%tx-op->json record)) out)
      (write-char #\Newline out))))

(defun decode-tx-ops (text last-tx-op)
  "Records with tx_op <= LAST-TX-OP, in order. Records must be numbered
1, 2, 3, ... with no gaps; anything else is a corrupt tx."
  (let ((records (loop with start = 0
                       for newline = (position #\Newline text :start start)
                       while newline
                       unless (= start newline)
                         collect (%json->tx-op (%parse-json (subseq text start newline)))
                       do (setf start (1+ newline)))))
    (loop for record in records
          for expected from 1
          unless (= (tx-op-record-tx-op record) expected)
            do (%format-error "tx ops are not numbered consecutively"))
    (when (< (length records) last-tx-op)
      (%format-error "tx index refers to a missing op"))
    (subseq records 0 last-tx-op)))

(defun encode-tx-reads (reads)
  (json-kit:stringify
   (json-object "reads" (mapcar (lambda (pair)
                                   (json-object "path" (car pair) "state" (entry-state->json (cdr pair))))
                                 (sort (copy-list reads) #'string< :key #'car)))))

(defun decode-tx-reads (text)
  (mapcar (lambda (pair)
            (cons (%json-relative-path pair "path") (%json-state-field pair "state")))
          (%json-list (%parse-json text) "reads")))

(defstruct (tx-meta (:copier nil))
  (id nil :type string :read-only t)
  (name nil :type (or null string) :read-only t)
  (created nil :type string :read-only t))

(defun encode-tx-meta (meta)
  (json-kit:stringify
   (json-object "tx" (tx-meta-id meta)
                 "name" (%json-null-or (tx-meta-name meta))
                 "created" (tx-meta-created meta))))

(defun decode-tx-meta (text)
  (let* ((object (%parse-json text))
         (id (%json-field object "tx" :type 'string)))
    (unless (valid-tx-id-p id)
      (%format-error "tx meta id has the wrong shape"))
    (make-tx-meta :id id
                  :name (%json-field object "name" :type 'string :required nil)
                  :created (%json-field object "created" :type 'string))))

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
