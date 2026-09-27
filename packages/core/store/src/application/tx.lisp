;;;; packages/core/store/src/application/tx.lisp
;;;;
;;;; Tx directories (docs/src/reference/transactions.md): `tx/<tx-id>/` holding meta.json, index.json,
;;;; the ops file (ops.jsonl, or ops.<n>.jsonl after a rebase), reads.json
;;;; and the tx lock file. This file covers the lifecycle and read side:
;;;; begin, list/status (drift and stale reads), overlay views, read-set
;;;; recording, abort. Staging, drop and rebase are in tx-stage.lisp; diff
;;;; and commit in tx-commit.lisp.
;;;;
;;;; Every operation that changes tx state holds the workspace lock and then
;;;; the tx lock (always in that order, so two processes can
;;;; never wait on each other). Blob writes and garbage collection therefore
;;;; never race. Recording a read is the exception: it takes only the tx lock
;;;; so that a `read --tx` never waits for a workspace writer, and it touches
;;;; no blobs.
;;;;
;;;; Final continuations run after both locks are released: each locked body
;;;; returns a thunk that %RUN-UNDER-TX-LOCKS calls on the way out.
(in-package #:aitools.store.application)

(defun %tx-file (store tx-id name)
  (join-path (tx-directory (store-state-directory store) tx-id) name))

(defun %run-under-tx-locks (store tx-id timeout-ms body on-busy on-not-found &key (workspace t))
  (declare (type function body on-busy on-not-found))
  (flet ((under-tx-lock ()
           (call-with-tx-lock/k store tx-id timeout-ms
                                :on-acquired body
                                :on-timeout (lambda () on-busy)
                                :on-not-found (lambda () on-not-found))))
    (funcall (if workspace
                 (call-with-workspace-lock/k store timeout-ms
                                             :on-acquired #'under-tx-lock
                                             :on-timeout (lambda () on-busy))
                 (under-tx-lock)))))

(defun %load-tx (store tx-id)
  "(values meta index ops reads) of an existing tx."
  (let ((index (decode-tx-index (%read-text-if-exists store (%tx-file store tx-id "index.json")))))
    (values (decode-tx-meta (%read-text-if-exists store (%tx-file store tx-id "meta.json")))
            index
            (decode-tx-ops (or (%read-text-if-exists
                                store (%tx-file store tx-id (tx-ops-file-name (tx-index-ops-generation index))))
                               "")
                           (tx-index-last-tx-op index))
            (decode-tx-reads (%read-text-if-exists store (%tx-file store tx-id "reads.json"))))))

(defun %tx-exists-p (store tx-id)
  (and (valid-tx-id-p tx-id)
       (eq (%kind-at store (%tx-file store tx-id "index.json")) :file)))

(defstruct (tx-status (:copier nil))
  (id nil :type string :read-only t)
  (name nil :type (or null string) :read-only t)
  (created nil :type string :read-only t)
  ;; TX-OP-RECORDs, oldest first.
  (ops nil :type list :read-only t)
  ;; TX-PATHs sorted by path.
  (paths nil :type list :read-only t)
  ;; Paths: write-set paths whose disk state left `base`, and recorded reads
  ;; whose disk state left the record.
  (drift nil :type list :read-only t)
  (stale-reads nil :type list :read-only t))

(defun %tx-status (store tx-id)
  (multiple-value-bind (meta index ops reads) (%load-tx store tx-id)
    (flet ((disk (path) (workspace-state store path)))
      (make-tx-status :id tx-id
                      :name (tx-meta-name meta)
                      :created (tx-meta-created meta)
                      :ops ops
                      :paths (sort (loop for entry being the hash-values of (tx-index-paths index) collect entry)
                                   #'string< :key #'tx-path-path)
                      :drift (tx-drift-paths index #'disk)
                      :stale-reads (loop for (path . recorded) in (sort (copy-list reads) #'string< :key #'car)
                                         unless (entry-state-equal recorded (disk path))
                                           collect path)))))

(defun tx-begin/k (store &key name (lock-timeout-ms +default-lock-timeout-ms+) on-begun on-busy)
  "Begin a tx. ON-BEGUN (tx-id name created) or ON-BUSY ()."
  (declare (type function on-begun on-busy))
  (funcall
   (call-with-workspace-lock/k
    store lock-timeout-ms
    :on-acquired
    (lambda ()
      (let* ((now (%io store now))
             (tx-id (format-tx-id now (%io store random-hex 8)))
             (created (aitools.kernel.domain:iso8601-utc now))
             (root (tx-root-directory (store-state-directory store)))
             (staging-path (join-path root (format nil ".~A.creating" tx-id))))
        (with-temp-dir (staging store staging-path)
          (flet ((put (file text) (%io store create-file (join-path staging file) (string-octets text))))
            (put "meta.json" (encode-tx-meta (make-tx-meta :id tx-id :name name :created created)))
            (put "ops.jsonl" "")
            (put "reads.json" (encode-tx-reads '()))
            (put "lock" "")
            (put "index.json" (encode-tx-index (make-tx-index))))
          (%io store rename staging (join-path root tx-id)))
        (lambda () (funcall on-begun tx-id name created))))
    :on-timeout (lambda () on-busy))))

(defun tx-list (store)
  "`tx status` without an argument: a TX-STATUS for every open tx, by id. A tx
removed while being read is skipped."
  (loop for name in (sort (copy-list (%io store list-directory (tx-root-directory (store-state-directory store))))
                          #'string<)
        for status = (and (%tx-exists-p store name)
                          (handler-case (%tx-status store name)
                            (store-io-error () nil)))
        when status collect status))

(defun tx-status/k (store tx-id &key on-status on-not-found)
  "`tx status` with a tx: ON-STATUS (tx-status) or ON-NOT-FOUND ()."
  (declare (type function on-status on-not-found))
  (if (%tx-exists-p store tx-id)
      (funcall on-status (%tx-status store tx-id))
      (funcall on-not-found)))

(defun call-with-tx-view/k (store tx-id &key on-view on-not-found)
  "ON-VIEW (view) with a STORE-VIEW of TX-ID's state over the disk (the
tx read-through), or ON-NOT-FOUND (). Takes no lock: like any read, the view
may observe a concurrent tx op's before or after state, never a mix,
because each tx op replaces index.json with one rename."
  (declare (type function on-view on-not-found))
  (if (%tx-exists-p store tx-id)
      (funcall on-view (%make-store-view store (nth-value 1 (%load-tx store tx-id))))
      (funcall on-not-found)))

(defun tx-record-read/k (store tx-id path &key (lock-timeout-ms +default-lock-timeout-ms+)
                                            on-recorded on-not-found on-busy)
  "The tx read set: record PATH's current disk state in reads.json, replacing
an earlier record for the same path. ON-RECORDED (entry-state)."
  (declare (type function on-recorded on-not-found on-busy))
  (unless (valid-relative-path-p path)
    (error "tx-record-read/k: ~S is not a workspace-relative path" path))
  (%run-under-tx-locks
   store tx-id lock-timeout-ms
   (lambda ()
     (let* ((state (workspace-state store path))
            (reads (decode-tx-reads (%read-text-if-exists store (%tx-file store tx-id "reads.json"))))
            (updated (acons path state (remove path reads :key #'car :test #'string=))))
       (%replace-file store (%tx-file store tx-id "reads.json") (encode-tx-reads updated))
       (lambda () (funcall on-recorded state))))
   on-busy on-not-found
   :workspace nil))

(defun tx-abort/k (store tx-id &key (lock-timeout-ms +default-lock-timeout-ms+) on-aborted on-not-found on-busy)
  "Delete the tx without touching the working tree. ON-ABORTED
(paths) with the discarded write-set paths, sorted."
  (declare (type function on-aborted on-not-found on-busy))
  (%run-under-tx-locks
   store tx-id lock-timeout-ms
   (lambda ()
     (let ((paths (sort (loop for entry being the hash-values of (tx-index-paths (nth-value 1 (%load-tx store tx-id)))
                              collect (tx-path-path entry))
                        #'string<)))
       (%delete-tx-directory store tx-id)
       (collect-garbage store)
       (lambda () (funcall on-aborted paths))))
   on-busy on-not-found))
