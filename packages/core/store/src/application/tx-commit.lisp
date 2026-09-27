;;;; packages/core/store/src/application/tx-commit.lisp
;;;;
;;;; `tx diff` and `tx commit`. Commit holds the
;;;; workspace lock and the tx lock, checks the write set and the read set
;;;; against the disk, and then runs the whole staged state through the write protocol
;;;; as ONE op whose intent record also names the tx directory: the tx is
;;;; removed during the op's completion, so a crash after the commit point
;;;; can never leave a tx that would then conflict with its own result.
(in-package #:aitools.store.application)

(defun %tx-diff-result (store entry)
  (let* ((path (tx-path-path entry))
         (base (tx-path-base entry))
         (staged (tx-path-staged entry))
         (base-kind (entry-state-kind base))
         (staged-kind (entry-state-kind staged)))
    (flet ((content (state)
             (when (eq (entry-state-kind state) :file)
               (read-blob store (entry-state-hash state)))))
      (make-change-result
       :path path
       :action (cond ((eq staged-kind :absent) :deleted)
                     ((eq staged-kind :symlink) :linked)
                     ((eq base-kind :absent) :created)
                     ((and (eq base-kind staged-kind)
                           (member base-kind '(:file :directory))
                           (equal (entry-state-hash base) (entry-state-hash staged)))
                      (if (and (eq staged-kind :file) (entry-state-mtime staged)) :modified :mode-changed))
                     (t :modified))
       :before base
       :after staged
       :before-content (content base)
       :after-content (content staged)))))

(defun %changed-entries (index)
  (sort (loop for entry being the hash-values of (tx-index-paths index)
              when (tx-staged-changed-p entry) collect entry)
        #'string< :key #'tx-path-path))

(defun tx-diff/k (store tx-id &key on-diff on-not-found)
  "ON-DIFF (results) with a CHANGE-RESULT from `base` to `staged` for
every path the tx changes, sorted by path, carrying both contents for the
caller's diff. ON-NOT-FOUND ()."
  (declare (type function on-diff on-not-found))
  (if (%tx-exists-p store tx-id)
      (funcall on-diff (mapcar (lambda (entry) (%tx-diff-result store entry))
                               (%changed-entries (nth-value 1 (%load-tx store tx-id)))))
      (funcall on-not-found)))

(defun %tx-commit-requests (store index)
  "Requests reproducing INDEX's staged states on a disk that equals `base`:
everything but deletions in path order (parents first), then deletions in
reverse path order (contents before their directory)."
  (let ((writes '()) (deletions '()))
    (dolist (entry (%changed-entries index))
      (let ((path (tx-path-path entry))
            (base (tx-path-base entry))
            (staged (tx-path-staged entry)))
        (ecase (entry-state-kind staged)
          (:absent (push (delete-request path) deletions))
          (:file
           (push (cond ((not (and (eq (entry-state-kind base) :file)
                                (string= (entry-state-hash base) (entry-state-hash staged))))
                        (write-file-request path (read-blob store (entry-state-hash staged))
                                            :mode (entry-state-mode staged) :mtime (entry-state-mtime staged)))
                       ((entry-state-mtime staged)
                        (mtime-request path (entry-state-mtime staged) :mode (entry-state-mode staged)))
                       (t (chmod-request path (entry-state-mode staged))))
                 writes))
          (:directory
           (push (if (eq (entry-state-kind base) :directory)
                     (chmod-request path (entry-state-mode staged))
                     (mkdir-request path))
                 writes))
          (:symlink (push (symlink-request path (entry-state-target staged)) writes)))))
    (append (nreverse writes) deletions)))

(defun tx-commit/k (store tx-id argv &key ignore-stale-reads (lock-timeout-ms +default-lock-timeout-ms+)
                                       on-committed on-rejected on-not-found on-busy)
  "Commit the tx. Continuations, after the locks are released:
  ON-COMMITTED (op-id results)  as for COMMIT-CHANGES/K; the journal holds the
                                whole tx as op OP-ID (NIL for an empty tx,
                                which is simply removed)
  ON-REJECTED (code message &rest keys)
                                refusal.target-changed with :CONFLICTS (the
                                write and read conflicts; nothing written),
                                or environment.io
  ON-NOT-FOUND () / ON-BUSY ()"
  (declare (type function on-committed on-rejected on-not-found on-busy))
  (%run-under-tx-locks
   store tx-id lock-timeout-ms
   (lambda ()
     (multiple-value-bind (meta index ops reads) (%load-tx store tx-id)
       (declare (ignore meta ops))
       (let ((conflicts (tx-commit-conflicts index reads (lambda (path) (workspace-state store path))
                                             :ignore-stale-reads ignore-stale-reads)))
         (if conflicts
             (lambda ()
               (funcall on-rejected "refusal.target-changed"
                        (format nil "~D path~:P changed outside tx ~A" (length conflicts) tx-id)
                        :conflicts conflicts))
             (let ((requests (%tx-commit-requests store index)))
               (if (null requests)
                   (progn (%delete-tx-directory store tx-id)
                          (collect-garbage store)
                          (lambda () (funcall on-committed nil '())))
                   (%plan-and-commit/k store argv requests
                                       :tx-id tx-id
                                       :on-committed (lambda (op-id results)
                                                       (lambda () (funcall on-committed op-id results)))
                                       :on-rejected (lambda (&rest rejection)
                                                      (lambda () (apply on-rejected rejection))))))))))
   on-busy on-not-found))
