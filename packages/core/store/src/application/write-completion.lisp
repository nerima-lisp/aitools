;;;;
;;;; The write protocol after the commit point: apply each intent step, append the
;;;; journal entry, remove the tx directory and the intent record, and
;;;; collect blobs. Every step checks whether it already took effect, so
;;;; recovery (recovery.lisp) replays %COMPLETE-INTENT on a committed record.
(in-package #:aitools.store.application)

(defun %refuse-changed-deletion (store step &key allow-absent)
  (when (and (member (intent-step-op step) '(:unlink :rmdir))
             (intent-step-before step))
    (let* ((path (%workspace-path store (intent-step-path step)))
           (identity (nth-value 5 (%io store lstat path)))
           (current (%state-at store path :max-bytes (intent-step-max-bytes step))))
      ;; Absence is the idempotent replay case. The check and unlink are not
      ;; atomic: callers must exclude concurrent uncooperative writers.
      (unless (and allow-absent (entry-state-absent-p current))
        (unless (and (entry-state-equal current (intent-step-before step))
                     (or (null (intent-step-identity step))
                         (equal identity (intent-step-identity step)))
                     (equal identity (nth-value 5 (%io store lstat path))))
          (error 'store-io-error :operation "delete" :path path :detail "deletion target changed"))))))

(defun %apply-step (store step replace-temps)
  "Perform STEP unless it has already taken effect. REPLACE-TEMPS maps a path
to the temp file a later :REPLACE step renames onto it: a move whose source
is rewritten by this same op has happened exactly when that temp is gone or
the source is gone."
  (%refuse-changed-deletion store step :allow-absent t)
  (let ((path (%workspace-path store (intent-step-path step))))
    (flet ((kind (absolute) (%kind-at store absolute)))
      (ecase (intent-step-op step)
        (:mkdir
         (if (intent-step-identity step)
             (%check-directory-identity store step)
             (progn
               ;; An old record cannot prove that an existing directory is ours.
               (unless (eq (kind path) :absent)
                 (error 'store-io-error :operation "mkdir" :path path
                                        :detail "directory identity unavailable"))
               (if (intent-step-mode step)
                   (%io store mkdir path :mode #o700)
                   (%io store mkdir path)))))
        (:move
         (let* ((from (%workspace-path store (intent-step-from step)))
                (successor-temp (gethash (intent-step-from step) replace-temps)))
           (when (and (not (eq (kind from) :absent))
                      (or (null successor-temp)
                          (not (eq (kind (%workspace-path store successor-temp)) :absent))))
             (%io store rename from path))))
        (:replace
         (let ((temp (%workspace-path store (intent-step-temp step))))
           (unless (eq (kind temp) :absent)
             (%io store rename temp path))))
        (:unlink
         (unless (eq (kind path) :absent)
           (%io store unlink path)))
        (:rmdir
         (when (eq (kind path) :directory)
           (%io store rmdir path)))
        (:chmod
         (unless (eq (kind path) :absent)
           (%io store chmod path (intent-step-mode step))))
        (:utime
         (when (eq (kind path) :file)
           (%io store chmod path (intent-step-mode step))
           (%io store set-mtime path (intent-step-mtime step))))))))

(defun %finish-directory-modes (store steps)
  ;; Children first: a parent's final mode can remove traversal or write access.
  (dolist (step (reverse steps))
    (when (and (eq (intent-step-op step) :mkdir) (intent-step-mode step))
      (%io store chmod (%workspace-path store (intent-step-path step)) (intent-step-mode step)))))

(defun %delete-tx-directory (store tx-id)
  "Rename the tx directory out of the way before deleting it, so a crash
mid-delete leaves a dot-named leftover (collected later) rather than a tx
with half its files."
  (let ((directory (tx-directory (store-state-directory store) tx-id)))
    (unless (eq (%kind-at store directory) :absent)
      (let ((doomed (join-path (tx-root-directory (store-state-directory store))
                               (format nil ".~A.~A.deleting" tx-id (%io store random-hex 8)))))
        (%io store rename directory doomed)
        (%delete-tree store doomed)))))

(defun %replace-temps (steps)
  (let ((replace-temps (make-hash-table :test 'equal)))
    (dolist (step steps replace-temps)
      (when (eq (intent-step-op step) :replace)
        (setf (gethash (intent-step-path step) replace-temps) (intent-step-temp step))))))

(defun %directory-checkpoint-p (intent)
  (every (lambda (step)
           (and (eq (intent-step-op step) :chmod) (intent-step-identity step)))
         (intent-steps intent)))

(defun %check-directory-identity (store step)
  (let ((path (%workspace-path store (intent-step-path step))))
    (multiple-value-bind (kind mode target mtime links identity) (%io store lstat path)
      (declare (ignore target mtime links))
      (unless (and (eq kind :directory) (equal identity (intent-step-identity step)))
        (error 'store-io-error :operation "finalize-directory" :path path
                               :detail "directory identity changed"))
      mode)))

(defun %checkpoint-intent (store intent steps)
  ;; Preserve the full original journal and tx even as the replay suffix shrinks.
  (let* ((next (make-intent :op-id (intent-op-id intent) :steps steps
                            :journal-entry (intent-journal-entry intent) :tx-id (intent-tx-id intent)))
         (header (encode-intent-header next))
         (path (intent-file-path (store-state-directory store) (intent-op-id intent)))
         (temp (concatenate 'string path "." (%io store random-hex 8) ".checkpoint"))
         (sync (store-io-sync-directory (store-io-port store))))
    (unless sync
      (error 'store-io-error :operation "checkpoint" :path path :detail "directory sync unavailable"))
    (with-temp-file (created store temp
                            (string-octets (format nil "~A~%~A~%" header (encode-intent-checksum header))) :sync t)
      (%io store rename created path)
      (funcall sync (commit-directory (store-state-directory store))))
    (fault-point :after-intent-checkpoint (intent-op-id intent))
    next))

(defun %directory-finalization-steps (store intent)
  (loop for step in (reverse (intent-steps intent))
        when (and (eq (intent-step-op step) :mkdir) (intent-step-mode step))
          collect (let* ((path (%workspace-path store (intent-step-path step)))
                         (identity (intent-step-identity step)))
                    (unless identity
                      (error 'store-io-error :operation "finalize-directory" :path path
                                             :detail "directory identity unavailable"))
                    (%check-directory-identity store step)
                    (make-intent-step :op :chmod :path (intent-step-path step)
                                      :mode (intent-step-mode step) :identity identity))))

(defun %checkpoint-created-directory (store intent step)
  (let ((path (%workspace-path store (intent-step-path step))))
    (multiple-value-bind (kind mode target mtime links identity) (%io store lstat path)
      (declare (ignore mode target mtime links))
      (unless (and (eq kind :directory) identity)
        (error 'store-io-error :operation "mkdir" :path path
                               :detail "directory identity unavailable"))
      ;; This observation is not atomic with mkdir. Uncooperative concurrent
      ;; writers must still be excluded during the operation.
      (%checkpoint-intent
       store intent
       (substitute (make-intent-step :op :mkdir :path (intent-step-path step)
                                     :mode (intent-step-mode step) :identity identity)
                   step (intent-steps intent) :test #'eq)))))

(defun %complete-directory-checkpoint (store intent)
  (loop while (intent-steps intent)
        for step = (first (intent-steps intent))
        do (%check-directory-identity store step)
           ;; A matching mode does not prove that the preceding fsync succeeded.
           (let ((chmod (store-io-chmod-directory-identity (store-io-port store)))
                   (path (%workspace-path store (intent-step-path step))))
               (unless chmod
                 (error 'store-io-error :operation "finalize-directory" :path path
                                        :detail "identity-checked chmod unavailable"))
               (funcall chmod path (intent-step-mode step) (intent-step-identity step)))
           (fault-point :after-directory-mode (intent-op-id intent))
           ;; Persist removal of each child before a parent loses search permission.
           (setf intent (%checkpoint-intent store intent (rest (intent-steps intent))))))

(defun %complete-intent (store intent &key recovered)
  "Everything after the commit point, idempotent: apply, journal, tx
cleanup, intent removal, garbage collection. An I/O failure before the
intent is removed is signalled as STORE-COMMITTED-ERROR: the op is decided
and the record stays for recovery. RECOVERED marks a recovery replay, whose
journal step rewrites the file (idempotent by op id, and it heals a torn
tail) rather than appending."
  (let* ((op-id (intent-op-id intent))
         (replace-temps (%replace-temps (intent-steps intent))))
    (handler-bind ((store-io-error
                     (lambda (condition)
                       (unless (typep condition 'store-committed-error)
                         (error 'store-committed-error
                                :op-id op-id
                                :operation (store-io-error-operation condition)
                                :path (store-io-error-path condition)
                                :errno (store-io-error-errno condition)
                                :detail (store-io-error-detail condition))))))
      (if (%directory-checkpoint-p intent)
          (%complete-directory-checkpoint store intent)
          (progn
            (loop for step in (intent-steps intent)
                  for n from 1
                  do (%apply-step store step replace-temps)
                     (when (and (eq (intent-step-op step) :mkdir)
                                (null (intent-step-identity step)))
                       (setf intent (%checkpoint-created-directory store intent step)))
                     (fault-point :after-apply-step op-id n))
            (let ((steps (%directory-finalization-steps store intent)))
              (when steps
                (%complete-directory-checkpoint store (%checkpoint-intent store intent steps))))))
      (fault-point :after-apply op-id)
      (%append-journal-entry store (intent-journal-entry intent) :recovered recovered)
      (fault-point :after-journal op-id)
      (when (intent-tx-id intent)
        (%delete-tx-directory store (intent-tx-id intent))
        (fault-point :after-tx-cleanup op-id))
      (%io store unlink (intent-file-path (store-state-directory store) op-id)))
    (fault-point :after-intent-removal op-id)
    ;; The op is complete once its record is gone. Blobs a failed collection
    ;; leaves behind are collected by the next write.
    (handler-case (%reap-journal store)
      (store-io-error () nil))))
