;;;; packages/core/store/src/application/write-completion.lisp
;;;;
;;;; The write protocol after the commit point: apply each intent step, append the
;;;; journal entry, remove the tx directory and the intent record, and
;;;; collect blobs. Every step checks whether it already took effect, so
;;;; recovery (recovery.lisp) replays %COMPLETE-INTENT on a committed record.
(in-package #:aitools.store.application)

(defun %apply-step (store step replace-temps)
  "Perform STEP unless it has already taken effect. REPLACE-TEMPS maps a path
to the temp file a later :REPLACE step renames onto it: a move whose source
is rewritten by this same op has happened exactly when that temp is gone or
the source is gone."
  (let ((path (%workspace-path store (intent-step-path step))))
    (flet ((kind (absolute) (%kind-at store absolute)))
      (ecase (intent-step-op step)
        (:mkdir
         (unless (eq (kind path) :directory)
           (%io store mkdir path)))
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
      (loop for step in (intent-steps intent)
            for n from 1
            do (%apply-step store step replace-temps)
               (fault-point :after-apply-step op-id n))
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
