;;;; packages/core/store/src/application/write-protocol.lisp
;;;;
;;;; The write protocol: lock -> validate -> prepare -> intent -> apply -> complete.
;;;;
;;;;   prepare   before-content blobs (fsync), intent line 1 (fsync), temp
;;;;             files `.aitools-<op>-<n>.tmp` (fsync; symlinks are created
;;;;             directly, a symlink has no data to sync)
;;;;   intent    intent line 2, the checksum (fsync) = the commit point
;;;;   apply     %APPLY-STEP for each step, in the record's order
;;;;   complete  journal append, tx directory removal, intent removal, blob GC
;;;;
;;;; An ERROR before the commit point discards what prepare created and the
;;;; workspace is untouched. After the commit point nothing is undone: the
;;;; next start's recovery rolls the record forward (recovery.lisp), with the
;;;; same %COMPLETE-INTENT used here, so every step is idempotent, and an
;;;; I/O failure there is signalled as STORE-COMMITTED-ERROR, not rejected.
;;;;
;;;; FAULT-POINT names, in order: :after-lock, :after-validate, :after-blobs,
;;;; :after-intent-header, :after-temp (n), :after-prepare, :after-intent,
;;;; :after-apply-step (n), :after-apply, :after-journal, :after-tx-cleanup,
;;;; :after-intent-removal. Each receives the op id as its first detail
;;;; (except :after-lock).
;;;;
;;;; Prepare is in write-preparation.lisp, apply and complete in
;;;; write-completion.lisp; this file sequences them for one op.
(in-package #:aitools.store.application)

(defun %new-id (store formatter)
  (funcall formatter (%io store now) (%io store random-hex 8)))

(defun %disk-plan/k (store requests on-planned on-rejected)
  (plan-changes/k requests
                  :lookup-state (lambda (path) (workspace-state store path))
                  :lookup-content (lambda (path) (%io store read-file (%workspace-path store path)))
                  :list-children (lambda (path) (%list-children store path))
                  :lookup-mtime (lambda (path) (workspace-mtime store path))
                  :on-planned on-planned
                  :on-rejected on-rejected))

(defun %strip-contents (result)
  (make-change-result :path (change-result-path result)
                      :action (change-result-action result)
                      :from (change-result-from result)
                      :before (change-result-before result)
                      :after (change-result-after result)
                      :source-before (change-result-source-before result)))

(defun %commit-results (store argv results &key undoes tx-id)
  "Run prepare, intent, apply, and complete for planned RESULTS (non-empty)
with the workspace lock held. Returns the new op id."
  (let* ((state (store-state-directory store))
         (op-id (%new-id store #'format-op-id))
         (entry (make-journal-entry :op-id op-id :argv argv
                                    :time (aitools.kernel.domain:iso8601-utc (%io store now))
                                    :changes (mapcar #'%strip-contents results)
                                    :undoes undoes))
         (steps (changes->steps op-id results
                                (lambda (path) (eq (%kind-at store (%workspace-path store path)) :directory))))
         (intent (make-intent :op-id op-id :steps steps :journal-entry entry :tx-id tx-id))
         (intent-path (intent-file-path state op-id))
         (header (encode-intent-header intent))
         (by-path (make-hash-table :test 'equal)))
    (dolist (result results)
      (setf (gethash (change-result-path result) by-path) result))
    (fault-point :after-validate op-id)
    (%refuse-unsafe-steps store steps)
    (when (store-temporary store)
      ;; mktemp's area is scratch space, outside the journal. Without
      ;; a record there is nothing to roll forward, so a crash mid-apply can
      ;; leave some of the op's paths changed. Only the prepared temp files are
      ;; a resource to unwind, so the apply that follows runs unguarded.
      (with-prepared-intent (store nil steps)
        (%prepare-temps store op-id steps by-path))
      (let ((replace-temps (%replace-temps steps)))
        (dolist (step steps)
          (%apply-step store step replace-temps)))
      (return-from %commit-results op-id))
    (with-prepared-intent (store intent-path steps)
      (dolist (result results)
        ;; A mode change keeps the content, so it has no before-content to keep.
        (when (change-result-before-content result)
          (write-blob store (change-result-before-content result))))
      (fault-point :after-blobs op-id)
      (%io store create-file intent-path (string-octets (format nil "~A~%" header)) :sync t)
      (fault-point :after-intent-header op-id)
      (%prepare-temps store op-id steps by-path)
      (fault-point :after-prepare op-id)
      ;; Writing line 2, the checksum, durably is the commit point. Once
      ;; WITH-PREPARED-INTENT returns the op is committed and its record must
      ;; survive, so %COMPLETE-INTENT runs outside the discard guard.
      (%io store append-file intent-path (string-octets (format nil "~A~%" (encode-intent-checksum header)))
           :sync t))
    (fault-point :after-intent op-id)
    (%complete-intent store intent)
    op-id))

(defun %io-rejection (condition)
  (list "environment.io" (princ-to-string condition)))

(defun %plan-and-commit/k (store argv requests &key undoes tx-id on-committed on-rejected)
  "Plan REQUESTS against the disk and commit them; the workspace lock must be
held. ON-COMMITTED (op-id results) with OP-ID NIL when nothing changed. A
STORE-COMMITTED-ERROR is not a rejection and propagates to the caller."
  (declare (type function on-committed on-rejected))
  (%disk-plan/k store requests
                (lambda (results)
                  (if (null results)
                      (funcall on-committed nil '())
                      (let ((op-id (handler-case (%commit-results store argv results :undoes undoes :tx-id tx-id)
                                     (store-refusal (condition)
                                       (return-from %plan-and-commit/k
                                         (funcall on-rejected (store-refusal-code condition)
                                                  (store-refusal-message condition))))
                                     ((and store-io-error (not store-committed-error)) (condition)
                                       (return-from %plan-and-commit/k
                                         (apply on-rejected (%io-rejection condition)))))))
                        (funcall on-committed op-id results))))
                on-rejected))

(defun commit-changes/k (store argv validate
                         &key (lock-timeout-ms +default-lock-timeout-ms+) dry-run undoes
                           on-committed on-rejected on-busy)
  "The write protocol for one write command.

VALIDATE is called as (VALIDATE COMMIT REJECT) after the workspace lock is
held (validating before the lock would race). It reads the
targets itself, checks selectors, guards, the boundary, UTF-8 and the
redaction placeholder, and then calls exactly one of:
  (COMMIT requests)                 a list of CHANGE-REQUEST
  (REJECT code message &rest keys)  keys as for ON-REJECTED

Exactly one continuation is then called, after the lock is released:
  ON-COMMITTED (op-id results)  RESULTS is a list of CHANGE-RESULT carrying
                                before/after states and bytes for the result's
                                `changes` and `diff`. OP-ID is NIL for
                                DRY-RUN and when nothing changed.
  ON-REJECTED (code message &key candidates diagnostics conflicts repairs)
                                validation failed, the planner refused, or
                                an I/O error occurred (`environment.io`).
  ON-BUSY ()                    the lock was not acquired within
                                LOCK-TIMEOUT-MS (`environment.busy`).

An I/O failure after the commit point is neither: STORE-COMMITTED-ERROR is
signalled (the lock released), naming the op and the failing path.

DRY-RUN validates and plans without the lock and writes nothing. UNDOES is
recorded in the journal entry as `undoes`; ARGV is the command's
argv, recorded as given."
  (declare (type function validate on-committed on-rejected on-busy))
  (let ((outcome nil))
    (flet ((run ()
             (fault-point :after-lock)
             (funcall validate
                      (lambda (requests)
                        (setf outcome
                              (if dry-run
                                  (%disk-plan/k store requests
                                                (lambda (results) (list :committed nil results))
                                                (lambda (&rest rejection) (list* :rejected rejection)))
                                  (%plan-and-commit/k store argv requests
                                                      :undoes undoes
                                                      :on-committed (lambda (op-id results)
                                                                      (list :committed op-id results))
                                                      :on-rejected (lambda (&rest rejection)
                                                                     (list* :rejected rejection))))))
                      (lambda (&rest rejection)
                        (setf outcome (list* :rejected rejection))))))
      (if dry-run
          (run)
          (call-with-workspace-lock/k store lock-timeout-ms
                                      :on-acquired #'run
                                      :on-timeout (lambda () (setf outcome (list :busy))))))
    (unless outcome
      (error "commit-changes/k: VALIDATE returned without calling COMMIT or REJECT"))
    (ecase (first outcome)
      (:committed (funcall on-committed (second outcome) (third outcome)))
      (:rejected (apply on-rejected (rest outcome)))
      (:busy (funcall on-busy)))))
