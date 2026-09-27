;;;; packages/core/store/src/application/recovery.lisp
;;;;
;;;; Write-protocol recovery, run by the composition root before every command.
;;;; The common case costs one readdir of `commit/`. Otherwise it takes the
;;;; workspace lock (a live writer holds it for its whole op, so any record
;;;; still present once the lock is ours belongs to a dead process), then:
;;;;
;;;;   complete record   -> %COMPLETE-INTENT, the same idempotent code the
;;;;                        writer runs after its commit point: rolled-forward
;;;;   incomplete record -> delete the temp files line 1 names, then the
;;;;                        record; the workspace was never touched: discarded
;;;;   complete record naming .git, the state directory, or a path behind a
;;;;   symlink out of the root -> the record only: discarded (this store
;;;;                        would never have written it)
;;;;
;;;; A crash during recovery leaves either the record (and the next run
;;;; repeats the same idempotent work) or nothing.
(in-package #:aitools.store.application)

(defun %intent-names (store)
  (sort (remove-if-not (lambda (name)
                         (let ((length (length name)))
                           (and (> length 5) (string= ".json" name :start2 (- length 5)))))
                       (%io store list-directory (commit-directory (store-state-directory store))))
        #'string<))

(defun %recover-intent (store name)
  "Recover the record NAME in `commit/`; returns (op-id . action). A complete
record naming a path the store must not write (%INTENT-VIOLATION: it was not
written by this store, e.g. forged in a state directory an agent could
reach) is discarded without touching the workspace."
  (let* ((path (join-path (commit-directory (store-state-directory store)) name))
         (text (handler-case (octets-string (%io store read-file path))
                 ;; A torn multi-byte sequence can only be in an unfinished
                 ;; line, which DECODE-INTENT already treats as incomplete.
                 (sb-int:character-decoding-error () ""))))
    (multiple-value-bind (status intent) (decode-intent text)
      (let ((op-id (if intent (intent-op-id intent) (subseq name 0 (- (length name) 5)))))
        (ecase status
          (:complete
           (if (%intent-violation store intent)
               (progn
                 (%io store unlink path)
                 (cons op-id "discarded"))
               (progn
                 (%complete-intent store intent :recovered t)
                 (cons op-id "rolled-forward"))))
          (:incomplete
           (when intent
             (dolist (step (intent-steps intent))
               (let ((temp (intent-step-temp step)))
                 (when (and temp (null (%path-violation store temp)))
                   (let ((temp (%workspace-path store temp)))
                     (unless (eq (%kind-at store temp) :absent)
                       (%io store unlink temp)))
                   (fault-point :recovery-after-discard op-id)))))
           (%io store unlink path)
           (cons op-id "discarded")))))))

(defun recover/k (store &key (lock-timeout-ms +default-lock-timeout-ms+)
                          on-rolled-forward on-discarded on-none on-busy on-failed)
  "Recover unfinished write-protocol operations before a command runs.

When `commit/` is empty (or absent), calls ON-NONE () and returns its value.
Otherwise acquires the workspace lock, recovers every record, releases the
lock, then calls ON-ROLLED-FORWARD (op-id) or ON-DISCARDED (op-id) once per
recovered op in op-id order and returns the list of (op-id . action)
conses, action being \"rolled-forward\" or \"discarded\" -- the envelope's
`recovered[]` elements. If another writer finished the
record while this process waited, the outcome is ON-NONE. ON-BUSY () when
the lock could not be acquired within LOCK-TIMEOUT-MS.

An I/O failure while recovering a record stops recovery there: that record
and every later one stay in `commit/` (a later record may depend on the
failed one's paths). The records recovered before it are reported through
ON-ROLLED-FORWARD and ON-DISCARDED as usual, and then ON-FAILED (condition
entries) is called and its value returned, CONDITION being a
STORE-COMMITTED-ERROR whose op id, operation and path name the failure and
ENTRIES the (op-id . action) list so far. Without ON-FAILED the condition is
signalled instead, after the lock is released."
  (declare (type function on-rolled-forward on-discarded on-none on-busy)
           (type (or null function) on-failed))
  (unless (%intent-names store)
    (return-from recover/k (funcall on-none)))
  (let ((entries '())
        (failure nil))
    (call-with-workspace-lock/k store lock-timeout-ms
                                :on-acquired (lambda ()
                                               (dolist (name (%intent-names store))
                                                 (handler-case (push (%recover-intent store name) entries)
                                                   (store-io-error (condition)
                                                     (setf failure (%as-committed-error condition name))
                                                     (return)))))
                                :on-timeout (lambda () (return-from recover/k (funcall on-busy))))
    (setf entries (nreverse entries))
    (dolist (entry entries)
      (funcall (if (string= (cdr entry) "rolled-forward") on-rolled-forward on-discarded)
               (car entry)))
    (cond
      (failure (if on-failed (funcall on-failed failure entries) (error failure)))
      ((null entries) (funcall on-none))
      (t entries))))

(defun %as-committed-error (condition name)
  "CONDITION as the STORE-COMMITTED-ERROR of the record NAME: a failure to
discard a record is reported the same way as a failure to roll one forward."
  (if (typep condition 'store-committed-error)
      condition
      (make-condition 'store-committed-error
                      :op-id (subseq name 0 (- (length name) 5))
                      :operation (store-io-error-operation condition)
                      :path (store-io-error-path condition)
                      :errno (store-io-error-errno condition)
                      :detail (store-io-error-detail condition))))
