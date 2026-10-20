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
          (:complete (%recover-complete-intent store path op-id intent))
          (:incomplete (%discard-incomplete-intent store path op-id intent)))))))
(defun %recover-complete-intent (store path op-id intent)
  ;; A replaced checkpoint target is an I/O failure, not an invalid
  ;; intent to discard. Keep the durable record for operator recovery.
  (dolist (step (intent-steps intent))
    (when (and (member (intent-step-op step) '(:mkdir :chmod))
               (intent-step-identity step))
      (%check-directory-identity store step)))
  (if (%intent-violation store intent)
      (progn
        (%io store unlink path)
        (cons op-id "discarded"))
      (progn
        (%complete-intent store intent :recovered t)
        (cons op-id "rolled-forward"))))
(defun %discard-incomplete-step (store op-id step)
  (let ((temp (intent-step-temp step)))
    (when (and temp (null (%path-violation store temp)))
      (let ((temp-path (%workspace-path store temp)))
        (unless (eq (%kind-at store temp-path) :absent)
          (%io store unlink temp-path)))
      (fault-point :recovery-after-discard op-id))))
(defun %discard-incomplete-intent (store path op-id intent)
  (when intent
    (dolist (step (intent-steps intent))
      (%discard-incomplete-step store op-id step)))
  (%io store unlink path)
  (cons op-id "discarded"))
(aitools.kernel.domain:define-flow
    (%notify-recovery-entry/k entry &key on-rolled-forward on-discarded)
  (declare (type function on-rolled-forward on-discarded))
  (if (string= (cdr entry) "rolled-forward")
      (aitools.kernel.domain:flow-continue :on-rolled-forward (car entry))
      (aitools.kernel.domain:flow-continue :on-discarded (car entry))))

(aitools.kernel.domain:define-flow
    (recover/k store &key (lock-timeout-ms +default-lock-timeout-ms+)
               on-rolled-forward on-discarded on-none on-busy
               (on-failed (lambda (condition entries)
                            (declare (ignore entries))
                            (error condition))))
  "Recover unfinished write-protocol intents.

Call ON-ROLLED-FORWARD (op-id) or ON-DISCARDED (op-id) after releasing the
workspace lock for each recovered entry. On success, return the (op-id . action)
list. Call ON-NONE () when no entries remain, without acquiring the lock if the
initial scan is empty or after releasing it if the locked scan is empty. Call
ON-BUSY () if acquisition times out without taking the lock. ON-FAILED
(condition entries) receives the committed error and entries recovered before
failure after lock release; the default signals that condition."
  (declare (type function on-rolled-forward on-discarded on-none on-busy on-failed))
  (if (null (%intent-names store))
      (aitools.kernel.domain:flow-continue :on-none)
      (let ((entries '())
            (failure nil))
        (let ((lock-result
                (aitools.kernel.domain:flow-call
                    (call-with-workspace-lock/k store lock-timeout-ms)
                  (:on-acquired ()
                    (dolist (name (%intent-names store))
                      (handler-case (push (%recover-intent store name) entries)
                        (store-io-error (condition)
                          (setf failure (%as-committed-error condition name))
                          (return))))
                    :acquired)
                  (:on-timeout ()
                    :timeout))))
          (if (eq lock-result :timeout)
              (aitools.kernel.domain:flow-continue :on-busy)
              (progn
                (setf entries (nreverse entries))
                (dolist (entry entries)
                  (aitools.kernel.domain:flow-call
                      (%notify-recovery-entry/k entry)
                    (:on-rolled-forward (op-id)
                      (aitools.kernel.domain:flow-continue :on-rolled-forward op-id))
                    (:on-discarded (op-id)
                      (aitools.kernel.domain:flow-continue :on-discarded op-id))))
                (cond
                  (failure
                   (aitools.kernel.domain:flow-continue :on-failed failure entries))
                  ((null entries)
                   (aitools.kernel.domain:flow-continue :on-none))
                  (t entries))))))))

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
