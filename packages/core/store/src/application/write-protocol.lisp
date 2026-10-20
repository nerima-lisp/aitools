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
;;;; Fault-injection points include :after-lock, :after-validate, :after-blobs,
;;;; :after-intent-header, :after-temp (n), :after-prepare, :after-intent,
;;;; :after-intent-checkpoint (repeatable), :after-apply-step (n),
;;;; :after-directory-mode, :after-apply, :after-journal, :after-tx-cleanup,
;;;; and :after-intent-removal. Each receives the op id as its first detail,
;;;; except :after-lock.
;;;;
;;;; Prepare is in write-preparation.lisp, apply and complete in
;;;; write-completion.lisp; this file sequences them for one op.
(in-package #:aitools.store.application)

(defmacro %flow-continue-rejection (rejection)
  (let ((record (gensym "REJECTION")) (keys (gensym "KEYS")) (mask (gensym "MASK")))
    `(let* ((,record ,rejection) (,keys (cddr ,record))
            (,mask (loop with mask = 0 for (key value) on ,keys by #'cddr
                         for bit = (position key
                                   (list :candidates :diagnostics :conflicts
                                         :repairs :path)
                                   :test (function eq))
                         do (unless bit (error "Unsupported rejection field ~S." key))
                            (setf mask (logior mask (ash 1 bit))) finally (return mask))))
       (case ,mask
         ,@(loop for value from 0 below 32 collect
                 `(,value
                  (aitools.kernel.domain:flow-continue
                   :on-rejected
                   (first ,record)
                   (second ,record)
                   ,@(loop for key in (list :candidates :diagnostics :conflicts :repairs :path)
                           for bit from 0
                           when (logbitp bit value)
                             append (list key `(getf ,keys ,key))))))))))
(defun %rejection-keywords
    (&key candidates candidates-p
          diagnostics diagnostics-p
          conflicts conflicts-p
          repairs repairs-p
          path path-p)
  (append (when candidates-p (list :candidates candidates))
          (when diagnostics-p (list :diagnostics diagnostics))
          (when conflicts-p (list :conflicts conflicts))
          (when repairs-p (list :repairs repairs))
          (when path-p (list :path path))))

(aitools.kernel.domain:define-flow
    (commit-changes/k store argv validate
                      &key (lock-timeout-ms +default-lock-timeout-ms+)
                        dry-run undoes on-committed on-rejected on-busy)
  "The write protocol for one write command.

For a normal write, VALIDATE is called as (VALIDATE COMMIT REJECT) after the
workspace lock is held (validating before the lock would race). It reads the targets itself,
checks selectors, guards, the boundary, UTF-8 and the redaction placeholder,
and then calls exactly one of:
  (COMMIT requests)                 a list of CHANGE-REQUEST
  (REJECT code message &rest keys)  keys as for ON-REJECTED

Exactly one continuation is then called, after the lock is released:
  ON-COMMITTED (op-id results)  RESULTS is a list of CHANGE-RESULT carrying
                                before/after states and bytes for the result's
                                `changes` and `diff`. OP-ID is NIL for
                                DRY-RUN and when nothing changed.
  ON-REJECTED (code message &key candidates diagnostics conflicts repairs path)
                                validation failed, the planner refused, or an
                                I/O error occurred (`environment.io`).
  ON-BUSY ()                    the lock was not acquired within
                                LOCK-TIMEOUT-MS (`environment.busy`).

An I/O failure after the commit point is neither: STORE-COMMITTED-ERROR is
signalled (the lock released), naming the op and the failing path.

DRY-RUN validates and plans without the lock and writes nothing. UNDOES is
recorded in the journal entry as `undoes`; ARGV is the command's argv, recorded
as given."
  (declare (type function validate on-committed on-rejected on-busy))
  (let ((outcome nil))
    (let ((run-validation
            (lambda ()
              (fault-point :after-lock)
              (funcall
               validate
               (lambda (requests)
                 (setf outcome
                       (if dry-run
                           (aitools.kernel.domain:flow-call
                               (%disk-plan/k store requests)
                             (:on-planned (results)
                               (list :committed nil results))
                             (:on-rejected (code message)
                               (list :rejected code message)))
                           (aitools.kernel.domain:flow-call
                               (%plan-and-commit/k store argv requests
                                                   :undoes undoes)
                             (:on-committed (op-id results)
                               (list :committed op-id results))
                             (:on-rejected (code message)
                               (list :rejected code message))))))
               (lambda (code message
                      &key (candidates nil candidates-p)
                           (diagnostics nil diagnostics-p)
                           (conflicts nil conflicts-p)
                           (repairs nil repairs-p)
                           (path nil path-p))
                 (setf outcome
                       (list* :rejected code message
                              (%rejection-keywords
                               :candidates candidates :candidates-p candidates-p
                               :diagnostics diagnostics :diagnostics-p diagnostics-p
                               :conflicts conflicts :conflicts-p conflicts-p
                               :repairs repairs :repairs-p repairs-p
                               :path path :path-p path-p))))))))
      (if dry-run
          (funcall run-validation)
          (aitools.kernel.domain:flow-call
              (call-with-workspace-lock/k store lock-timeout-ms)
            (:on-acquired ()
              (funcall run-validation))
            (:on-timeout ()
              (setf outcome (list :busy))))))
    (unless outcome
      (error "commit-changes/k: VALIDATE returned without calling COMMIT or REJECT"))
    (ecase (first outcome)
      (:committed
       (aitools.kernel.domain:flow-continue
        :on-committed (second outcome) (third outcome)))
      (:rejected (%flow-continue-rejection (rest outcome)))
      (:busy
       (aitools.kernel.domain:flow-continue :on-busy))))
)
