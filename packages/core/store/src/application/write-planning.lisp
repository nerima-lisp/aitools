(in-package #:aitools.store.application)

(defun %new-id (store formatter)
  (funcall formatter (%io store now) (%io store random-hex 8)))

(aitools.kernel.domain:define-flow
    (%disk-plan/k store requests &key on-planned on-rejected)
  (aitools.kernel.domain:flow-call
      (plan-changes/k requests
                      :lookup-state
                      (lambda (path)
                        (workspace-state
                         store path
                         :max-bytes (%request-byte-limit requests path)))
                      :lookup-content
                      (lambda (path)
                        (%read-bounded
                         store (%workspace-path store path)
                         (%request-byte-limit requests path)))
                      :list-children (lambda (path) (%list-children store path))
                      :lookup-mtime (lambda (path) (workspace-mtime store path)))
    (:on-planned (results)
      (multiple-value-bind (refusal-code refusal-message)
          (handler-case
              (progn
                (%refuse-unsafe-steps
                 store
                 (changes->steps
                  "preview" results
                  (lambda (path)
                    (eq (%kind-at store (%workspace-path store path)) :directory))))
                (values nil nil))
            (store-refusal (condition)
              (values (store-refusal-code condition)
                      (store-refusal-message condition))))
        (if refusal-code
            (aitools.kernel.domain:flow-continue
             :on-rejected refusal-code refusal-message)
            (aitools.kernel.domain:flow-continue :on-planned results))))
    (:on-rejected (code message)
      (aitools.kernel.domain:flow-continue :on-rejected code message))))

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
                                (lambda (path) (eq (%kind-at store (%workspace-path store path)) :directory))
                                (lambda (path) (nth-value 5 (%io store lstat (%workspace-path store path))))))
         (intent (make-intent :op-id op-id :steps steps :journal-entry entry :tx-id tx-id))
         (intent-path (intent-file-path state op-id))
         (header (encode-intent-header intent))
         (by-path (make-hash-table :test 'equal)))
    (dolist (result results)
      (setf (gethash (change-result-path result) by-path) result))
    (fault-point :after-validate op-id)
    (%refuse-unsafe-steps store steps)
    (dolist (step steps) (%refuse-changed-deletion store step))
    (when (store-temporary store)
      ;; mktemp's area is scratch space, outside the journal. Without
      ;; a record there is nothing to roll forward, so a crash mid-apply can
      ;; leave some of the op's paths changed. Only the prepared temp files are
      ;; a resource to unwind, so the apply that follows runs unguarded.
      (with-prepared-intent (store nil steps)
        (%prepare-temps store op-id steps by-path))
      (let ((replace-temps (%replace-temps steps)))
        (dolist (step steps)
          (%apply-step store step replace-temps))
        (%finish-directory-modes store steps))
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
      (dolist (step steps) (%refuse-changed-deletion store step))
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

(aitools.kernel.domain:define-flow
    (%plan-and-commit/k store argv requests &key undoes tx-id
                        on-committed on-rejected)
  "Plan REQUESTS against the disk and commit them; the workspace lock must be
held. ON-COMMITTED (op-id results) with OP-ID NIL when nothing changed. A
STORE-COMMITTED-ERROR is not a rejection and propagates to the caller."
  (declare (type function on-committed on-rejected))
  (aitools.kernel.domain:flow-call
      (%disk-plan/k store requests)
    (:on-planned (results)
      (if (null results)
          (aitools.kernel.domain:flow-continue :on-committed nil '())
          (multiple-value-bind (status result)
              (handler-case
                  (values :committed
                          (%commit-results
                           store argv results :undoes undoes :tx-id tx-id))
                (store-refusal (condition)
                  (values :rejected
                          (list (store-refusal-code condition)
                                (store-refusal-message condition))))
                ((and store-io-error (not store-committed-error)) (condition)
                  (values :rejected (%io-rejection condition))))
            (ecase status
              (:committed
               (aitools.kernel.domain:flow-continue
                :on-committed result results))
              (:rejected
               (aitools.kernel.domain:flow-continue
                :on-rejected (first result) (second result)))))))
    (:on-rejected (code message)
      (aitools.kernel.domain:flow-continue :on-rejected code message))))

