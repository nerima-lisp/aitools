;;;;
;;;; The write protocol before the commit point: refuse steps the protocol must not
;;;; perform, create the temp files, and discard what was prepared when an
;;;; error unwinds before the intent record's checksum is written.
(in-package #:aitools.store.application)

(defun %step-paths (step)
  (remove nil (list (intent-step-path step) (intent-step-from step) (intent-step-temp step))))

(defun %intent-violation (store intent)
  "A description of the first path in INTENT that %PATH-VIOLATION refuses,
or NIL."
  (dolist (step (intent-steps intent))
    (dolist (path (%step-paths step))
      (let ((problem (%path-violation store path)))
        (when problem
          (return-from %intent-violation (format nil "~A ~A" path problem)))))))

(defun %refuse-unsafe-steps (store steps)
  "Signal STORE-REFUSAL for a step the write protocol must not perform:
a path %PATH-VIOLATION refuses, or a chmod or mtime change of a regular file
with other hard links, which would change the inode behind every link,
including one outside the workspace."
  (dolist (step steps)
    (dolist (path (%step-paths step))
      (let ((problem (%path-violation store path)))
        (when problem
          (error 'store-refusal :code "refusal.outside-workspace" :message (format nil "~A ~A" path problem)))))
    (when (member (intent-step-op step) '(:chmod :utime))
      (multiple-value-bind (kind mode target mtime links)
          (%io store lstat (%workspace-path store (intent-step-path step)))
        (declare (ignore mode target mtime))
        (when (and (eq kind :file) links (> links 1))
          (error 'store-refusal
                 :code "refusal.not-a-file"
                 :message (format nil "~A has ~D hard links; changing its mode or time in place would change every link"
                                  (intent-step-path step) links)))))))
(defun %refuse-case-collisions (store steps)
  "Refuse replacement paths that differ only by case on a case-insensitive filesystem.

The probe reuses a recorded temp path so precommit cleanup and recovery can remove it after interruption."
  (labels ((case-insensitive-path-key (path)
             (string-downcase path))
           (case-variant-path (path)
             (let ((slash (position #\/ path :from-end t)))
               (concatenate 'string
                            (if slash (subseq path 0 (1+ slash)) "")
                            (string-upcase (if slash (subseq path (1+ slash)) path)))))
           (first-case-collision (steps)
             (let ((paths-by-key (make-hash-table :test (function equal)))
                   (collision nil)
                   (previous-path nil))
               (dolist (step steps)
                 (when (and (null collision)
                            (eq (intent-step-op step) :replace))
                   (let* ((path (intent-step-path step))
                          (key (case-insensitive-path-key path))
                          (group (gethash key paths-by-key)))
                     (cond
                       ((null group)
                        (let ((new-group (cons path (make-hash-table :test (function equal)))))
                          (setf (gethash key paths-by-key) new-group)
                          (setf (gethash path (cdr new-group)) t)))
                       ((gethash path (cdr group)))
                       (t
                        (setf collision step
                              previous-path (car group))
                        (setf (gethash path (cdr group)) t))))))
               (values collision previous-path)))
           (empty-octets ()
             (make-array 0 :element-type '(unsigned-byte 8))))
    (multiple-value-bind (step previous-path)
        (first-case-collision steps)
      (when step
        (let* ((temp (intent-step-temp step))
               (temp-path (%workspace-path store temp))
               (variant-path (%workspace-path store (case-variant-path temp)))
               (created nil))
          (unwind-protect
               (progn
                 (%io store create-file temp-path (empty-octets) :mode #o600)
                 (setf created t)
                 (when (eq (nth-value 0 (%io store lstat variant-path)) :file)
                   (error 'store-refusal
                          :code "refusal.exists"
                          :message
                          (format nil
                                  "~A and ~A resolve to the same case-insensitive path"
                                  (intent-step-path step)
                                  previous-path))))
            (when (and created
                       (not (eq (%kind-at store temp-path) :absent)))
              (%io store unlink temp-path))))))))

(aitools.kernel.domain:define-flow
    (call-with-prepared-intent/k store intent-path steps &key on-prepared)
  "Run ON-PREPARED to prepare the temp files for STEPS and, when INTENT-PATH
is non-NIL, its intent record. If ON-PREPARED signals an ERROR, the temp
files and any intent record are removed on a best-effort basis before the
error propagates. A non-error unwind leaves the prepared state in place;
without an intent path, there is no record for recovery to inspect. A normal
return resumes the caller after preparation."
  (declare (type function on-prepared))
  (flet ((discard ()
           ;; Best-effort removal of what prepare created: the `.aitools-*.tmp`
           ;; files (every scan ignores them) and, when it exists, the
           ;; incomplete record.
           (handler-case
               (progn
                 (dolist (step steps)
                   (let ((temp (intent-step-temp step)))
                     (when temp
                       (let ((absolute (%workspace-path store temp)))
                         (unless (eq (%kind-at store absolute) :absent)
                           (%io store unlink absolute))))))
                 (when (and intent-path (not (eq (%kind-at store intent-path) :absent)))
                   (%io store unlink intent-path)))
             (store-io-error () nil))))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (discard))))
      (aitools.kernel.domain:flow-continue :on-prepared))))

(defmacro with-prepared-intent ((store intent-path steps) &body body)
  (let ((prepared (gensym "PREPARED")))
    `(flet ((,prepared () ,@body))
       (declare (dynamic-extent #',prepared))
       (aitools.kernel.domain:flow-call
           (call-with-prepared-intent/k ,store ,intent-path ,steps)
         (:on-prepared ()
           (,prepared))))))

(defun %prepare-temps (store op-id steps by-path)
  "The write protocol's temp files for the :REPLACE STEPS, contents from BY-PATH
(path -> CHANGE-RESULT)."
  (loop with n = 0
        for step in steps
        when (eq (intent-step-op step) :replace)
          do (let ((temp (%workspace-path store (intent-step-temp step))))
               (ecase (intent-step-kind step)
                 (:file
                  (%io store create-file temp
                       (change-result-after-content (gethash (intent-step-path step) by-path))
                       :mode (intent-step-mode step) :sync t)
                  (when (intent-step-mtime step)
                    (%io store set-mtime temp (intent-step-mtime step))))
                 (:symlink
                  (%io store symlink (intent-step-target step) temp)))
               (fault-point :after-temp op-id (incf n)))))
