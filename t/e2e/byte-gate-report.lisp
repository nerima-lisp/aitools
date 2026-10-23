(in-package #:aitools.e2e.test)
(defun byte-gate-id-label (id)
                                 (format nil "~A#~D" (first id) (second id)))
(defun byte-gate-report-value (report)
                                                (json-kit:alist->json-object
                                                 (list (cons "passed" (if (getf report :passed) t json-kit:+json-false+))
                                                       (cons "baseline_revision" (getf report :baseline-revision))
                                                       (cons "expected_invocation_count" (getf report :expected-invocation-count))
                                                       (cons "baseline_deterministic_count" (getf report :baseline-deterministic-count))
                                                       (cons "baseline_invocation_count" (getf report :baseline-invocation-count))
                                                       (cons "candidate_deterministic_count" (getf report :candidate-deterministic-count))
                                                       (cons "candidate_invocation_count" (getf report :candidate-invocation-count))
                                                       (cons "allowed_determinism_drop" (getf report :allowed-determinism-drop))
                                                       (cons "baseline_nondeterministic" (getf report :baseline-nondeterministic))
                                                       (cons "candidate_nondeterministic" (getf report :candidate-nondeterministic))
                                                       (cons "byte_mismatches" (getf report :byte-mismatches))
                                                       (cons "failure_reasons" (getf report :failure-reasons)))))
(defun byte-gate-nondeterministic-entries (ids deterministic
                                                                                                                  &key fields-by-id default-reason)
                                                                     (loop for id in ids
                                                                           unless (member id deterministic :test #'equal)
                                                                           collect (let* ((fields (cdr (assoc id fields-by-id :test #'equal)))
                                                                                          (reason
                                                                                           (if fields
                                                                                               (format nil "JSON response fields differed between baseline runs: ~{~A~^, ~}"
                                                                                                       fields)
                                                                                             default-reason)))
                                                                                     (json-kit:alist->json-object
                                                                                      (list (cons "case_id" (first id))
                                                                                            (cons "invocation" (second id))
                                                                                            (cons "reason" reason))))))
(defun byte-gate-mismatch-entries (mismatches)
                                                        (mapcar (lambda (mismatch)
                                                                  (json-kit:alist->json-object
                                                                   (list (cons "invocation_id" (first mismatch))
                                                                         (cons "candidate_run" (second mismatch)))))
                                                                mismatches))
(defun byte-gate-write-report (report pathname)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction :output :if-exists :error
                                   :if-does-not-exist :create :external-format :utf-8)
    (write-string (json-kit:stringify (byte-gate-report-value report)) stream)
    (terpri stream))
  pathname)

