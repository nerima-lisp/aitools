(in-package #:aitools.e2e.test)
(defun byte-gate-same-output-p (left right)
  (and (= (byte-gate-record-exit-code left) (byte-gate-record-exit-code right))
       (equalp (byte-gate-record-stdout left) (byte-gate-record-stdout right))
       (equalp (byte-gate-record-stderr left) (byte-gate-record-stderr right))))
(defun byte-gate-deterministic-ids (first second ids)
  (remove-if-not (lambda (id)
                   (byte-gate-same-output-p (gethash id first) (gethash id second)))
                 ids))
(defun byte-gate-nondeterministic-entry-valid-p (entry ids)
  (let ((entry-count (handler-case (list-length entry)
                      (type-error () nil))))
    (and entry-count
         (= entry-count 3)
         (let* ((id (first entry))
                (fields (second entry))
                (reason (third entry))
                (field-count (handler-case (list-length fields)
                               (type-error () nil))))
           (and (consp id)
                (stringp (first id))
                (plusp (length (first id)))
                (consp (rest id))
                (integerp (second id))
                (plusp (second id))
                (null (cddr id))
                (member id ids :test (function equal))
                field-count
                (plusp field-count)
                (every (lambda (field)
                         (and (stringp field) (plusp (length field))))
                       fields)
                (= field-count
                   (length (remove-duplicates fields :test (function string=))))
                (stringp reason)
                (plusp (length reason)))))))

(defun compare-e2e-byte-captures
    (baseline-first baseline-second candidate-first candidate-second
     &key (expected-ids nil expected-ids-p)
       baseline-revision
       (baseline-nondeterministic-fields nil fields-p)
       report-path)
  (unless expected-ids-p
    (error "byte-gate comparison requires the fixed expected invocation inventory"))
  (unless (and (stringp baseline-revision)
               (string= baseline-revision +byte-gate-baseline-revision+))
    (error "byte-gate baseline revision does not match the fixed C04 revision"))
  (unless fields-p
    (error "byte-gate comparison requires baseline nondeterministic field reasons"))
  (let* ((ids (byte-gate-validate-ids expected-ids))
         (reason-count (handler-case (list-length baseline-nondeterministic-fields)
                         (type-error () nil)))
         (reason-entries-valid-p
           (and reason-count
                (every (lambda (entry)
                         (byte-gate-nondeterministic-entry-valid-p entry ids))
                       baseline-nondeterministic-fields)))
         (reason-ids (if reason-entries-valid-p
                         (mapcar (function first) baseline-nondeterministic-fields)
                         nil))
         (captures (let ((captures (list (byte-gate-read-capture baseline-first)
                                         (byte-gate-read-capture baseline-second)
                                         (byte-gate-read-capture candidate-first)
                                         (byte-gate-read-capture candidate-second))))
                     (loop for capture in captures
                           for capture-number from 1
                           unless (equal ids (byte-gate-record-ids capture))
                             do (error "byte-gate capture ~D does not match the expected invocation inventory"
                                       capture-number))
                     captures))
         (baseline-a (first captures))
         (baseline-b (second captures))
         (candidate-a (third captures))
         (candidate-b (fourth captures))
         (deterministic-baseline
           (remove-if (lambda (id) (member id reason-ids :test (function equal)))
                      (byte-gate-deterministic-ids baseline-a baseline-b ids)))
         (baseline-nondeterministic-ids
           (remove-if (lambda (id) (member id deterministic-baseline :test (function equal))) ids))
         (deterministic-candidate
           (remove-if (lambda (id) (member id reason-ids :test (function equal)))
                      (byte-gate-deterministic-ids candidate-a candidate-b ids)))
         (candidate-nondeterministic-ids
           (remove-if (lambda (id) (member id deterministic-candidate :test (function equal))) ids))
         (mismatches (list))
         (failures (list)))
    (unless reason-entries-valid-p
      (error "baseline nondeterministic entries must have valid invocation IDs, fields, and reasons"))
    (unless (and (= reason-count (length (remove-duplicates reason-ids :test (function equal))))
                 (every (lambda (id) (member id baseline-nondeterministic-fields
                                              :key (function first) :test (function equal)))
                        reason-ids)
                 (every (lambda (id) (member id reason-ids :test (function equal)))
                        baseline-nondeterministic-ids))
      (error "baseline nondeterministic field reasons do not cover all observed output differences"))
    (when (null deterministic-baseline)
      (push "baseline has no deterministic invocation" failures))
    (when (null deterministic-candidate)
      (push "candidate has no deterministic invocation" failures))
    (dolist (id deterministic-baseline)
      (unless (byte-gate-same-output-p (gethash id baseline-a) (gethash id candidate-a))
        (push (list (byte-gate-id-label id) "candidate-first") mismatches))
      (unless (byte-gate-same-output-p (gethash id baseline-a) (gethash id candidate-b))
        (push (list (byte-gate-id-label id) "candidate-second") mismatches)))
    (when mismatches
      (push "deterministic baseline bytes or exit status changed" failures))
    (let* ((baseline-rate (/ (length deterministic-baseline) (length ids)))
           (candidate-rate (/ (length deterministic-candidate) (length ids)))
           (drop (- baseline-rate candidate-rate)))
      (when (> drop +byte-gate-allowed-determinism-drop+)
        (push "candidate determinism rate fell beyond the configured allowance" failures))
      (let ((report
              (list :passed (null failures)
                    :baseline-revision baseline-revision
                    :expected-invocation-count (length ids)
                    :baseline-deterministic-count (length deterministic-baseline)
                    :baseline-invocation-count (length ids)
                    :candidate-deterministic-count (length deterministic-candidate)
                    :candidate-invocation-count (length ids)
                    :allowed-determinism-drop
                    (princ-to-string +byte-gate-allowed-determinism-drop+)
                    :baseline-nondeterministic
                    (byte-gate-nondeterministic-entries
                     baseline-nondeterministic-ids
                     :reasons-by-id baseline-nondeterministic-fields)
                    :candidate-nondeterministic
                    (byte-gate-nondeterministic-entries
                     candidate-nondeterministic-ids
                     :reasons-by-id baseline-nondeterministic-fields
                     :default-reason
                     "candidate stdout, stderr, or exit status differed between runs; reason not identified")
                    :byte-mismatches (byte-gate-mismatch-entries (nreverse mismatches))
                    :failure-reasons (nreverse failures))))
        (when report-path
          (byte-gate-write-report report report-path))
        (unless (getf report :passed)
          (error "E2E byte gate failed: ~{~A~^; ~}" (getf report :failure-reasons)))
        report))))
