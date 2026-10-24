;;;; t/e2e/byte-gate-test.lisp
(in-package #:aitools.e2e.test)

(defun byte-gate-test-octets (octets)
  (make-array (length octets)
              :element-type '(unsigned-byte 8)
              :initial-contents (coerce octets 'list)))

(defun byte-gate-test-entry (case-id stdout &key (invocation 1) (exit-code 0) (stderr #()))
  (list :case-id case-id :invocation invocation :exit-code exit-code
        :stdout stdout :stderr stderr))

(defun byte-gate-test-write-capture (root name entries &key corrupt-first-stdout-length)
  (let* ((directory (merge-pathnames (format nil "~A/" name)
                                     (uiop:ensure-directory-pathname root)))
         (manifest (merge-pathnames "records.jsonl" directory)))
    (ensure-directories-exist manifest)
    (with-open-file (stream manifest :direction :output :if-exists :error
                                     :external-format :utf-8)
      (loop for entry in entries
            for index from 1
            for stem = (format nil "~8,'0D" index)
            for stdout-name = (concatenate 'string stem ".stdout.bin")
            for stderr-name = (concatenate 'string stem ".stderr.bin")
            for stdout = (getf entry :stdout)
            for stderr = (getf entry :stderr)
            for stdout-length = (+ (length stdout)
                                   (if (and corrupt-first-stdout-length (= index 1)) 1 0))
            do (with-open-file (blob (merge-pathnames stdout-name directory)
                                     :direction :output :if-exists :error
                                     :element-type '(unsigned-byte 8))
                 (write-sequence (byte-gate-test-octets stdout) blob))
               (with-open-file (blob (merge-pathnames stderr-name directory)
                                     :direction :output :if-exists :error
                                     :element-type '(unsigned-byte 8))
                 (write-sequence (byte-gate-test-octets stderr) blob))
               (write-line (json-kit:stringify (json-kit:alist->json-object (list (cons "case_id" (getf entry :case-id))
                        (cons "invocation" (getf entry :invocation))
                        (cons "exit_code" (getf entry :exit-code))
                        (cons "stdout_file" stdout-name)
                        (cons "stderr_file" stderr-name)
                        (cons "stdout_length" stdout-length)
                        (cons "stderr_length" (length stderr))))) stream)))
    directory))

(defun byte-gate-test-captures (root specifications)
  (loop for (name entries corrupt-first-stdout-length) in specifications
        collect (byte-gate-test-write-capture
                 root name entries
                 :corrupt-first-stdout-length corrupt-first-stdout-length)))

(defun byte-gate-test-error-message (function)
  (handler-case (progn (funcall function) "")
    (error (condition) (princ-to-string condition))))

(defun byte-gate-test-compare (captures expected-ids
                               &key report-path baseline-nondeterministic-fields)
  (apply #'compare-e2e-byte-captures
         (append captures
                 (list :expected-ids expected-ids
                       :baseline-revision +byte-gate-baseline-revision+
                       :baseline-nondeterministic-fields
                       baseline-nondeterministic-fields)
                 (when report-path (list :report-path report-path)))))

(describe "E2E byte capture comparison"
  (it "passes byte-identical captures and reports the fixed expected inventory and allowance"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((entries (list (byte-gate-test-entry "case-a" #(0 255 10) :stderr #(13 10))))
             (expected-ids '(("case-a" 1)))
             (captures (byte-gate-test-captures
                        root (list (list "baseline-a" entries)
                                   (list "baseline-b" entries)
                                   (list "candidate-a" entries)
                                   (list "candidate-b" entries))))
             (report-path (merge-pathnames "comparison.json" root))
             (report (byte-gate-test-compare captures expected-ids :report-path report-path))
             (saved-report
               (with-open-file (stream report-path :direction :input :external-format :utf-8)
                 (json-kit:parse (read-line stream) :false-value nil :null-value nil))))
        (expect (getf report :passed) :to-be t)
        (expect (getf report :expected-invocation-count) :to-be 1)
        (expect (getf report :allowed-determinism-drop) :to-equal "0.0")
        (expect (gethash "expected_invocation_count" saved-report) :to-be 1)
        (expect (gethash "allowed_determinism_drop" saved-report) :to-equal "0.0")))
    :prefix "aitools-byte-gate"))

  (it "rejects a raw output octet mismatch"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((expected-ids '(("case-a" 1)))
             (baseline (list (byte-gate-test-entry "case-a" #(0 255 10))))
             (candidate (list (byte-gate-test-entry "case-a" #(0 254 10))))
             (captures (byte-gate-test-captures
                        root (list (list "baseline-a" baseline)
                                   (list "baseline-b" baseline)
                                   (list "candidate-a" candidate)
                                   (list "candidate-b" candidate))))
             (message (byte-gate-test-error-message
                       (lambda () (byte-gate-test-compare captures expected-ids)))))
        (expect (search "deterministic baseline bytes or exit status changed" message)
                :to-be-truthy)))
    :prefix "aitools-byte-gate"))

  (it "rejects a missing invocation ID from one capture"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((expected-ids '(("case-a" 1) ("case-b" 1)))
             (entry-a (byte-gate-test-entry "case-a" #(1)))
             (entry-b (byte-gate-test-entry "case-b" #(2)))
             (complete (list entry-a entry-b))
             (missing (list entry-a))
             (captures (byte-gate-test-captures
                        root (list (list "baseline-a" complete)
                                   (list "baseline-b" complete)
                                   (list "candidate-a" missing)
                                   (list "candidate-b" complete))))
             (message (byte-gate-test-error-message
                       (lambda () (byte-gate-test-compare captures expected-ids)))))
        (expect (search "does not match the expected invocation inventory" message)
                :to-be-truthy)))
    :prefix "aitools-byte-gate"))

  (it "rejects an extra invocation ID from one capture"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((expected-ids '(("case-a" 1)))
             (entry-a (byte-gate-test-entry "case-a" #(1)))
             (entry-b (byte-gate-test-entry "case-b" #(2)))
             (baseline (list entry-a))
             (extra (list entry-a entry-b))
             (captures (byte-gate-test-captures
                        root (list (list "baseline-a" baseline)
                                   (list "baseline-b" baseline)
                                   (list "candidate-a" extra)
                                   (list "candidate-b" baseline))))
             (message (byte-gate-test-error-message
                       (lambda () (byte-gate-test-compare captures expected-ids)))))
        (expect (search "does not match the expected invocation inventory" message)
                :to-be-truthy)))
    :prefix "aitools-byte-gate"))

  (it "rejects four captures with the same nonempty but incomplete ID subset"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((expected-ids '(("case-a" 1) ("case-b" 1)))
             (incomplete (list (byte-gate-test-entry "case-a" #(1))))
             (captures (byte-gate-test-captures
                        root (list (list "baseline-a" incomplete)
                                   (list "baseline-b" incomplete)
                                   (list "candidate-a" incomplete)
                                   (list "candidate-b" incomplete))))
             (message (byte-gate-test-error-message
                       (lambda () (byte-gate-test-compare captures expected-ids)))))
        (expect (search "does not match the expected invocation inventory" message)
                :to-be-truthy)))
    :prefix "aitools-byte-gate"))

  (it "rejects an empty capture"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((expected-ids '(("case-a" 1)))
             (entries (list (byte-gate-test-entry "case-a" #(1))))
             (captures (byte-gate-test-captures
                        root (list (list "baseline-a" entries)
                                   (list "baseline-b" entries)
                                   (list "candidate-a" nil)
                                   (list "candidate-b" entries))))
             (message (byte-gate-test-error-message
                       (lambda () (byte-gate-test-compare captures expected-ids)))))
        (expect (search "capture is empty" message) :to-be-truthy)))
    :prefix "aitools-byte-gate"))

  (it "rejects a corrupt blob length"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((expected-ids '(("case-a" 1)))
             (entries (list (byte-gate-test-entry "case-a" #(1 2))))
             (captures (byte-gate-test-captures
                        root (list (list "baseline-a" entries)
                                   (list "baseline-b" entries t)
                                   (list "candidate-a" entries)
                                   (list "candidate-b" entries))))
             (message (byte-gate-test-error-message
                       (lambda () (byte-gate-test-compare captures expected-ids)))))
        (expect (search "blob length mismatch" message) :to-be-truthy)))
    :prefix "aitools-byte-gate"))

  (it "rejects candidate determinism-rate loss with the zero allowance"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((expected-ids '(("case-a" 1) ("case-b" 1)))
             (baseline (list (byte-gate-test-entry "case-a" #(1))
                             (byte-gate-test-entry "case-b" #(2))))
             (candidate-second (list (byte-gate-test-entry "case-a" #(1))
                                     (byte-gate-test-entry "case-b" #(3))))
             (captures (byte-gate-test-captures
                        root (list (list "baseline-a" baseline)
                                   (list "baseline-b" baseline)
                                   (list "candidate-a" baseline)
                                   (list "candidate-b" candidate-second))))
             (message (byte-gate-test-error-message
                       (lambda () (byte-gate-test-compare captures expected-ids)))))
        (expect (search "candidate determinism rate fell beyond the configured allowance"
                        message)
                :to-be-truthy)))
    :prefix "aitools-byte-gate")))

(describe "E2E byte capture inventory contract"
  (it "requires the fixed inventory and baseline revision before reading captures"
    (let ((missing-inventory-message
           (byte-gate-test-error-message
            (lambda ()
              (compare-e2e-byte-captures "unused" "unused" "unused" "unused"))))
          (wrong-revision-message
           (byte-gate-test-error-message
            (lambda ()
              (compare-e2e-byte-captures
               "unused" "unused" "unused" "unused"
               :expected-ids (list (list "case-a" 1))
               :baseline-revision "wrong-revision"
               :baseline-nondeterministic-fields nil)))))
      (expect (search "requires the fixed expected invocation inventory"
                      missing-inventory-message)
              :to-be-truthy)
      (expect (search "configured baseline revision" wrong-revision-message)
              :to-be-truthy)))

  (it "reads a fixed invocation inventory fixture"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((expected-ids '(("case-a" 1) ("case-b" 2)))
             (path (merge-pathnames "inventory.lisp" root)))
        (with-open-file (stream path :direction :output :if-exists :error
                                     :external-format :utf-8)
          (write expected-ids :stream stream :readably t)
          (terpri stream))
        (expect (byte-gate-read-expected-inventory path)
                :to-equal expected-ids)))
    :prefix "aitools-byte-inventory"))

  (it "rejects an empty or multi-form invocation inventory"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let ((empty-path (merge-pathnames "empty.lisp" root))
            (multi-path (merge-pathnames "multi.lisp" root)))
        (with-open-file (stream empty-path :direction :output :if-exists :error
                                            :external-format :utf-8)
          (declare (ignore stream)))
        (with-open-file (stream multi-path :direction :output :if-exists :error
                                            :external-format :utf-8)
          (write '(("case-a" 1)) :stream stream :readably t)
          (terpri stream)
          (write nil :stream stream :readably t)
          (terpri stream))
        (expect (search "inventory is empty"
                        (byte-gate-test-error-message
                         (lambda () (byte-gate-read-expected-inventory empty-path))))
                :to-be-truthy)
        (expect (search "must contain one form"
                        (byte-gate-test-error-message
                         (lambda () (byte-gate-read-expected-inventory multi-path))))
                :to-be-truthy)))
    :prefix "aitools-byte-inventory")))
