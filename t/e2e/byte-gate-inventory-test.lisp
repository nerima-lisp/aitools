(in-package #:aitools.e2e.test)
(describe "E2E byte-gate baseline inventory"
  (it "reads the fixed revision and source reasons"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((id-a (list "case-a" 1))
             (id-b (list "case-b" 2))
             (ids (list id-a id-b))
             (reason-fields (list "$.op_id" "$.mtime"))
             (reason "The store operation ID uses the current clock and a random suffix; the file mtime comes from the run-created fixture.")
             (fields (list (list id-a reason-fields reason)))
             (path (merge-pathnames "inventory.lisp" root))
             (ids-path (merge-pathnames "ids.jsonl" root))
             (fields-path (merge-pathnames "fields.jsonl" root))
             (inventory-form
               (list :baseline-revision "fixture-revision"
                     :allowed-determinism-drop 0.0
                     :expected-invocation-file "ids.jsonl"
                     :nondeterministic-field-file "fields.jsonl")))
        (with-open-file (stream path :direction :output :if-exists :error
                                     :external-format :utf-8)
          (write inventory-form :stream stream :readably t)
          (terpri stream))
        (with-open-file (stream ids-path :direction :output :if-exists :error
                                         :external-format :utf-8)
          (write-line (json-kit:stringify (vector "case-a" 1)) stream)
          (write-line (json-kit:stringify (vector "case-b" 2)) stream))
        (with-open-file (stream fields-path :direction :output :if-exists :error
                                            :external-format :utf-8)
          (write-line (json-kit:stringify
                       (vector (vector "case-a" 1)
                               (vector "$.op_id" "$.mtime")
                               reason))
                      stream))
        (let ((inventory (byte-gate-read-inventory path)))
          (expect (getf inventory :baseline-revision) :to-equal "fixture-revision")
          (expect (getf inventory :allowed-determinism-drop) :to-be 0.0)
          (expect (getf inventory :expected-ids) :to-equal ids)
          (expect (getf inventory :nondeterministic-fields) :to-equal fields))))
    :prefix "aitools-byte-inventory"))

  (it "reports each source reason for baseline differences"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((expected-ids '(("case-a" 1) ("case-b" 1)))
             (baseline-a (list (byte-gate-test-entry "case-a" #(1))
                               (byte-gate-test-entry "case-b" #(2))))
             (baseline-b (list (byte-gate-test-entry "case-a" #(3))
                               (byte-gate-test-entry "case-b" #(2))))
             (candidate (list (byte-gate-test-entry "case-a" #(1))
                              (byte-gate-test-entry "case-b" #(2))))
             (captures (byte-gate-test-captures
                        root (list (list "baseline-a" baseline-a)
                                   (list "baseline-b" baseline-b)
                                   (list "candidate-a" candidate)
                                   (list "candidate-b" candidate))))
             (reason "The store builds operation IDs from the current UTC time and a random suffix.")
             (reason-fields (list (list '("case-a" 1) '("$.op_id") reason)))
             (report
               (byte-gate-test-compare
                captures expected-ids
                :baseline-nondeterministic-fields reason-fields)))
        (expect (getf report :passed) :to-be t)
        (expect (getf report :baseline-revision) :to-equal +byte-gate-baseline-revision+)
        (expect (length (getf report :baseline-nondeterministic)) :to-be 1)
        (expect (gethash "reason"
                         (first (getf report :baseline-nondeterministic)))
                :to-equal reason)))
    :prefix "aitools-byte-gate"))

  (it "classifies source-declared runtime output even when baseline pairs match"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((expected-ids '(("case-a" 1) ("case-b" 1)))
             (baseline (list (byte-gate-test-entry "case-a" #(1))
                             (byte-gate-test-entry "case-b" #(2))))
             (candidate-a (list (byte-gate-test-entry "case-a" #(3))
                                (byte-gate-test-entry "case-b" #(2))))
             (candidate-b (list (byte-gate-test-entry "case-a" #(4))
                                (byte-gate-test-entry "case-b" #(2))))
             (captures (byte-gate-test-captures
                        root (list (list "baseline-a" baseline)
                                   (list "baseline-b" baseline)
                                   (list "candidate-a" candidate-a)
                                   (list "candidate-b" candidate-b))))
             (reason "The run response always includes duration_ms, measured from child start to exit.")
             (reason-fields (list (list '("case-a" 1) '("$.duration_ms") reason)))
             (report (byte-gate-test-compare
                      captures expected-ids
                      :baseline-nondeterministic-fields reason-fields)))
        (expect (getf report :passed) :to-be t)
        (expect (getf report :baseline-deterministic-count) :to-be 1)
        (expect (getf report :candidate-deterministic-count) :to-be 1)
        (expect (gethash "reason" (first (getf report :baseline-nondeterministic)))
                :to-equal reason)
        (expect (gethash "reason" (first (getf report :candidate-nondeterministic)))
                :to-equal reason)))
    :prefix "aitools-byte-gate"))

  (it "rejects baseline differences without a source reason"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((expected-ids '(("case-a" 1)))
             (first-run (list (byte-gate-test-entry "case-a" #(1))))
             (second-run (list (byte-gate-test-entry "case-a" #(2))))
             (captures (byte-gate-test-captures
                        root (list (list "baseline-a" first-run)
                                   (list "baseline-b" second-run)
                                   (list "candidate-a" first-run)
                                   (list "candidate-b" first-run))))
             (message (byte-gate-test-error-message
                       (lambda ()
                         (byte-gate-test-compare captures expected-ids)))))
        (expect (search "do not cover all observed output differences" message)
                :to-be-truthy)))
    :prefix "aitools-byte-gate")))

