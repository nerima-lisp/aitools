(in-package #:aitools.e2e.test)
(describe "E2E byte-gate baseline inventory"
  (it "reads the fixed revision and baseline nondeterministic fields"
    (aitools.test.support:with-temporary-directory (root :prefix "aitools-byte-inventory")
      (let* ((id-a (list "case-a" 1))
             (id-b (list "case-b" 2))
             (ids (list id-a id-b))
             (reason-fields (list "$.op_id" "$.mtime"))
             (fields (list (list id-a reason-fields)))
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
                       (vector (vector "case-a" 1) (vector "$.op_id" "$.mtime")))
                      stream))
        (let ((inventory (byte-gate-read-inventory path)))
          (expect (getf inventory :baseline-revision) :to-equal "fixture-revision")
          (expect (getf inventory :allowed-determinism-drop) :to-be 0.0)
          (expect (getf inventory :expected-ids) :to-equal ids)
          (expect (getf inventory :nondeterministic-fields)
                  :to-equal (list (cons id-a reason-fields)))))))

  (it "reports per-invocation reasons for nondeterministic baseline output"
    (aitools.test.support:with-temporary-directory (root :prefix "aitools-byte-gate")
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
             (reason-fields (list (cons '("case-a" 1) '("$.op_id"))))
             (report
               (byte-gate-test-compare
                captures expected-ids
                :baseline-nondeterministic-fields reason-fields)))
        (expect (getf report :passed) :to-be t)
        (expect (getf report :baseline-revision) :to-equal +byte-gate-baseline-revision+)
        (expect (length (getf report :baseline-nondeterministic)) :to-be 1)
        (expect (gethash "reason"
                         (first (getf report :baseline-nondeterministic)))
                :to-equal "JSON response fields differed between baseline runs: $.op_id"))))

  (it "rejects baseline reasons that do not match the observed nondeterministic IDs"
    (aitools.test.support:with-temporary-directory (root :prefix "aitools-byte-gate")
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
        (expect (search "reasons do not match the baseline captures" message)
                :to-be-truthy)))))

