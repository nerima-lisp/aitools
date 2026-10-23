(in-package #:aitools.integration.batch-test)

(describe "aitools batch trace"
  (it "collects one trace for a batch and parents every child command to it"
    (aitools.test.support:with-workspace
      (*root* state-home :chdir t)
      (declare (ignore state-home))
      (let ((stdin
              (json-kit:stringify
               (vector (vector "util" "calc" "1+1")
                       (vector "util" "calc" "2+2")))))
        (multiple-value-bind (code envelope stream)
            (run-aitools '("--trace" "batch" "--stdin") :stdin stdin)
          (expect (list code stream) :to-equal '(0 :stdout))
          (expect (value envelope "command") :to-equal "batch")
          (let* ((trace (value envelope "meta" "trace"))
                 (spans (coerce (value trace "spans") 'list))
                 (command-spans
                   (remove-if-not (lambda (span) (string= (value span "kind") "command"))
                                  spans))
                 (root (first command-spans))
                 (root-id (value root "span_id"))
                 (results (value envelope "results")))
            (expect (value root "name") :to-equal "batch")
            (expect (length command-spans) :to-be 3)
            (expect (>= (value trace "metrics" "span_count") 3) :to-be t)
            (expect (value trace "metrics" "command_count") :to-be 3)
            (expect
             (map 'list (lambda (span) (value span "parent_span_id")) command-spans)
             :to-equal (list json-kit:+json-null+ root-id root-id))
            (expect
             (map 'list (lambda (span) (value span "name")) command-spans)
             :to-equal '("batch" "util.calc" "util.calc"))
            (expect (map 'list (lambda (result) (value result "meta")) results)
                    :to-equal '(nil nil)))))))
)