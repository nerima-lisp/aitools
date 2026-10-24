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
                 (spans (coerce (value trace "spans") (quote list)))
                 (command-spans
                   (remove-if-not (lambda (span) (string= (value span "kind") "command"))
                                  spans))
                 (root-spans
                   (remove-if-not
                    (lambda (span)
                      (eq (value span "parent_span_id") json-kit:+json-null+))
                    command-spans))
                 (root (first root-spans))
                 (root-id (and root (value root "span_id")))
                 (child-spans (set-difference command-spans root-spans))
                 (results (value envelope "results")))
            (expect (length command-spans) :to-be 3)
            (expect (length root-spans) :to-be 1)
            (expect (and root (string= (value root "name") "batch")) :to-be t)
            (expect (length child-spans) :to-be 2)
            (expect (>= (value trace "metrics" "span_count") 3) :to-be t)
            (expect (value trace "metrics" "command_count") :to-be 3)
            (expect
             (map (quote list) (lambda (span) (value span "parent_span_id")) child-spans)
             :to-equal (list root-id root-id))
            (expect
             (map (quote list) (lambda (span) (value span "name")) child-spans)
             :to-equal (list "util.calc" "util.calc"))
            (expect (map (quote list) (lambda (result) (value result "meta")) results)
                    :to-equal (list nil nil)))))))
)