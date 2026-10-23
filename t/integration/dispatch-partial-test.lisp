(in-package #:aitools.integration.dispatch-test)

(describe "aitools dispatch: partial results"
  (it "writes partial fields to stdout and returns exit code 3 with trace metadata"
    (aitools.test.support:with-workspace
      (*root* state-home)
      (declare (ignore state-home))
      (let ((app
              (cl-cli:make-app
               :name "aitools"
               :require-command t
               :global-options
               (list (cl-cli:make-option :name "root" :kind :value)
                     (cl-cli:make-option :name "lock-timeout" :kind :value)
                     (cl-cli:make-option :name "trace" :kind :flag))
               :commands
               (list
                (cl-cli:make-command
                 :name "partial"
                 :handler
                 (lambda (invocation)
                   (declare (ignore invocation))
                   (aitools.protocol.application:call-with-command-result/k
                    (lambda (&key on-ok on-partial on-error)
                      (declare (ignore on-ok on-error))
                      (funcall on-partial
                               (list (cons "items" (vector "one" "two"))))))))))))
        (multiple-value-bind (code envelope stream)
            (dispatch-envelope app (aitools.protocol.application:make-command-registry)
                               (list "--root" *root* "--trace" "partial"))
          (expect (list code stream) :to-equal (list 3 :stdout))
          (expect envelope :to-match-envelope "command" "partial" "status" "partial")
          (expect (length (value envelope "items")) :to-be 2)
          (expect (value envelope "items" 0) :to-equal "one")
          (expect (value envelope "items" 1) :to-equal "two")
          (let* ((meta (value envelope "meta"))
                 (trace (value meta "trace"))
                 (spans (coerce (value trace "spans") 'list))
                 (root
                  (find-if (lambda (span)
                             (and (equal (value span "kind") "command")
                                  (eq (value span "parent_span_id")
                                      json-kit:+json-null+)))
                           spans))
                 (metrics (value trace "metrics")))
            (flet ((object-keys (object)
                     (sort (loop for key being the hash-keys of object collect key)
                           #'string<)))
              (expect (mapcar #'object-keys (list meta trace root metrics))
                      :to-equal
                      '(("trace")
                        ("metrics" "spans")
                        ("duration_ns" "kind" "name" "outcome" "parent_span_id" "span_id" "start_ns")
                        ("child_process_count" "command_count" "duration_ns" "io_count" "span_count")))
              (expect (length spans) :to-be 2)
              (expect (sort (mapcar (lambda (span)
                                           (value span "span_id"))
                                         spans)
                                 (function <))
                      :to-equal (quote (0 1)))
              (progn
                (expect (value root "parent_span_id") :to-be json-kit:+json-null+)
                (expect (value
                         (find-if (lambda (span)
                                    (equal (value span "kind") "io"))
                                  spans)
                         "parent_span_id")
                        :to-be (value root "span_id")))
              (expect (value root "kind") :to-equal "command")
              (expect (value root "name") :to-equal "partial")
              (expect (value root "start_ns") :to-be 0)
              (expect (and (integerp (value root "duration_ns"))
                           (>= (value root "duration_ns") 0))
                      :to-be t)
              (expect (value root "outcome") :to-equal "partial")
              (expect (list (value metrics "span_count") (value metrics "command_count") (value metrics "child_process_count") (value metrics "io_count")) :to-equal (quote (2 1 0 1)))
              (expect (value metrics "duration_ns")
                      :to-be (value root "duration_ns")))))))))
