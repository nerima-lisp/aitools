(in-package #:aitools.integration.dispatch-test)

(describe "aitools dispatch: partial results"
  (it "writes partial fields to stdout and returns exit code 3"
    (aitools.test.support:with-workspace
      (*root* state-home)
      (declare (ignore state-home))
      (let ((app
              (cl-cli:make-app
               :name "aitools"
               :require-command t
               :global-options
               (list (cl-cli:make-option :name "root" :kind :value)
                     (cl-cli:make-option :name "lock-timeout" :kind :value))
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
                               (list "--root" *root* "partial"))
          (expect (list code stream) :to-equal (list 3 :stdout))
          (expect envelope :to-match-envelope "command" "partial" "status" "partial")
          (expect (length (value envelope "items")) :to-be 2)
          (expect (value envelope "items" 0) :to-equal "one")
          (expect (value envelope "items" 1) :to-equal "two"))))))
