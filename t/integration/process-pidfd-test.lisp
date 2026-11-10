(in-package #:aitools.process.integration-test)

(describe "Linux pidfd process signalling"
  (it "signals a live child and reports its pidfd gone after reaping"
    (aitools.test.support:call-with-temporary-directory
     (lambda (root)
       (multiple-value-bind (supervisor token) (signal-child root)
         (unwind-protect
              (let* ((target (or (matching-signal-child token supervisor)
                                 (error "signal child did not appear")))
                     (pid (aitools.process.domain:process-identity-pid target))
                     (boundaries
                       (aitools.process.composition::make-process-dependencies))
                     (signal-process
                       (cl-boundary-kit:boundary-context-require
                        boundaries :process-signal-process)))
                (expect (funcall signal-process target 15) :to-be t)
                (expect (wait-for-pid-gone pid) :to-be t)
                (expect (funcall signal-process target 15) :to-be nil)
                (expect (pid-alive-p pid) :to-be nil))
           (kill-group supervisor)
           (expect (wait-for-group-gone supervisor) :to-be t)))))))
