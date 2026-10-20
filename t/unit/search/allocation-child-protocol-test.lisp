(in-package #:aitools.search.test)
(describe "aitools.search allocation child READY/GO protocol"
  (it "starts the measurement timeout after slow setup reaches READY"
    (let* ((now 0)
           (measurement-timeout 1)
           (ready-p nil)
           (communicated-at nil)
           (communicated-input nil)
           (communicated-timeout nil)
           (poll-timeout nil)
           (setup-output (format nil "compile complete~%~A~%" +allocation-child-ready-marker+))
           (setup-diagnostic "setup diagnostic~%")
           (stdout-position 0)
           (stderr-position 0)
           (cleanup (make-%allocation-protocol-test-cleanup))
           (stdout (make-string-input-stream ""))
           (stderr (make-string-input-stream ""))
           (waiter (%allocation-protocol-test-waiter
                    stdout stderr
                    (lambda (fds &key timeout)
                      (declare (ignore fds))
                      (setf poll-timeout timeout)
                      (incf now (* 2 internal-time-units-per-second))
                      (setf ready-p t)
                      '(11 12))
                    (lambda () now) t
                    (lambda (stream eof-error-p eof-value)
                      (declare (ignore eof-error-p))
                      (let* ((stdout-p (eq stream stdout))
                             (content (if stdout-p setup-output setup-diagnostic))
                             (position (if stdout-p stdout-position stderr-position)))
                        (when ready-p
                          (if (< position (length content))
                              (prog1 (char content position)
                                (if stdout-p
                                    (incf stdout-position)
                                    (incf stderr-position)))
                              eof-value)))))))
      (multiple-value-bind (captured-stdout captured-stderr)
          (%run-allocation-protocol-test
           (lambda (process timeout)
             (multiple-value-bind (out err) (funcall waiter process timeout)
               (setf ready-p t)
               (values out err)))
           (lambda (process input timeout)
             (declare (ignore process))
             (setf communicated-at now communicated-input input communicated-timeout timeout)
             (make-%allocation-protocol-test-result
              :stdout (%allocation-protocol-test-output) :stderr ""
              :exit-code 0 :timed-out-p nil))
           cleanup :exited-p t :startup-timeout 10
           :measurement-timeout measurement-timeout)
        (expect communicated-input :to-equal +allocation-child-go-line+)
        (expect communicated-timeout :to-be measurement-timeout)
        (expect (= poll-timeout 10d0) :to-be t)
        (expect (> communicated-at (* measurement-timeout internal-time-units-per-second)) :to-be t)
        (expect captured-stdout :to-contain "compile complete")
        (expect captured-stdout :to-contain "@@aitools-search-allocation-results@@")
        (expect captured-stderr :to-contain "setup diagnostic")
        (expect (%allocation-protocol-test-cleanup-streams-closed-p cleanup) :to-be t)
        (expect (%allocation-protocol-test-cleanup-process-terminated-p cleanup) :to-be nil))))


  (it "reports startup timeout while the child remains alive"
    (let* ((timeout-now 0)
           (timeout-cleanup (make-%allocation-protocol-test-cleanup))
           (timeout-communicated nil)
           (timeout-stdout (make-string-input-stream ""))
           (timeout-stderr (make-string-input-stream ""))
           (timeout-waiter
             (%allocation-protocol-test-waiter
              timeout-stdout timeout-stderr
              (lambda (fds &key timeout)
                (declare (ignore fds))
                (incf timeout-now (* timeout internal-time-units-per-second))
                nil)
              (lambda () timeout-now) nil))
           (timeout-message
             (%allocation-protocol-error
              (lambda ()
                (%run-allocation-protocol-test
                 timeout-waiter
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (setf timeout-communicated t))
                 timeout-cleanup :exited-p nil :startup-timeout 2)))))
      (expect timeout-message :to-contain "startup timed out waiting for READY")
      (expect timeout-communicated :to-be nil)
      (expect (%allocation-protocol-test-cleanup-process-terminated-p timeout-cleanup) :to-be t)
      (expect (%allocation-protocol-test-cleanup-streams-closed-p timeout-cleanup) :to-be t)))

  (it "classifies a deadline-edge exit as startup timeout"
    (let* ((timeout-now 0)
           (timeout-cleanup (make-%allocation-protocol-test-cleanup))
           (timeout-communicated nil)
           (timeout-stdout (make-string-input-stream ""))
           (timeout-stderr (make-string-input-stream ""))
           (timeout-waiter
             (%allocation-protocol-test-waiter
              timeout-stdout timeout-stderr
              (lambda (fds &key timeout)
                (declare (ignore fds))
                (incf timeout-now (* timeout internal-time-units-per-second))
                nil)
              (lambda () timeout-now) t))
           (timeout-message
             (%allocation-protocol-error
              (lambda ()
                (%run-allocation-protocol-test
                 timeout-waiter
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (setf timeout-communicated t))
                 timeout-cleanup :exited-p t :startup-timeout 2)))))
      (expect timeout-message :to-contain "startup timed out waiting for READY")
      (expect timeout-communicated :to-be nil)
      (expect (search "setup failed: child exited before READY" timeout-message) :to-be nil)
      (expect (%allocation-protocol-test-cleanup-process-terminated-p timeout-cleanup) :to-be nil)
      (expect (%allocation-protocol-test-cleanup-streams-closed-p timeout-cleanup) :to-be t)))

  (it "reports a child exit before READY with setup output"
    (let* ((cleanup (make-%allocation-protocol-test-cleanup))
           (stdout (make-string-input-stream "compiler failed before READY~%"))
           (stderr (make-string-input-stream "setup stderr~%"))
           (waiter (%allocation-protocol-test-waiter
                    stdout stderr (lambda (fds &key timeout)
                                    (declare (ignore timeout)) fds)
                    (lambda () 0) t))
           (communicated nil)
           (message (%allocation-protocol-error
                     (lambda ()
                       (%run-allocation-protocol-test
                        waiter
                        (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (setf communicated t))
                        cleanup :exited-p t)))))
      (expect message :to-contain "setup failed: child exited before READY")
      (expect message :to-contain "compiler failed before READY")
      (expect message :to-contain "setup stderr")
      (expect (search "startup timed out" message) :to-be nil)
      (expect communicated :to-be nil)
      (expect (%allocation-protocol-test-cleanup-streams-closed-p cleanup) :to-be t)))

  (it "rejects READY received after the startup deadline"
    (let* ((now 0)
           (ready-p nil)
           (cleanup (make-%allocation-protocol-test-cleanup))
           (communicated nil)
           (stdout-output (format nil "late setup output~%~A~%" +allocation-child-ready-marker+))
           (stderr-output "late setup diagnostic~%")
           (stdout-position 0)
           (stderr-position 0)
           (stdout (make-string-input-stream ""))
           (stderr (make-string-input-stream ""))
           (waiter (%allocation-protocol-test-waiter
                    stdout stderr
                    (lambda (fds &key timeout)
                      (declare (ignore fds timeout))
                      (incf now (* 3 internal-time-units-per-second))
                      (setf ready-p t)
                      '(11 12))
                    (lambda () now) nil
                    (lambda (stream eof-error-p eof-value)
                      (declare (ignore eof-error-p))
                      (let* ((stdout-p (eq stream stdout))
                             (content (if stdout-p stdout-output stderr-output))
                             (position (if stdout-p stdout-position stderr-position)))
                        (when ready-p
                          (if (< position (length content))
                              (prog1 (char content position)
                                (if stdout-p
                                    (incf stdout-position)
                                  (incf stderr-position)))
                            eof-value))))))
           (message (%allocation-protocol-error
                     (lambda ()
                       (%run-allocation-protocol-test
                        waiter
                        (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (setf communicated t))
                        cleanup :exited-p nil :startup-timeout 2)))))
      (expect ready-p :to-be t)
      (expect message :to-contain "startup timed out waiting for READY")
      (expect message :to-contain +allocation-child-ready-marker+)
      (expect message :to-contain "late setup output")
      (expect message :to-contain "late setup diagnostic")
      (expect communicated :to-be nil)
      (expect (%allocation-protocol-test-cleanup-process-terminated-p cleanup) :to-be t)
      (expect (%allocation-protocol-test-cleanup-streams-closed-p cleanup) :to-be t)))


  (it "fails a post-GO timeout with exit zero, partial output, and child cleanup"
    (let* ((cleanup (make-%allocation-protocol-test-cleanup))
           (stdout (make-string-input-stream
                    (format nil "setup output~%~A~%" +allocation-child-ready-marker+)))
           (stderr (make-string-input-stream "setup warning~%"))
           (waiter (%allocation-protocol-test-waiter
                    stdout stderr (lambda (fds &key timeout) (declare (ignore timeout)) fds)
                    (lambda () 0) nil))
           (communicated-input nil)
           (communicated-timeout nil)
           (message (%allocation-protocol-error
                     (lambda ()
                       (%run-allocation-protocol-test
                        waiter
                        (lambda (process input timeout)
                          (declare (ignore process))
                          (setf communicated-input input communicated-timeout timeout)
                          (make-%allocation-protocol-test-result
                           :stdout "partial output after GO"
                           :stderr "partial error after GO"
                           :exit-code 0 :timed-out-p t))
                        cleanup :exited-p nil :measurement-timeout 0.25)))))
      (expect message :to-contain "exit code: 0, timed out: T")
      (expect communicated-input :to-equal +allocation-child-go-line+)
      (expect communicated-timeout :to-be 0.25)
      (expect message :to-contain "setup output")
      (expect message :to-contain "setup warning")
      (expect message :to-contain "partial output after GO")
      (expect message :to-contain "partial error after GO")
      (expect (%allocation-protocol-test-cleanup-process-terminated-p cleanup) :to-be t)
      (expect (%allocation-protocol-test-cleanup-streams-closed-p cleanup) :to-be t))))
