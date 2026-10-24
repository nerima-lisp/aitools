(in-package #:aitools.process.integration-test)

(defun signal-child-script (&key stubborn)
  (format nil "~:[while :; do sleep 1; done~;trap '' TERM; while :; do sleep 1; done~]"
          stubborn))

(defun signal-child (root &key stubborn)
  (let* ((token (format nil "aitools-signal-~36R" (random (expt 36 12) (make-random-state t))))
         (script (signal-child-script :stubborn stubborn))
         (supervisor (value (start-bg root "sh" "-c" script token) "pid")))
    (values supervisor token)))

(defun signal-child-pattern (token &key stubborn)
  (format nil "^/[^ ]*/sh -c ~:[while.*~;trap.*~]~A$" stubborn token))

(defun matching-signal-child (token supervisor)
  (loop repeat 500
        for found = (loop for pid in (aitools.process.infrastructure::%host-list-pids)
                          for info = (aitools.process.infrastructure::%safe-process-info pid)
                          when (and info (/= pid supervisor)
                                    (let ((command-line
                                            (aitools.process.domain:process-identity-command-line info)))
                                      (and (stringp command-line)
                                           (search token command-line))))
                            collect info)
        when (= (length found) 1) return (first found)
        do (sleep 0.02)))

(defun invoke-signal-with-pid-only (pid argv)
  "Run a signal command while its production PID list names only PID.
The process-info and signal-process ports remain the production ports."
  (let ((name 'aitools.process.infrastructure::%host-list-pids))
    (cl-weave:with-mocked-functions
        (((symbol-function name) (lambda () (list pid))))
      (invoke argv))))

#+(or darwin linux)
(describe-skip-if (null (aitools.process.infrastructure:find-spawn-trampoline))
    "aitools signal (integration)"
  (it "reaches a guarded PID and rejects changed guards and protected PIDs"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (multiple-value-bind (supervisor token) (signal-child root)
        (unwind-protect
             (let* ((target (or (matching-signal-child token supervisor) (error "signal child did not appear")))
                    (pid (aitools.process.domain:process-identity-pid target))
                    (pid-text (princ-to-string pid))
                    (self (or (aitools.process.infrastructure::%safe-process-info (sb-posix:getpid))
                              (error "cannot inspect self")))
                    (parent (or (aitools.process.infrastructure::%safe-process-info (sb-posix:getppid))
                                (error "cannot inspect ancestor"))))
               (dolist (bad (list (list "--pid" pid-text "--expect-command" "wrong-token")
                                  (list "--pid" pid-text "--expect-start" "wrong-start")
                                  (list "--pid" "0" "--expect-command" "sh")
                                  (list "--pid" "1" "--expect-command" "sh")
                                  (list "--pid" (princ-to-string (sb-posix:getpid))
                                        "--expect-command"
                                        (aitools.process.domain:process-identity-command-line self))
                                  (list "--pid" (princ-to-string (sb-posix:getppid))
                                        "--expect-command"
                                        (aitools.process.domain:process-identity-command-line parent))))
                 (multiple-value-bind (code envelope) (invoke (cons "signal" bad))
                   (expect code :to-be 2)
                   (expect (value envelope "error" "code") :to-equal "refusal.target-changed"))
                 (expect (pid-alive-p pid) :to-be t))
               (multiple-value-bind (code envelope)
                   (invoke (list "signal" "--pid" pid-text "--expect-command" token))
                 (expect code :to-be 0)
                 (expect (value envelope "total") :to-be 1)))
          (kill-group supervisor)
          (expect (wait-for-group-gone supervisor) :to-be t))))
    :prefix "aitools-process"))

  (it "rejects incomplete or mismatched pattern selection and sends KILL after grace"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (multiple-value-bind (supervisor token) (signal-child root :stubborn t)
        (unwind-protect
             (let* ((target (or (matching-signal-child token supervisor) (error "signal child did not appear")))
                    (pid (aitools.process.domain:process-identity-pid target)))
               (multiple-value-bind (code envelope)
                   (invoke-signal-with-pid-only
                    pid
                    (list "signal" "--pattern" (signal-child-pattern token :stubborn t)
                          "--expect-count" "2"))
                 (expect code :to-be 2)
                 (expect (value envelope "error" "code")
                         :to-equal "selection.count-mismatch"))
               (expect (pid-alive-p pid) :to-be t)
               (let* ((signal-symbol 'aitools.process.infrastructure::%signal-process)
                      (wait-symbol 'aitools.process.application::%wait-for-exit)
                      (original-signal (symbol-function signal-symbol))
                      (original-wait (symbol-function wait-symbol))
                      (observations (make-hash-table :test #'eq))
                      (observer (lambda (phase &rest values)
                                  (setf (gethash phase observations) values)))
                      (started (get-internal-real-time)))
                 (cl-weave:with-mocked-functions
                     (((symbol-function signal-symbol)
                       (lambda (&rest arguments)
                         (let ((signal (second arguments)))
                           (when (member signal '(15 9))
                             (funcall observer (if (= signal 15) :term-sent :kill-sent)
                                      (get-internal-real-time)))
                           (apply original-signal arguments))))
                      ((symbol-function wait-symbol)
                       (lambda (&rest arguments)
                         (let ((decision (apply original-wait arguments)))
                           (funcall observer :wait-for-exit-decision decision)
                           decision))))
                   (multiple-value-bind (code envelope)
                       (invoke (list "signal" "--pid" (princ-to-string pid)
                                     "--expect-command" token
                                     "--grace" "300ms"))
                     (expect code :to-be 0)
                     (expect (value envelope "items" 0 "signal") :to-be 9))
                   (let ((ended (get-internal-real-time)))
                     (handler-case
                         (expect (>= (- ended started)
                                     (* 0.3 internal-time-units-per-second)) :to-be t)
                       (cl-weave::assertion-failure ()
                         (let* ((term-values (gethash :term-sent observations))
                                (term-sent (if term-values (first term-values) :not-recorded))
                                (kill-values (gethash :kill-sent observations))
                                (kill-sent (if kill-values (first kill-values) :not-recorded))
                                (wait-values (gethash :wait-for-exit-decision observations))
                                (wait-decision (if wait-values (first wait-values) :not-recorded))
                                (elapsed (- ended started)))
                           (flet ((relative-ms (timestamp)
                                    (when (numberp timestamp)
                                      (float (/ (* 1000 (- timestamp started))
                                                internal-time-units-per-second)
                                             1d0))))
                             (cl-weave:fail
                              "Grace-duration assertion failed: elapsed=~D ticks (~,3Fms); TERM sent at ~S ticks (~S ms after start); KILL sent at ~S ticks (~S ms after start); %wait-for-exit returned ~S"
                              elapsed (relative-ms ended)
                              term-sent (relative-ms term-sent)
                              kill-sent (relative-ms kill-sent)
                              wait-decision)))))))))
          (kill-group supervisor)
          (expect (wait-for-group-gone supervisor) :to-be t))))
    :prefix "aitools-process"))

  (it "signals its own child through a unique argv marker in pattern mode"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (multiple-value-bind (supervisor token) (signal-child root)
        (unwind-protect
             (let* ((target (or (matching-signal-child token supervisor)
                                (error "signal child did not appear")))
                    (pid (aitools.process.domain:process-identity-pid target)))
               (multiple-value-bind (code envelope)
                   (invoke-signal-with-pid-only
                    pid
                    (list "signal" "--pattern" (signal-child-pattern token)
                          "--expect-count" "1"))
                 (expect code :to-be 0)
                 (expect (value envelope "total") :to-be 1)
                 (expect (value envelope "items" 0 "pid") :to-be pid)
                 (expect (value envelope "items" 0 "signal") :to-be 15)
                 (expect (wait-for-pid-gone pid) :to-be t)))
          (kill-group supervisor)
          (expect (wait-for-group-gone supervisor) :to-be t))))
    :prefix "aitools-process"))

  (it "accepts a start-time guard and sends the host USR1 number"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (multiple-value-bind (supervisor token) (signal-child root)
        (unwind-protect
             (let* ((target (or (matching-signal-child token supervisor)
                                (error "signal child did not appear")))
                    (pid (aitools.process.domain:process-identity-pid target))
                    (start (aitools.process.domain:process-identity-start target)))
               (multiple-value-bind (code envelope)
                   (invoke (list "signal" "--pid" (princ-to-string pid)
                                 "--expect-start" (princ-to-string start) "--signal" "USR1"))
                 (expect code :to-be 0)
                 (expect (value envelope "items" 0 "signal")
                         :to-be #+darwin 30 #+linux 10)))
          (kill-group supervisor)
          (expect (wait-for-group-gone supervisor) :to-be t))))
    :prefix "aitools-process")))
