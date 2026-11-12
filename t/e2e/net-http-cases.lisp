(in-package #:aitools.e2e.test)

(progn
  (defun net-e2e-call-with-stalled-http-server (function)
    (let* ((listener
             (make-instance 'sb-bsd-sockets:inet-socket
                            :type :stream :protocol :tcp))
           (accepted (sb-thread:make-semaphore :count 0))
           (release (sb-thread:make-semaphore :count 0))
           (accepted-socket nil)
           (accept-thread nil))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
             (sb-bsd-sockets:socket-listen listener 1)
             (let ((port (nth-value 1 (sb-bsd-sockets:socket-name listener))))
               (setf accept-thread
                     (sb-thread:make-thread
                      (lambda ()
                        (setf accepted-socket
                              (sb-bsd-sockets:socket-accept listener))
                        (sb-thread:signal-semaphore accepted)
                        (sb-thread:wait-on-semaphore release))
                      :name "aitools e2e stalled loopback HTTP server"))
               (funcall function port accepted)))
        (sb-thread:signal-semaphore release)
        (when accept-thread
          (when (eq (sb-thread:join-thread accept-thread
                                           :timeout 1d0
                                           :default :timeout)
                    :timeout)
            (sb-thread:terminate-thread accept-thread)
            (sb-thread:join-thread accept-thread)))
        (when accepted-socket
          (sb-bsd-sockets:socket-close accepted-socket))
        (sb-bsd-sockets:socket-close listener))))

  (describe "aitools net HTTP acceptance"
    (cl-weave:it-each
     (("line count" "lines" ("--max-lines" "1") 5 "--range 2:2")
      ("line bytes" "long-line" nil 16384 "--range 1:1 --line-offset 16384"))
     "returns exit 3 and a continuation when GET output reaches the ~A limit"
     (limit response-mode options line-length continuation)
     (with-progress ((format nil "net get ~A partial output" limit))
       (let ((*current-case* (list 0 (format nil "net get ~A partial output" limit)))
             (*binary* (net-e2e-binary)))
         (aitools-binary)
         (call-with-workspace
          (lambda (ws)
            (let* ((result
                     (aitools
                      ws
                      (append
                       '("net" "get" "https://fixture.invalid/resource")
                       options)
                      :env (list (cons "AITOOLS_E2E_NET_RESPONSE_MODE"
                                       response-mode))))
                   (envelope (aitools-envelope result))
                   (commands (jlist (jget envelope "next_commands"))))
              (expect (aitools-exit-code result) :to-be 3)
              (expect (jget envelope "status") :to-equal "partial")
              (expect (jget envelope "truncated") :to-be t)
              (expect (length (jlist (jget envelope "lines"))) :to-be 1)
              (expect (length (jget envelope "lines" 0)) :to-be line-length)
              (expect (search continuation (first commands)) :to-be-truthy))))))
     :prefix "net-http-partial")

    (cl-weave:it-each
     (("GET" "get") ("HEAD" "head"))
     "reports the deadline and environment.unavailable for ~A"
     (method command)
     (with-progress ((format nil "net ~A timeout and unavailable" command))
       (let ((*current-case* (list 0 (format nil "net ~A timeout and unavailable" command)))
             (*binary* (net-e2e-binary)))
         (aitools-binary)
         (call-with-workspace
          (lambda (ws)
            (net-e2e-call-with-stalled-http-server
             (lambda (port accepted)
               (let* ((started (get-internal-real-time))
                      (result
                        (run-process
                         (native *binary*)
                         (list "net" command
                               (format nil "http://stall.fixture.invalid:~D/resource" port)
                               "--timeout" "1s")
                         :directory (workspace-root ws)
                         :environment
                         (workspace-environment
                          ws
                          '(("AITOOLS_E2E_NET_HTTP_MODE" . "transport")
                            ("AITOOLS_E2E_NET_LOOKUP_MODE" . "loopback")))
                         :timeout-seconds +net-e2e-watchdog-seconds+))
                      (elapsed-seconds
                        (/ (float (- (get-internal-real-time) started) 1d0)
                           internal-time-units-per-second))
                      (accepted-p
                        (sb-thread:wait-on-semaphore accepted :timeout 1d0))
                      (envelope
                        (json-kit:parse (utf8 (result-stderr result))
                                        :false-value nil :null-value nil)))
                 (expect accepted-p :to-be-truthy)
                 (expect (result-exit-code result) :to-be 1)
                 (expect (length (result-stdout result)) :to-be 0)
                 (expect (jget envelope "status") :to-equal "error")
                 (expect (jget envelope "error" "code")
                         :to-equal "environment.timeout")
                 (expect (>= elapsed-seconds +net-e2e-timeout-seconds+) :to-be t)
                 (expect (<= elapsed-seconds
                             (+ +net-e2e-timeout-seconds+
                                +net-e2e-timeout-margin-seconds+))
                         :to-be t))))
            (let* ((result
                     (aitools
                      ws
                      (list "net" command
                            "http://stall.fixture.invalid/resource")
                      :env '(("AITOOLS_E2E_NET_HTTP_MODE" . "unavailable")
                             ("AITOOLS_E2E_NET_LOOKUP_MODE" . "loopback"))))
                   (envelope (aitools-envelope result)))
              (expect (aitools-exit-code result) :to-be 1)
              (expect (aitools-stream result) :to-be :stderr)
              (expect (jget envelope "status") :to-equal "error")
              (expect (jget envelope "error" "code")
                      :to-equal "environment.unavailable"))))))
     :prefix "net-http-failure")))
