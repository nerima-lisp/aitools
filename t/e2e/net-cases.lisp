(in-package #:aitools.e2e.test)

(defvar *net-e2e-binary* nil)
(defparameter +net-e2e-timeout-seconds+ 1)
(defparameter +net-e2e-timeout-margin-seconds+ 1)
(defparameter +net-e2e-watchdog-seconds+ 4)

(defun net-e2e-binary ()
  (or *net-e2e-binary*
      (setf *net-e2e-binary* (build-net-e2e-binary))))

(defun build-net-e2e-binary ()
  (let* ((directory (scratch-path "net-e2e-build/"))
         (target (merge-pathnames "aitools-net-e2e" directory))
         (fasl-directory (merge-pathnames "fasl/" directory))
         (script
           (asdf:system-relative-pathname
            "aitools" "t/e2e/fixtures/net-cli-build.lisp"))
         (result
           (run-process (native sb-ext:*runtime-pathname*)
             (list "--non-interactive"
                   "--no-sysinit"
                   "--no-userinit"
                   "--disable-debugger"
                   "--load"
                   (native script))
             :environment
             (environment-with
              `(("AITOOLS_E2E_NET_CLI_OUTPUT" . ,(native target))
                ("AITOOLS_E2E_NET_CLI_FASL" . ,(native fasl-directory))))
             :timeout-seconds +build-timeout-seconds+)))
    (unless (and (eql (result-exit-code result) 0) (probe-file target))
      (fail "~A"
            (format nil "building the net e2e CLI failed (exit ~A): ~A~%~A"
                    (result-exit-code result)
                    (utf8 (result-stdout result))
                    (utf8 (result-stderr result)))))
    target))

(defmacro define-net-row-case ((row name &key foreign) (workspace) &body body)
  (let ((title (format nil "row ~2,'0D: ~A" row name)))
    `(progn
       (note-row-case ,row ,title ',foreign)
       (describe ,(format nil "aitools e2e row ~2,'0D" row)
         (it ,title
           (with-progress (,title)
             (let ((*current-case* (list ,row ,title))
                   (*binary* (net-e2e-binary)))
               (aitools-binary)
               (call-with-workspace
                (lambda (,workspace)
                  (declare (ignorable ,workspace))
                  ,@body)))))))))

(define-net-row-case (42 "curl and WebFetch GET and HEAD use net get and net head"
                         :foreign ("curl" "WebFetch")) (ws)
  (let ((get-envelope
          (run-ok ws '("net" "get" "https://fixture.invalid/resource")))
        (head-envelope
          (run-ok ws '("net" "head" "https://fixture.invalid/resource"))))
    (expect (jlist (jget get-envelope "lines"))
            :to-equal '("fixture response"))
    (expect (jget head-envelope "headers" 0 "name")
            :to-equal "Content-Type")
    (expect (jget head-envelope "headers" 0 "value")
            :to-equal "text/plain")))

(define-net-row-case (43 "dig and nslookup A and AAAA use net resolve"
                         :foreign ("dig" "nslookup")) (ws)
  (let* ((envelope
           (run-ok ws '("net" "resolve" "fixture.example")
                   :env '(("AITOOLS_E2E_NET_LOOKUP_MODE" . "resolved"))))
         (records (jlist (jget envelope "records"))))
    (expect (mapcar (lambda (record) (jget record "type")) records)
            :to-equal '("A" "AAAA"))
    (expect (mapcar (lambda (record) (jget record "name")) records)
            :to-equal '("fixture.example" "fixture.example"))
    (expect (jget envelope "records" 0 "value") :to-equal "192.0.2.7")))

(describe "aitools net resolver process timeout"
  (it "reports the lookup deadline and exits within its timeout plus margin"
    (with-progress ("net resolve process timeout")
      (let* ((*current-case* (list 0 "net resolve process timeout"))
             (binary (net-e2e-binary)))
        (call-with-workspace
         (lambda (ws)
           (let* ((listener
                    (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
                  (accepted (sb-thread:make-semaphore :count 0))
                  (release (sb-thread:make-semaphore :count 0))
                  (accepted-socket nil)
                  (accept-thread nil)
                  (marker
                    (scratch-path
                     (format nil "net-e2e-lookup-~D" (incf *invocation-counter*))))
                  (started (get-internal-real-time))
                  (result nil))
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
                             :name "aitools e2e stalled loopback resolver"))
                      (setf result
                            (run-process
                             (native binary)
                             (list "net" "resolve" "stall.fixture.invalid"
                                   "--timeout" "1s")
                             :directory (workspace-root ws)
                             :environment
                             (workspace-environment
                              ws
                              `(("AITOOLS_E2E_NET_LOOKUP_MODE" . "stall")
                                ("AITOOLS_E2E_NET_LOOKUP_MARKER" . ,(native marker))
                                ("AITOOLS_E2E_NET_LOOKUP_PORT" . ,(princ-to-string port))))
                             :timeout-seconds +net-e2e-watchdog-seconds+)))
                    (let* ((elapsed-seconds
                             (/ (float (- (get-internal-real-time) started) 1d0)
                                internal-time-units-per-second))
                           (accepted-p
                             (sb-thread:wait-on-semaphore accepted :timeout 1d0))
                           (lookup-started-at
                             (and (probe-file marker)
                                  (with-open-file (stream marker)
                                    (read stream nil nil))))
                           (stderr (utf8 (result-stderr result)))
                           (envelope
                             (json-kit:parse stderr :false-value nil :null-value nil)))
                      (expect accepted-p :to-be t)
                      (expect (integerp lookup-started-at) :to-be t)
                      (expect (result-exit-code result) :to-be 1)
                      (expect (length (result-stdout result)) :to-be 0)
                      (expect (jget envelope "status") :to-equal "error")
                      (expect (jget envelope "error" "code")
                              :to-equal "environment.timeout")
                      (expect (>= elapsed-seconds +net-e2e-timeout-seconds+) :to-be t)
                      (expect (<= elapsed-seconds
                                  (+ +net-e2e-timeout-seconds+
                                     +net-e2e-timeout-margin-seconds+))
                              :to-be t)))
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
               (sb-bsd-sockets:socket-close listener)))))))))
