;;;; These tests exercise `bg` through real cl-cli dispatch with production
;;;; ports and child processes. The first describe group requires process-kit's
;;;; native spawn trampoline and is skipped when unavailable; the no-state-
;;;; directory case below runs without it. `invoke`, `value`, and
;;;; `program-on-path` are in t/integration/process-test.lisp; the package and
;;;; directory helpers are in t/support/process-test-support.lisp.
(in-package #:aitools.process.integration-test)

(defun state-directory (root)
  (merge-pathnames "state/" root))

(defun pid-alive-p (pid)
  (handler-case (progn (sb-posix:kill pid 0) t)
    (sb-posix:syscall-error () nil)))

(defun wait-for-pid-gone (pid &key (timeout-ms 10000))
  "Poll until PID can no longer be signalled (SIGKILL is asynchronous, so the
process is reaped a moment after the group signal). Return T once it is gone,
NIL if TIMEOUT-MS elapses first."
  (loop with deadline = (+ (get-internal-real-time)
                           (floor (* timeout-ms internal-time-units-per-second) 1000))
        when (not (pid-alive-p pid)) return t
        when (>= (get-internal-real-time) deadline) return nil
        do (sleep 0.02)))

(defun wait-for-group-gone (pid &key (timeout-ms 10000))
  "Poll until process group PID has no member left to signal. Return T once
it is empty, NIL if TIMEOUT-MS elapses first. A bg supervisor writes the exit
file as its last act, so an empty group means no bg process of this test can
still write under the state directory."
  (loop with deadline = (+ (get-internal-real-time)
                           (floor (* timeout-ms internal-time-units-per-second) 1000))
        when (not (handler-case (progn (sb-posix:kill (- pid) 0) t)
                    (sb-posix:syscall-error () nil)))
          return t
        when (>= (get-internal-real-time) deadline) return nil
        do (sleep 0.02)))

(defun kill-group (pid)
  "Best-effort SIGKILL of the process group PID leads, so a test never leaks a
bg process it started. Ignores ESRCH and every other signalling error: the
group is normally already gone by the time cleanup runs."
  (ignore-errors (sb-posix:kill (- pid) 9)))

(defun start-bg (root &rest argv)
  (multiple-value-bind (code envelope) (invoke (list* "bg" "start" "--" argv) :state-directory (state-directory root))
    (unless (= code 0)
      (error "bg start failed: ~A" (value envelope "error" "message")))
    envelope))

(defun system-directories ()
  "Source directory of every loaded system, so a child SBCL can find the
same checkouts however this image was configured."
  (remove-duplicates
   (loop for name in (asdf:already-loaded-systems)
         for directory = (ignore-errors (asdf:system-source-directory name))
         when directory collect (namestring directory))
   :test #'string=))

(defparameter +envelope-marker+ "@@aitools-envelope@@")

(defun child-sbcl-start-bg (root argv)
  "Start ARGV as a bg process from a separate SBCL that exits right after,
and return the envelope it printed. Each form is its own --eval so the
child reads aitools's package-qualified symbols only after loading them."
  (flet ((form-text (form)
           ;; Printed from this package so its own symbols (the LET* variables)
           ;; come out unqualified and are read into the child's CL-USER.
           (let ((*package* (find-package :aitools.process.integration-test)))
             (prin1-to-string form))))
    (let* ((setup `(progn
                     (setf asdf:*central-registry* ',(system-directories))
                     ,@(when asdf/output-translations:*output-translations-parameter*
                         `((asdf:initialize-output-translations
                            ',asdf/output-translations:*output-translations-parameter*)))
                     (asdf:load-system "aitools/cli")))
           (start `(let* ((registry (aitools.protocol.application:make-command-registry))
                          (ports (aitools.process.infrastructure:make-production-process-ports
                                  :state-directory-function (lambda () ,(namestring (state-directory root)))))
                          (stdout (make-string-output-stream)))
                     (aitools.process.presentation:register-process-commands registry ports)
                     (let ((code (aitools/cli:dispatch
                                  (cl-cli:make-app :name "aitools"
                                                   :commands (aitools/cli:finalize-app-commands registry))
                                  registry ',(list* "aitools" "bg" "start" "--" argv)
                                  :stdout stdout :stderr stdout)))
                       (write-line ,+envelope-marker+)
                       (write-string (get-output-stream-string stdout))
                       (finish-output)
                       (uiop:quit code))))
           (result (process-kit:run sb-ext:*runtime-pathname*
                                    (list "--core" (namestring sb-ext:*core-pathname*)
                                          "--noinform" "--non-interactive" "--no-sysinit" "--no-userinit"
                                          "--eval" "(require :asdf)"
                                          "--eval" (form-text setup)
                                          "--eval" (form-text start))
                                    :timeout 300))
           (out (process-kit:process-result-stdout result))
           (marker (search +envelope-marker+ out)))
      (unless (and (eql (process-kit:process-result-exit-code result) 0) marker)
        (error "child sbcl failed: ~A ~A" out (process-kit:process-result-stderr result)))
      (json-kit:parse (subseq out (+ marker (length +envelope-marker+)))))))

(describe-skip-if (null (aitools.process.infrastructure:find-spawn-trampoline))
    "aitools bg (integration; skipped without the cl-process-kit-spawn trampoline)"
  (it "keeps running after the aitools process that started it has exited"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((envelope (child-sbcl-start-bg root (list "sleep" "30")))
             (id (value envelope "id"))
             (pid (value envelope "pid")))
        (unwind-protect
             (progn
               (expect (pid-alive-p pid) :to-be t)
               (multiple-value-bind (code status) (invoke (list "bg" "status" id) :state-directory (state-directory root))
                 (expect code :to-be 0)
                 (expect (value status "items" 0 "running") :to-be t)
                 (expect (coerce (value status "items" 0 "argv") 'list) :to-equal '("sleep" "30"))))
          (invoke (list "bg" "stop" id "--grace" "1s") :state-directory (state-directory root)))))
    :prefix "aitools-process"))

  (it "reads only what follows --from and continues from next_offset"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((state (state-directory root))
             (id (value (start-bg root "sh" "-c" "echo one; sleep 0.5; echo two; sleep 30") "id")))
        (unwind-protect
             (progn
               (expect (invoke (list "wait" "--bg" id "--pattern" "^one$" "--timeout" "10s") :state-directory state)
                       :to-be 0)
               (multiple-value-bind (code first) (invoke (list "bg" "logs" id "--from" "0") :state-directory state)
                 (expect code :to-be 0)
                 (expect (lines first "lines") :to-equal '("one"))
                 (expect (value first "next_offset") :to-be 4)
                 (expect (value first "next_commands" 0) :to-equal (format nil "aitools bg logs ~A --from 4" id)))
               (expect (invoke (list "wait" "--bg" id "--pattern" "^two$" "--timeout" "10s") :state-directory state)
                       :to-be 0)
               (multiple-value-bind (code second) (invoke (list "bg" "logs" id "--from" "4") :state-directory state)
                 (expect code :to-be 0)
                 (expect (lines second "lines") :to-equal '("two"))
                 (expect (value second "next_offset") :to-be 8))
               (multiple-value-bind (code grepped) (invoke (list "bg" "logs" id "--grep" "w") :state-directory state)
                 (expect code :to-be 0)
                 (expect (lines grepped "lines") :to-equal '("two"))))
          (invoke (list "bg" "stop" id "--grace" "1s") :state-directory state)))) :prefix "aitools-process"))

  (it "stops with SIGTERM, and with SIGKILL after --grace when TERM is ignored"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((state (state-directory root))
             (polite-envelope (start-bg root "sh" "-c" "echo ready; sleep 30"))
             (polite (value polite-envelope "id"))
             (polite-pid (value polite-envelope "pid"))
             (stubborn-envelope (start-bg root "sh" "-c" "trap '' TERM; echo armed; while :; do sleep 1; done"))
             (stubborn (value stubborn-envelope "id"))
             (stubborn-pid (value stubborn-envelope "pid")))
        ;; Both children (a TERM-ignoring loop among them) are killed by group
        ;; on any exit, so a failed assertion never leaks a bg process.
        (unwind-protect
             (progn
               ;; Stop only once each child is actually running: a stop that
               ;; raced startup would signal a group not yet set up.
               (expect (invoke (list "wait" "--bg" polite "--pattern" "ready" "--timeout" "10s") :state-directory state)
                       :to-be 0)
               (expect (invoke (list "wait" "--bg" stubborn "--pattern" "armed" "--timeout" "10s") :state-directory state)
                       :to-be 0)
               (multiple-value-bind (code envelope) (invoke (list "bg" "stop" polite) :state-directory state)
                 (expect code :to-be 0)
                 (expect (value envelope "stopped") :to-be t)
                 (expect (value envelope "signal") :to-be 15))
               (expect (wait-for-pid-gone polite-pid) :to-be t)
               (let ((started (get-internal-real-time)))
                 (multiple-value-bind (code envelope) (invoke (list "bg" "stop" stubborn "--grace" "400ms") :state-directory state)
                   (expect code :to-be 0)
                   (expect (value envelope "signal") :to-be 9)
                   (expect (>= (- (get-internal-real-time) started) (* 0.4 internal-time-units-per-second)) :to-be t)))
               ;; SIGKILL cannot be trapped: the whole group must really be gone.
               (expect (wait-for-pid-gone stubborn-pid) :to-be t))
          (kill-group polite-pid)
          (kill-group stubborn-pid)))) :prefix "aitools-process"))

  (it "stops a descendant after its target exits but leaves the group alive"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((envelope (start-bg root "sh" "-c" "sleep 30 & exit 0"))
             (id (value envelope "id"))
             (pid (value envelope "pid"))
             (exit-path (merge-pathnames (format nil "bg/~A.exit" id)
                                         (state-directory root))))
        (unwind-protect
             (progn
               (expect (loop repeat 500
                             when (probe-file exit-path) return t
                             do (sleep 0.02))
                       :to-be t)
               (multiple-value-bind (code stopped)
                   (invoke (list "bg" "stop" id "--grace" "100ms")
                           :state-directory (state-directory root))
                 (expect code :to-be 0)
                 (expect (value stopped "stopped") :to-be t))
               (expect (wait-for-group-gone pid) :to-be t))
          (kill-group pid)))) :prefix "aitools-process"))

  (it "reports a bg exit to wait --exit and bg status"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((state (state-directory root))
             (id (value (start-bg root "sh" "-c" "sleep 0.3; exit 4") "id")))
        (multiple-value-bind (code envelope) (invoke (list "wait" "--bg" id "--exit" "--timeout" "10s") :state-directory state)
          (expect code :to-be 0)
          (expect (value envelope "exit_code") :to-be 4))
        (multiple-value-bind (code envelope) (invoke (list "bg" "status") :state-directory state)
          (expect code :to-be 0)
          (expect (value envelope "items" 0 "running") :to-be json-kit:+json-false+)
          (expect (value envelope "items" 0 "exit_code") :to-be 4)))) :prefix "aitools-process"))

  (it "cannot stop a process aitools did not start, by pid or by a forged record"
    (aitools.test.support:call-with-temporary-directory (lambda (root)
      (let* ((state (state-directory root))
             (bystander (process-kit:spawn (program-on-path "sleep") (list "30")))
             (pid (process-kit:process-id bystander))
             (started nil))
        (unwind-protect
             (progn
               (setf started (value (start-bg root "true") "pid"))
               (multiple-value-bind (code envelope) (invoke (list "bg" "stop" (princ-to-string pid)) :state-directory state)
                 (expect code :to-be 1)
                 (expect (value envelope "error" "code") :to-equal "input.not-found"))
               (with-open-file (out (merge-pathnames "bg/bg-9.json" state) :direction :output)
                 (write-string (aitools.process.domain:serialize-bg-record
                                (aitools.process.domain:make-bg-record :id "bg-9" :argv '("sleep" "30") :pid pid
                                                                       :started "2026-01-01T00:00:00Z"))
                               out))
               (multiple-value-bind (code envelope) (invoke (list "bg" "stop" "bg-9" "--grace" "100ms") :state-directory state)
                 (expect code :to-be 0)
                 (expect (value envelope "stopped") :to-be json-kit:+json-false+))
               (expect (process-kit:process-alive-p bystander) :to-be t))
          (process-kit:close-process bystander)
          ;; The `true` bg exits at once, but its supervisor writes bg-N.exit
          ;; afterwards; deleting the state directory before that write lands
          ;; races it ("Directory not empty"). Wait until its group is empty.
          (when started
            (kill-group started)
            (wait-for-group-gone started))))) :prefix "aitools-process")))

(describe "aitools bg without a state directory (integration)"
  (it "fails with environment.unavailable and starts nothing"
    (multiple-value-bind (code envelope) (invoke (list "bg" "start" "--" "sleep" "1"))
      (expect code :to-be 1)
      (expect (value envelope "error" "code") :to-equal "environment.unavailable")
      (expect (plusp (length (value envelope "error" "repairs"))) :to-be t))))

