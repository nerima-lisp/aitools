(in-package #:aitools.process.test)

(defun signal-fixture (&key (count 2) ignore-term unsafe missing-info nil-command-line
                         change-on-send disappear-on-send)
  (let ((table (make-hash-table))
        (sent '())
        (clock 0))
    (loop for pid from 2101 below (+ 2101 count)
          do (setf (gethash pid table)
                   (aitools.process.domain:make-process-identity
                    :pid pid :ppid 100 :pgid pid :uid 501 :ruid 501
                    :start pid
                    :command-line (unless (eql pid nil-command-line)
                                    (format nil "sleep fixture-~D" pid)))))
    (values
     (aitools.process.application:make-process-ports
      :list-pids (lambda () (loop for pid being the hash-keys of table collect pid))
      :process-info (lambda (pid) (unless (eql pid missing-info) (gethash pid table)))
      :safe-target-p (lambda (target)
                       (and (stringp (aitools.process.domain:process-identity-command-line target))
                            (not (eql (aitools.process.domain:process-identity-pid target) unsafe))
                            (aitools.process.domain:same-process-p
                             target (gethash (aitools.process.domain:process-identity-pid target) table))))
      :signal-process (lambda (target number)
                        (when change-on-send
                          (setf (gethash (aitools.process.domain:process-identity-pid target) table)
                                (aitools.process.domain:make-process-identity
                                 :pid (aitools.process.domain:process-identity-pid target)
                                 :ppid 100 :pgid (aitools.process.domain:process-identity-pid target)
                                 :uid 501 :ruid 501 :start 99999
                                 :command-line "replacement")))
                        (unless (and (not (eql (aitools.process.domain:process-identity-pid target) unsafe))
                                     (aitools.process.domain:same-process-p
                                      target (gethash (aitools.process.domain:process-identity-pid target) table)))
                          (error 'aitools.process.application::process-target-changed
                                 :message "target changed"))
                        (unless disappear-on-send
                          (push (cons (aitools.process.domain:process-identity-pid target) number) sent)
                          (unless (and ignore-term (= number 15))
                            (remhash (aitools.process.domain:process-identity-pid target) table))
                          t))
      :monotonic-ms (lambda () clock)
      :sleep-ms (lambda (milliseconds) (incf clock milliseconds)))
     (lambda () (reverse sent))
     (lambda () clock))))

(defun signal-flow (ports &rest args)
  (apply #'run-flow #'aitools.process.application:signal-command/k ports args))

(defun call-with-fake-signal-process-info (target function)
  (let* ((name 'aitools.process.infrastructure::%safe-process-info)
         (original (symbol-function name))
         (parent (sb-posix:getppid)))
    (unwind-protect
         (progn
           (setf (symbol-function name)
                 (lambda (pid)
                   (cond ((= pid (aitools.process.domain:process-identity-pid target)) target)
                         ((= pid parent)
                          (aitools.process.domain:make-process-identity
                           :pid parent :ppid 1 :pgid 0 :uid 0 :ruid 0
                           :start 0 :command-line "parent")))))
           (funcall function))
      (setf (symbol-function name) original))))

(defun call-signal-port (target)
  (let ((ports (aitools.process.infrastructure:make-production-process-ports)))
    (funcall (aitools.process.application::process-ports-signal-process ports)
             target 15)))

(describe "aitools signal flow guards"
  (it "refuses PID without a guard and unsafe PID before sending"
    (multiple-value-bind (ports sent) (signal-fixture)
      (dolist (case '(((:pid 2101) "argument.invalid")
                      ((:pid 0 :expect-command "sleep") "refusal.target-changed")
                      ((:pid 1 :expect-command "sleep") "refusal.target-changed")
                      ((:pid 2101 :expect-command "wrong") "refusal.target-changed")
                      ((:pid 2101 :expect-start "wrong") "refusal.target-changed")))
        (destructuring-bind (args expected-code) case
          (multiple-value-bind (kind fields) (apply #'signal-flow ports args)
            (expect kind :to-be :error)
            (expect (field fields :code) :to-equal expected-code))))
      (expect (funcall sent) :to-equal '())))

  (it "accepts a zero-count pattern without sending"
    (multiple-value-bind (ports sent) (signal-fixture)
      (multiple-value-bind (kind fields)
          (signal-flow ports :pattern "absent" :expect-count 0)
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 0))
      (expect (funcall sent) :to-equal '())))

  (it "rechecks identity at the signal port after flow preflight"
    (multiple-value-bind (ports sent) (signal-fixture :count 1 :change-on-send t)
      (multiple-value-bind (kind fields)
          (signal-flow ports :pid 2101 :expect-command "fixture-2101")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "refusal.target-changed"))
      (expect (funcall sent) :to-equal '())))

  (it "reports a target that vanishes at the send boundary"
    (multiple-value-bind (ports sent) (signal-fixture :count 1 :disappear-on-send t)
      (multiple-value-bind (kind fields)
          (signal-flow ports :pid 2101 :expect-command "fixture-2101")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "refusal.target-changed"))
      (expect (funcall sent) :to-equal '())))

  (it "rejects a PID below 2 in the signal port before inspecting it"
    (let ((inspected nil)
          (name 'aitools.process.infrastructure::%safe-process-info))
      (let ((original (symbol-function name)))
        (unwind-protect
             (progn
               (setf (symbol-function name)
                     (lambda (pid)
                       (declare (ignore pid))
                       (setf inspected t)
                       nil))
               (signals aitools.process.application::process-target-changed
                 (call-signal-port
                  (aitools.process.domain:make-process-identity
                   :pid 1 :ppid 100 :pgid 100 :uid 501 :ruid 501
                   :start 1 :command-line "sleep fixture-1")))
               (expect inspected :to-be nil))
          (setf (symbol-function name) original)))))

  (it "rejects a signal target with a different UID"
    (let* ((uid (logxor (sb-posix:getuid) 1))
           (target (aitools.process.domain:make-process-identity
                    :pid 2101 :ppid (sb-posix:getppid)
                    :pgid (1+ (sb-posix:getpgid 0))
                    :uid uid :ruid uid :start 2101
                    :command-line "sleep fixture-2101")))
      (call-with-fake-signal-process-info
       target
       (lambda ()
         (signals aitools.process.application::process-target-changed
           (call-signal-port target))))))

  (it "rejects a signal target in the caller's process group"
    (let* ((uid (sb-posix:getuid))
           (pgid (sb-posix:getpgid 0))
           (target (aitools.process.domain:make-process-identity
                    :pid 2101 :ppid (sb-posix:getppid) :pgid pgid
                    :uid uid :ruid uid :start 2101
                    :command-line "sleep fixture-2101")))
      (call-with-fake-signal-process-info
       target
       (lambda ()
         (signals aitools.process.application::process-target-changed
           (call-signal-port target))))))

  (it "checks pattern count after filtering unsafe candidates before the first send"
    (multiple-value-bind (ports sent) (signal-fixture)
      (multiple-value-bind (kind fields)
          (signal-flow ports :pattern "fixture-" :expect-count 3)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "selection.count-mismatch"))
      (expect (funcall sent) :to-equal '()))
    (multiple-value-bind (ports sent) (signal-fixture :unsafe 2102)
      (multiple-value-bind (kind fields)
          (signal-flow ports :pattern "fixture-" :expect-count 2)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "selection.count-mismatch"))
      (expect (funcall sent) :to-equal '())))

  (it "skips a PID that disappears and signals the surviving expected-count candidates"
    (multiple-value-bind (ports sent) (signal-fixture :missing-info 2102)
      (multiple-value-bind (kind fields)
          (signal-flow ports :pattern "fixture-" :expect-count 1)
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 1)
        (let ((item (elt (field fields "items") 0)))
          (expect (json-alist-value item "pid") :to-be 2101)
          (expect (json-alist-value item "signal") :to-be 15)))
      (expect (funcall sent) :to-equal '((2101 . 15)))))

  (it "rejects a pattern candidate whose same-UID argv cannot be read"
    (multiple-value-bind (ports sent) (signal-fixture :nil-command-line 2102)
      (multiple-value-bind (kind fields)
          (signal-flow ports :pattern "fixture-" :expect-count 1)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "refusal.target-changed")
        (expect (search "2102" (field fields :message)) :to-be-truthy))
      (expect (funcall sent) :to-equal '())))

  (it "reports every signalled pattern target in the total"
    (multiple-value-bind (ports sent) (signal-fixture)
      (multiple-value-bind (kind fields)
          (signal-flow ports :pattern "fixture-" :expect-count 2)
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 2)
        (expect (length (field fields "items")) :to-be 2))
      (expect (length (funcall sent)) :to-be 2)))

  (it "sends TERM then KILL only after grace to a surviving target"
    (multiple-value-bind (ports sent clock) (signal-fixture :count 1 :ignore-term t)
      (multiple-value-bind (kind fields)
          (signal-flow ports :pid 2101 :expect-command "fixture-2101" :grace "300ms")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 1))
      (expect (funcall sent) :to-equal '((2101 . 15) (2101 . 9)))
      (expect (>= (funcall clock) 300) :to-be t)))

  (it "uses the host's SIGUSR numbers"
    (expect (aitools.process.domain:signal-number "USR1")
            :to-be #+darwin 30 #+linux 10)
    (expect (aitools.process.domain:signal-number "USR2")
            :to-be #+darwin 31 #+linux 12)))
