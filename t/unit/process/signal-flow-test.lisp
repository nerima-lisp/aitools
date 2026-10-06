(in-package #:aitools.process.test)

(defun signal-fixture (&key (count 2) ignore-term unsafe missing-info change-on-send disappear-on-send)
  (let ((table (make-hash-table))
        (sent '())
        (clock 0))
    (loop for pid from 2101 below (+ 2101 count)
          do (setf (gethash pid table)
                   (aitools.process.domain:make-process-identity
                    :pid pid :ppid 100 :pgid pid :uid 501 :ruid 501
                    :start pid :command-line (format nil "sleep fixture-~D" pid))))
    (values
     (aitools.process.application:make-process-ports
      :list-pids (lambda () (loop for pid being the hash-keys of table collect pid))
      :process-info (lambda (pid) (unless (eql pid missing-info) (gethash pid table)))
      :safe-target-p (lambda (target)
                       (and (not (eql (aitools.process.domain:process-identity-pid target) unsafe))
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

(describe "aitools signal flow guards"
  (it "refuses PID without a guard and unsafe PID before sending"
    (multiple-value-bind (ports sent) (signal-fixture)
      (dolist (args '((:pid 2101) (:pid 0 :expect-command "sleep")
                      (:pid 1 :expect-command "sleep")
                      (:pid 2101 :expect-command "wrong")
                      (:pid 2101 :expect-start "wrong")))
        (multiple-value-bind (kind fields) (apply #'signal-flow ports args)
          (expect kind :to-be :error)
          (expect (and (member (field fields :code)
                               '("argument.invalid" "refusal.target-changed") :test #'string=) t)
                  :to-be t)))
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

  (it "checks pattern count and all target safety before the first send"
    (multiple-value-bind (ports sent) (signal-fixture :unsafe 2102)
      (multiple-value-bind (kind fields)
          (signal-flow ports :pattern "fixture-" :expect-count 3)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "selection.count-mismatch"))
      (multiple-value-bind (kind fields)
          (signal-flow ports :pattern "fixture-" :expect-count 2)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "refusal.target-changed"))
      (expect (funcall sent) :to-equal '())))

  (it "refuses the whole pattern selection when a listed PID cannot be inspected"
    (multiple-value-bind (ports sent) (signal-fixture :missing-info 2102)
      (multiple-value-bind (kind fields)
          (signal-flow ports :pattern "fixture-" :expect-count 1)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "refusal.target-changed")
        (expect (not (null (search "2102" (field fields :message)))) :to-be t))
      (expect (length (funcall sent)) :to-be 0)))

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
