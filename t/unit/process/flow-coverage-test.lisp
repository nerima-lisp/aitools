(in-package #:aitools.process.test)
(describe "process signal identity coverage"
  (it "rejects targets without readable argv before signalling"
    (multiple-value-bind (boundaries sent)
        (signal-fixture :count 1 :nil-command-line 2101)
      (let ((target
              (funcall
               (cl-boundary-kit:boundary-context-require boundaries
                                                         :process-process-info)
               2101)))
        (expect (aitools.process.application::%signal-alive-p boundaries target)
                :to-be nil))
      (multiple-value-bind (kind fields)
          (signal-flow boundaries :pid 2101 :expect-command "sleep")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "refusal.target-changed"))
      (expect (funcall sent) :to-equal (quote ())))))

(defun flow-coverage-bg-world (text &rest options)
  (let ((world (apply #'make-fake-world options)))
    (run-flow #'aitools.process.application:bg-start/k
              (make-fake-process-boundaries world)
              '("sleep" "60")
              nil)
    (setf (fake-file world #p"/state/ws/bg/bg-1.log") text)
    world))

(defun flow-coverage-signal (ports &rest options)
  (apply #'run-flow #'aitools.process.application:signal-command/k ports options))

(defun flow-coverage-duration-result (text &key (allow-zero t))
  (let ((result nil))
    (aitools.process.application::%call-with-duration-ms
     text "--duration" "coverage"
     :allow-zero allow-zero
     :on-value (lambda (milliseconds)
                 (setf result (list :value milliseconds)))
     :on-error (lambda (code message &key repairs)
                 (declare (ignore message))
                 (setf result (list :error code (not (null repairs))))))
    result))

(describe "aitools.process.application flow coverage"
  (it "expands and executes a process boundary call"
    (let* ((expansion
             (macroexpand-1
              '(aitools.process.application::process-boundary-call
                boundaries :coverage-probe 4)))
           (call (eval (list 'lambda '(boundaries) expansion)))
           (boundaries
             (make-test-process-boundaries
              :coverage-probe (lambda (value) (* value 3)))))
      (expect (funcall call boundaries) :to-be 12)))

  (it "expands reporting handlers for all process boundary conditions"
    (let ((expansion
            (macroexpand-1
             '(aitools.process.application::%reporting-boundary-errors
               "probe" :body))))
      (expect (first expansion) :to-be 'handler-case)
      (expect (mapcar #'first (cddr expansion))
              :to-equal
              '(aitools.process.application::process-target-changed
                aitools.process.application:process-port-error
                aitools.process.domain:invalid-line-pattern))))

  (it "accepts zero durations by default and rejects invalid or disallowed zero values"
    (expect (flow-coverage-duration-result "250ms") :to-equal '(:value 250))
    (expect (flow-coverage-duration-result "0ms") :to-equal '(:value 0))
    (expect (flow-coverage-duration-result "0ms" :allow-zero nil)
            :to-equal '(:error "argument.invalid" t))
    (expect (flow-coverage-duration-result "soon")
            :to-equal '(:error "argument.invalid" t)))

  (it "uses run-request defaults when no fields are supplied"
    (let ((request (aitools.process.application:make-run-request)))
      (expect
       (list (aitools.process.application::run-request-argv request)
             (aitools.process.application::run-request-timeout request)
             (aitools.process.application::run-request-head request)
             (aitools.process.application::run-request-tail request)
             (aitools.process.application::run-request-grep request)
             (aitools.process.application::run-request-grep-limit request)
             (aitools.process.application::run-request-strip-ansi request)
             (aitools.process.application::run-request-stdout-to request)
             (aitools.process.application::run-request-root request)
             (aitools.process.application::run-request-stdin request)
             (aitools.process.application::run-request-stdin-data request)
             (aitools.process.application::run-request-stdin-file request)
             (aitools.process.application::run-request-cwd request)
             (aitools.process.application::run-request-env request))
       :to-equal '(nil "120s" 50 150 nil 50 t nil nil nil nil nil nil nil))))

  (it "uses the default tail and ANSI stripping for background logs"
    (let* ((escape (code-char 27))
           (world (flow-coverage-bg-world
                   (format nil "~C[31mready~C[0m~%" escape escape))))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.process.application:bg-logs/k
                    (make-fake-process-boundaries world) "bg-1")
        (expect kind :to-be :ok)
        (expect (field fields "lines") :to-equal '("ready"))
        (expect (field fields "running") :to-be t))))

  (it "uses bg-stop's default grace before escalating to SIGKILL"
    (let ((world (flow-coverage-bg-world "" :launch-behavior :ignore-term)))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.process.application:bg-stop/k
                    (make-fake-process-boundaries world) "bg-1")
        (expect kind :to-be :ok)
        (expect (field fields "signal") :to-be 9)
        (expect (fake-world-clock world) :to-be 5000)
        (expect (reverse (fake-world-signals world))
                :to-equal '((1001 . 15) (1001 . 9))))))

  (it "maps a target change during background log access to a refusal"
    (let* ((world (flow-coverage-bg-world "line"))
           (base (make-fake-process-boundaries world))
           (ports
             (cl-boundary-kit:boundary-context-with
              base :process-file-size
              (lambda (path)
                (declare (ignore path))
                (error 'aitools.process.application::process-target-changed
                       :message "changed")))))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.process.application:bg-logs/k ports "bg-1")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "refusal.target-changed")
        (expect (field fields :message) :to-equal "changed"))))

  (it "reports a matching-budget failure after reading background log bytes"
    (let ((world
            (flow-coverage-bg-world
             (format nil "~Ab~%" (make-string 40 :initial-element #\a)))))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.process.application:bg-logs/k
                    (make-fake-process-boundaries world) "bg-1"
                    :grep "^(a+)+\\1$")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "input.syntax-error")
        (expect (member "/state/ws/bg/bg-1.log" (fake-world-reads world)
                        :test #'string=)
                :to-be-truthy))))

  (it "uses wait-request defaults when no fields are supplied"
    (let ((request (aitools.process.application:make-wait-request)))
      (expect
       (list (aitools.process.application::wait-request-file request)
             (aitools.process.application::wait-request-pattern request)
             (aitools.process.application::wait-request-port request)
             (aitools.process.application::wait-request-bg request)
             (aitools.process.application::wait-request-exit request)
             (aitools.process.application::wait-request-duration request)
             (aitools.process.application::wait-request-timeout request))
       :to-equal '(nil nil nil nil nil nil "60s"))))

  (it "reports a missing bg directory from wait as unavailable"
    (let ((world (make-fake-world :bg-directory nil)))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.process.application:wait-command/k
                    (make-fake-process-boundaries world)
                    (aitools.process.application:make-wait-request
                     :bg "bg-1" :exit t))
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "environment.unavailable"))))

  (it "reports disappeared and unsafe PID targets without signalling"
    (multiple-value-bind (ports sent)
        (signal-fixture :count 1 :missing-info 2101)
      (multiple-value-bind (kind fields)
          (flow-coverage-signal ports :pid 2101 :expect-command "fixture-2101")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "refusal.target-changed"))
      (expect (funcall sent) :to-equal nil))
    (multiple-value-bind (ports sent)
        (signal-fixture :count 1 :unsafe 2101)
      (multiple-value-bind (kind fields)
          (flow-coverage-signal ports :pid 2101 :expect-command "fixture-2101")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "refusal.target-changed"))
      (expect (funcall sent) :to-equal nil)))

  (it "returns a partial result when a later signal target changes"
    (let* ((targets
             (mapcar
              (lambda (pid)
                (aitools.process.domain:make-process-identity
                 :pid pid :ppid 100 :pgid pid :uid 501 :ruid 501 :start pid
                 :command-line (format nil "worker ~D" pid)))
              '(2101 2102)))
           (sent nil)
           (ports
             (make-test-process-boundaries
              :process-list-pids (lambda () '(2101 2102))
              :process-process-info
              (lambda (pid)
                (find pid targets :key #'aitools.process.domain:process-identity-pid))
              :process-safe-target-p
              (lambda (target)
                (aitools.process.domain:same-process-p
                 target
                 (find (aitools.process.domain:process-identity-pid target)
                       targets :key #'aitools.process.domain:process-identity-pid)))
              :process-signal-process
              (lambda (target number)
                (let ((pid (aitools.process.domain:process-identity-pid target)))
                  (if (= pid 2102)
                      (error 'aitools.process.application::process-target-changed
                             :message "target changed")
                      (progn (push (cons pid number) sent) t)))))))
      (multiple-value-bind (kind fields)
          (flow-coverage-signal ports :pattern "worker" :expect-count 2)
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 2)
        (expect (mapcar (lambda (item) (json-alist-value item "pid"))
                        (field fields "items"))
                :to-equal '(2101)))
      (expect (reverse sent) :to-equal '((2101 . 15)))))

  (it "checks TERM grace and the default signal through the public flow"
    (multiple-value-bind (ports sent) (signal-fixture :count 1)
      (multiple-value-bind (kind fields)
          (flow-coverage-signal ports :pid 2101
                                :expect-command "fixture-2101" :grace "1ms")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 1))
      (expect (funcall sent) :to-equal '((2101 . 15))))
    (multiple-value-bind (ports sent) (signal-fixture :count 0)
      (multiple-value-bind (kind fields)
          (flow-coverage-signal ports :pattern "absent" :expect-count 0 :grace "0ms")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 0))
      (expect (funcall sent) :to-equal nil)))

  (it "rejects unknown signals and grace on non-TERM before process selection"
    (dolist (options '((:pattern "worker" :expect-count 0 :signal "UNKNOWN")
                       (:pattern "worker" :expect-count 0 :signal "KILL" :grace "0ms")))
      (multiple-value-bind (kind fields)
          (apply #'flow-coverage-signal (make-test-process-boundaries) options)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "argument.invalid")))))
