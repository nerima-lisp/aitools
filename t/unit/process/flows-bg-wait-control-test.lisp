(in-package #:aitools.process.test)

(defun wait-on (world &rest request-arguments)
  (run-flow #'aitools.process.application:wait-command/k (fake-ports world)
            (apply #'aitools.process.application:make-wait-request request-arguments)))

(describe "aitools.process.application wait-command/k"
  (it "waits out --duration"
    (let ((world (make-fake-world)))
      (multiple-value-bind (kind fields) (wait-on world :duration "250ms")
        (expect kind :to-be :ok)
        (expect (field fields "matched") :to-be t)
        (expect (field fields "elapsed_ms") :to-be 250))))

  (it "matches a line once it appears in the file"
    (let ((world (make-fake-world)))
      (push (list* 300 "/tmp/app.log" (format nil "boot~%listening on 8080~%")) (fake-world-scheduled-files world))
      (multiple-value-bind (kind fields) (wait-on world :file "/tmp/app.log" :pattern "listening")
        (expect kind :to-be :ok)
        (expect (field fields "line") :to-equal "listening on 8080")
        (expect (>= (field fields "elapsed_ms") 300) :to-be t))))

  (it "matches an open port"
    (let ((world (make-fake-world :open-ports '(5432))))
      (multiple-value-bind (kind fields) (wait-on world :port 5432)
        (expect kind :to-be :ok)
        (expect (field fields "port") :to-be 5432))))

  (it "matches a bg log line and a bg exit"
    (let ((world (world-with-log (esc (format nil "^[32mready^[0m~%")))))
      (expect (field (nth-value 1 (wait-on world :bg "bg-1" :pattern "^ready$")) "line") :to-equal "ready")
      (%process-exit world 1001 7)
      (expect (field (nth-value 1 (wait-on world :bg "bg-1" :exit t)) "exit_code") :to-be 7)))

  (it "fails with environment.timeout and a longer-timeout repair"
    (let ((world (make-fake-world)))
      (multiple-value-bind (kind fields) (wait-on world :port 9 :timeout "1s")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "environment.timeout")
        (expect (repair-commands fields) :to-equal '("aitools wait --port 9 --timeout 2000ms")))
      (expect (fake-world-clock world) :to-be 1000)))

  (it "adds a bg logs repair when a bg condition times out"
    (let ((world (world-with-log "")))
      (multiple-value-bind (kind fields) (wait-on world :bg "bg-1" :pattern "never" :timeout "200ms")
        (expect kind :to-be :error)
        (expect (repair-commands fields)
                :to-equal '("aitools wait --bg bg-1 --pattern never --timeout 400ms" "aitools bg logs bg-1")))))

  (it-each (((:port 1 :duration "1s") "argument.invalid")
            (() "argument.invalid")
            ((:duration "soon") "argument.invalid")
            ((:file "x" :pattern "(") "input.syntax-error")
            ((:bg "bg-4" :exit t) "input.not-found"))
      "rejects ~S with ~A"
      (arguments code)
    (let ((world (world-with-log "")))
      (multiple-value-bind (kind fields) (apply (function wait-on) world arguments)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal code)
        (when (member :bg arguments)
          (expect (field fields :candidates) :to-equal (list "bg-1")))))))
