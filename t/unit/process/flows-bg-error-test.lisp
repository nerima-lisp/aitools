(in-package #:aitools.process.test)

(describe "aitools.process.application flow error continuations"
  (it "reports an invalid bg-log grep pattern before reading files"
    (let ((world (make-fake-world)))
      (multiple-value-bind (kind fields) (bg-logs world "bg-1" :grep "(")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "input.syntax-error")
        (expect (fake-world-reads world) :to-equal '()))))

  (it "reports an invalid bg-stop grace without signalling"
    (let ((world (make-fake-world)))
      (bg-start world)
      (multiple-value-bind (kind fields) (bg-stop world "bg-1" "soon")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "argument.invalid")
        (expect (fake-world-signals world) :to-equal '()))))

  (it "reports an invalid wait timeout after validating the wait condition"
    (let ((world (make-fake-world)))
      (multiple-value-bind (kind fields) (wait-on world :duration "1s" :timeout "soon")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "argument.invalid")
        (expect (fake-world-clock world) :to-be 0))))

  (it "reports an invalid signal grace before enumerating processes"
    (let ((world (make-fake-world)))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.process.application:signal-command/k
                    (make-fake-process-boundaries world) :pattern "sleep" :expect-count 1 :grace "soon")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "argument.invalid")
        (expect (fake-world-signals world) :to-equal '()))))

  (it "maps a bg-log read port error to environment.io"
    (let* ((world (world-with-log "line"))
           (base (make-fake-process-boundaries world))
           (ports (make-test-process-boundaries :process-bg-directory (cl-boundary-kit:boundary-context-require base :process-bg-directory)
                   :process-list-directory (cl-boundary-kit:boundary-context-require base :process-list-directory)
                   :process-read-file-text (cl-boundary-kit:boundary-context-require base :process-read-file-text)
                   :process-read-file-octets
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error 'aitools.process.application:process-port-error :message "read failed"))
                   :process-file-size (cl-boundary-kit:boundary-context-require base :process-file-size)
                   :process-group-alive-p (cl-boundary-kit:boundary-context-require base :process-group-alive-p))))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.process.application:bg-logs/k ports "bg-1")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "environment.io")
        (expect (search "read failed" (field fields :message)) :to-be-truthy)))))
