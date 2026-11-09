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

(describe "aitools.process.application signal request validation"
  (it-each ((() "argument.invalid")
            ((:pid 2101 :pattern "sleep") "argument.invalid")
            ((:pattern "sleep") "argument.invalid")
            ((:pattern "sleep" :expect-count 1 :expect-command "sleep") "argument.invalid")
            ((:signal "NOT-A-SIGNAL") "argument.invalid")
            ((:signal "KILL" :grace "1s") "argument.invalid"))
      "rejects ~S with ~A before contacting process ports"
      (arguments code)
    (let ((world (make-fake-world)))
      (multiple-value-bind (kind fields)
          (apply (function run-flow)
                 (function aitools.process.application:signal-command/k)
                 (make-fake-process-boundaries world)
                 arguments)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal code)
        (expect (repair-commands fields) :not :to-equal nil))
      (expect (fake-world-signals world) :to-equal nil))));


(describe "aitools.process.application background flow state errors"
  (it "reports missing state directories before reading, listing, or signalling"
    (dolist (call (list (list (function aitools.process.application:bg-logs/k) "bg-1")
                       (list (function aitools.process.application:bg-status/k) nil)
                       (list (function aitools.process.application:bg-stop/k) "bg-1")))
      (let ((world (make-fake-world :bg-directory nil)))
        (multiple-value-bind (kind fields)
            (apply (function run-flow) (first call)
                   (make-fake-process-boundaries world) (rest call))
          (expect kind :to-be :error)
          (expect (field fields :code) :to-equal "environment.unavailable")
          (expect (repair-commands fields) :not :to-equal nil))
        (expect (fake-world-reads world) :to-equal nil)
        (expect (fake-world-signals world) :to-equal nil)))))

(describe "aitools.process.application bg-state exit-file race"
  (it "rereads an exit file that appears after the first read"
    (let ((world (make-fake-world))
          (exit-reads 0))
      (bg-start world)
      (let ((ports
              (make-test-process-boundaries
               :process-bg-directory (lambda () #p"/state/ws/bg/")
               :process-read-file-text
               (lambda (path)
                 (if (string= (file-namestring path) "bg-1.exit")
                     (if (= (incf exit-reads) 1) nil (format nil "7~%"))
                     (fake-file world path)))
               :process-group-alive-p (lambda (pid) (declare (ignore pid)) nil))))
        (multiple-value-bind (kind fields)
            (run-flow (function aitools.process.application:bg-status/k) ports "bg-1")
          (expect kind :to-be :ok)
          (let ((item (first (field fields "items"))))
            (expect (json-alist-value item "exit_code") :to-be 7)
            (expect (json-alist-value item "signal") :to-be json-kit:+json-null+))
          (expect exit-reads :to-be 2))))))
