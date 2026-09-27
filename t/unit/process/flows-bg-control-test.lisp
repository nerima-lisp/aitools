;;;; t/unit/process/flows-bg-control-test.lisp
;;;;
;;;; The bg-logs, bg-status, bg-stop and wait flows over the fake world.
(in-package #:aitools.process.test)

(defun bg-logs (world id &rest options)
  (apply #'run-flow #'aitools.process.application:bg-logs/k (fake-ports world) id options))

(defun world-with-log (text &key (behavior :start))
  (let ((world (make-fake-world :launch-behavior behavior)))
    (bg-start world)
    (setf (fake-file world (bg-path "bg-1.log")) text)
    world))

(describe "aitools.process.application bg-logs/k"
  (it "returns the last lines, partial when earlier ones were left out, with a --from continuation"
    (let ((world (world-with-log (numbered-lines 5))))
      (multiple-value-bind (kind fields) (bg-logs world "bg-1" :tail 2)
        (expect kind :to-be :partial)
        (expect (field fields "lines") :to-equal '("line 4" "line 5"))
        (expect (field fields "next_offset") :to-be (length (numbered-lines 5)))
        (expect (field fields "next_commands")
                :to-equal (list (format nil "aitools bg logs bg-1 --from ~D" (length (numbered-lines 5))))))))

  (it "returns only what follows --from"
    (let ((world (world-with-log (format nil "old~%new~%"))))
      (multiple-value-bind (kind fields) (bg-logs world "bg-1" :from 4)
        (expect kind :to-be :ok)
        (expect (field fields "lines") :to-equal '("new"))
        (expect (field fields "next_offset") :to-be 8))))

  (it "rejects --from past the end of the log"
    (let ((world (world-with-log "abc")))
      (multiple-value-bind (kind fields) (bg-logs world "bg-1" :from 99)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "argument.invalid")
        (expect (repair-commands fields) :to-equal '("aitools bg logs bg-1 --from 3")))))

  (it "reports an unknown id as input.not-found with the known ids as candidates"
    (let ((world (world-with-log "")))
      (multiple-value-bind (kind fields) (bg-logs world "bg-9")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "input.not-found")
        (expect (field fields :candidates) :to-equal '("bg-1")))))

  (it "never builds a path from an id outside the id grammar"
    (let ((world (world-with-log "")))
      (setf (fake-world-reads world) '())
      (multiple-value-bind (kind fields) (bg-logs world "../../etc/passwd")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "input.not-found"))
      (expect (fake-world-reads world) :to-equal '())))

  (it "stops a --from read at the per-call byte limit and continues where it stopped"
    (let ((world (world-with-log (numbered-lines 3)))
          (aitools.process.application::+log-read-limit+ 10))
      (%process-exit world 1001 0)
      (multiple-value-bind (kind fields) (bg-logs world "bg-1" :from 0)
        (expect kind :to-be :partial)
        (expect (field fields "lines") :to-equal '("line 1"))
        (expect (field fields "next_offset") :to-be 7))
      (multiple-value-bind (kind fields) (bg-logs world "bg-1" :from 14)
        (expect kind :to-be :ok)
        (expect (field fields "lines") :to-equal '("line 3"))
        (expect (field fields "next_offset") :to-be 21)))))

(defun bg-status (world &optional id)
  (run-flow #'aitools.process.application:bg-status/k (fake-ports world) id))

(describe "aitools.process.application bg-status/k"
  (it "lists running and ended processes with the recorded exit status"
    (let ((world (make-fake-world)))
      (bg-start world '("sleep" "60"))
      (bg-start world '("false"))
      (%process-exit world 1002 1)
      (multiple-value-bind (kind fields) (bg-status world)
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 2)
        (destructuring-bind (first second) (field fields "items")
          (expect (json-alist-value first "running") :to-be t)
          (expect (json-alist-value first "exit_code") :to-be json-kit:+json-null+)
          (expect (json-alist-value second "running") :to-be json-kit:+json-false+)
          (expect (json-alist-value second "exit_code") :to-be 1)))))

  (it "skips a record that no longer parses instead of failing the listing"
    (let ((world (make-fake-world)))
      (bg-start world)
      (bg-start world)
      (setf (fake-file world (bg-path "bg-1.json")) "{\"id\":\"bg-1\"}")
      (multiple-value-bind (kind fields) (bg-status world)
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 1))
      (expect (field (nth-value 1 (bg-status world "bg-1")) :code) :to-equal "environment.io")))

  (it "takes the exit status the supervisor wrote while the liveness probe ran"
    (let ((world (make-fake-world)))
      (bg-start world)
      (setf (getf (gethash 1001 (fake-world-processes world)) :exit-on-probe) 3)
      (multiple-value-bind (kind fields) (bg-status world "bg-1")
        (expect kind :to-be :ok)
        (let ((item (first (field fields "items"))))
          (expect (json-alist-value item "running") :to-be json-kit:+json-false+)
          (expect (json-alist-value item "exit_code") :to-be 3)
          (expect (json-alist-value item "signal") :to-be json-kit:+json-null+))))))

(defun bg-stop (world id &optional (grace "5s"))
  (run-flow #'aitools.process.application:bg-stop/k (fake-ports world) id :grace grace))

(describe "aitools.process.application bg-stop/k"
  (it "sends SIGTERM and reports the signal it recorded before sending"
    (let ((world (make-fake-world)))
      (bg-start world)
      (multiple-value-bind (kind fields) (bg-stop world "bg-1")
        (expect kind :to-be :ok)
        (expect (field fields "stopped") :to-be t)
        (expect (field fields "signal") :to-be 15))
      (expect (fake-world-signals world) :to-equal '((1001 . 15)))))

  (it "sends SIGKILL after the grace period when SIGTERM is ignored"
    (let ((world (make-fake-world :launch-behavior :ignore-term)))
      (bg-start world)
      (multiple-value-bind (kind fields) (bg-stop world "bg-1" "300ms")
        (expect kind :to-be :ok)
        (expect (field fields "signal") :to-be 9))
      (expect (reverse (fake-world-signals world)) :to-equal '((1001 . 15) (1001 . 9)))
      (expect (>= (fake-world-clock world) 300) :to-be t)
      (expect (aitools.process.domain:bg-record-stop-signal
               (aitools.process.domain:parse-bg-record (fake-file world (bg-path "bg-1.json")) "bg-1"))
              :to-be 9)))

  (it "sends nothing to a process that already ended"
    (let ((world (make-fake-world)))
      (bg-start world)
      (%process-exit world 1001 0)
      (multiple-value-bind (kind fields) (bg-stop world "bg-1")
        (expect kind :to-be :ok)
        (expect (field fields "stopped") :to-be json-kit:+json-false+)
        (expect (field fields "exit_code") :to-be 0))
      (expect (fake-world-signals world) :to-equal '())))

  (it "cannot address a process by pid, only by a recorded id"
    (let ((world (make-fake-world)))
      (bg-start world)
      (multiple-value-bind (kind fields) (bg-stop world "1001")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "input.not-found"))
      (expect (fake-world-signals world) :to-equal '())))

  (it "reports environment.io when the group outlives SIGKILL"
    (let ((world (make-fake-world :launch-behavior :ignore-kill)))
      (bg-start world)
      (multiple-value-bind (kind fields) (bg-stop world "bg-1" "100ms")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "environment.io")
        (expect (field fields :message) :to-equal "bg process bg-1 (pid 1001) did not exit after SIGKILL")
        (expect (repair-commands fields) :to-equal '("aitools bg status bg-1")))
      (expect (reverse (fake-world-signals world)) :to-equal '((1001 . 15) (1001 . 9)))
      (expect (>= (fake-world-clock world) 2100) :to-be t))))

;;; ---------------------------------------------------------------- wait

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
      (multiple-value-bind (kind fields) (apply #'wait-on world arguments)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal code)))))
