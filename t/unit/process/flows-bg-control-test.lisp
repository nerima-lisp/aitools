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

  (it "stops a --from read at the per-call byte limit and continues from the reported offset"
    (let ((world (world-with-log (numbered-lines 3)))
          (aitools.process.application::+log-read-limit+ 10))
      (%process-exit world 1001 0)
      (multiple-value-bind (kind fields) (bg-logs world "bg-1" :from 0)
        (expect kind :to-be :partial)
        (expect (field fields "lines") :to-equal (list "line 1"))
        (expect (field fields "next_offset") :to-be 7)
        (multiple-value-bind (kind fields) (bg-logs world "bg-1" :from (field fields "next_offset"))
          (expect kind :to-be :partial)
          (expect (field fields "lines") :to-equal (list "line 2"))
          (expect (field fields "next_offset") :to-be 14)
          (multiple-value-bind (kind fields) (bg-logs world "bg-1" :from (field fields "next_offset"))
            (expect kind :to-be :ok)
            (expect (field fields "lines") :to-equal (list "line 3"))
            (expect (field fields "next_offset") :to-be 21)))))))

(defun bg-status (world &optional id)
  (run-flow #'aitools.process.application:bg-status/k (fake-ports world) id))

(describe "aitools.process.application bg-status/k"
  (it "lists running and ended processes with the recorded exit status and missing-ID candidates"
    (let ((world (make-fake-world)))
      (bg-start world (list "sleep" "60"))
      (bg-start world (list "false"))
      (%process-exit world 1002 1)
      (multiple-value-bind (kind fields) (bg-status world)
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 2)
        (expect (field fields "observed_total") :to-be 2)
        (expect (field fields "omitted") :to-equal nil)
        (destructuring-bind (first second) (field fields "items")
          (expect (json-alist-value first "running") :to-be t)
          (expect (json-alist-value first "exit_code") :to-be json-kit:+json-null+)
          (expect (json-alist-value second "running") :to-be json-kit:+json-false+)
          (expect (json-alist-value second "exit_code") :to-be 1)))
      (multiple-value-bind (kind fields) (bg-status world "bg-9")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "input.not-found")
        (expect (field fields :candidates) :to-equal (list "bg-1" "bg-2")))))

  (it "reports an invalid record as omitted from a partial listing"
    (let ((world (make-fake-world)))
      (bg-start world)
      (bg-start world)
      (setf (fake-file world (bg-path "bg-1.json")) "{\"id\":\"bg-1\"}")
      (multiple-value-bind (kind fields) (bg-status world)
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 1)
        (expect (field fields "observed_total") :to-be 2)
        (expect (mapcar (lambda (item) (json-alist-value item "id")) (field fields "items"))
                :to-equal '("bg-2"))
        (expect (mapcar #'aitools.protocol.domain:json-object-members (field fields "omitted"))
                :to-equal '((("id" . "bg-1") ("reason" . "invalid-record")))))
      (expect (field (nth-value 1 (bg-status world "bg-1")) :code) :to-equal "environment.io")))

  (it "serializes a partial omitted-record envelope as JSON"
    (let ((world (make-fake-world)))
      (bg-start world)
      (setf (fake-file world (bg-path "bg-1.json")) "{}")
      (multiple-value-bind (kind fields) (bg-status world)
        (expect kind :to-be :partial)
        (let* ((text (with-output-to-string (stream)
                       (aitools.protocol.infrastructure:write-envelope
                        (aitools.protocol.domain:make-ok-envelope "bg.status" fields :status "partial")
                        stream)))
               (envelope (json-kit:parse text))
               (omitted (gethash "omitted" envelope)))
          (expect (gethash "status" envelope) :to-equal "partial")
          (expect (gethash "total" envelope) :to-be 0)
          (expect (gethash "observed_total" envelope) :to-be 1)
          (expect (length omitted) :to-be 1)
          (expect (gethash "id" (aref omitted 0)) :to-equal "bg-1")
          (expect (gethash "reason" (aref omitted 0)) :to-equal "invalid-record")))))

  (it "reports partial rather than an empty successful listing when every record is invalid"
    (let ((world (make-fake-world)))
      (bg-start world)
      (bg-start world)
      (setf (fake-file world (bg-path "bg-1.json")) "{"
            (fake-file world (bg-path "bg-2.json")) "{}")
      (multiple-value-bind (kind fields) (bg-status world)
        (expect kind :to-be :partial)
        (expect (field fields "items") :to-equal '())
        (expect (field fields "total") :to-be 0)
        (expect (field fields "observed_total") :to-be 2)
        (expect (mapcar #'aitools.protocol.domain:json-object-members (field fields "omitted"))
                :to-equal '((("id" . "bg-1") ("reason" . "invalid-record"))
                            (("id" . "bg-2") ("reason" . "invalid-record")))))))

  (it "reports a record removed after directory enumeration as unavailable"
    (let ((world (make-fake-world)))
      (bg-start world)
      (let* ((base (fake-ports world))
             (ports (aitools.process.application:make-process-ports
                     :bg-directory (aitools.process.application::process-ports-bg-directory base)
                     :read-file-text (aitools.process.application::process-ports-read-file-text base)
                     :list-directory
                     (lambda (directory)
                       (prog1 (funcall (aitools.process.application::process-ports-list-directory base)
                                       directory)
                         (remhash (namestring (bg-path "bg-1.json")) (fake-world-files world)))))))
        (multiple-value-bind (kind fields)
            (run-flow #'aitools.process.application:bg-status/k ports nil)
          (expect kind :to-be :partial)
          (expect (field fields "items") :to-equal '())
          (expect (field fields "total") :to-be 0)
          (expect (field fields "observed_total") :to-be 1)
          (expect (mapcar #'aitools.protocol.domain:json-object-members (field fields "omitted"))
                  :to-equal '((("id" . "bg-1") ("reason" . "record-unavailable"))))))
      (multiple-value-bind (kind fields) (bg-status world "bg-1")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "input.not-found"))))

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

  (it "cannot address a process by pid, only by a recorded id and reports candidates"
    (let ((world (make-fake-world)))
      (bg-start world)
      (multiple-value-bind (kind fields) (bg-stop world "1001")
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "input.not-found")
        (expect (field fields :candidates) :to-equal (list "bg-1")))
      (expect (fake-world-signals world) :to-equal nil)))

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

