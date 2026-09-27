;;;; t/unit/process/flows-test.lisp
;;;;
;;;; The run-command and bg-start flows over the fake world. The bg-logs,
;;;; bg-status, bg-stop and wait flows are in flows-bg-control-test.lisp.
(in-package #:aitools.process.test)

(defun run-request (&rest arguments &key (argv '("make")) &allow-other-keys)
  (apply #'aitools.process.application:make-run-request :argv argv
         (loop for (key value) on arguments by #'cddr unless (eq key :argv) append (list key value))))

(defun run-with (behavior &rest request-arguments)
  (let ((world (make-fake-world :run-behavior behavior)))
    (multiple-value-bind (kind fields)
        (run-flow #'aitools.process.application:run-command/k (fake-ports world)
                  (apply #'run-request request-arguments))
      (values kind fields world))))

(describe "aitools.process.application run-command/k"
  (it "reports the child's exit code as data with exit status ok"
    (multiple-value-bind (kind fields)
        (run-with (list :exited :exit-code 3 :stdout (format nil "a~%b~%") :duration-ms 12))
      (expect kind :to-be :ok)
      (expect (field fields "exit_code") :to-be 3)
      (expect (field fields "timed_out") :to-be json-kit:+json-false+)
      (expect (json-alist-value (field fields "stdout") "head") :to-equal '("a" "b"))))

  (it "reports a timeout as a successful result with timed_out true"
    (multiple-value-bind (kind fields) (run-with (list :timed-out :signal 15))
      (expect kind :to-be :ok)
      (expect (field fields "timed_out") :to-be t)
      (expect (field fields "signal") :to-be 15)
      (expect (field fields "exit_code") :to-be json-kit:+json-null+)))

  (it "passes the parsed --timeout to the runner"
    (multiple-value-bind (kind fields world) (run-with (list :exited :exit-code 0) :timeout "2s")
      (declare (ignore kind fields))
      (expect (fake-world-run-calls world) :to-equal '((("make") 2000)))))

  (it "is partial (exit 3) when --grep matches more lines than --grep-limit"
    (multiple-value-bind (kind fields)
        (run-with (list :exited :exit-code 1 :stderr (format nil "E1~%E2~%E3~%")) :grep "^E" :grep-limit 2)
      (expect kind :to-be :partial)
      (expect (json-alist-value (field fields "stderr") "total_matches") :to-be 3)))

  (it "masks secrets in stderr and counts them"
    (multiple-value-bind (kind fields)
        (run-with (list :exited :exit-code 0 :stderr (format nil "auth: Bearer ~A~%" *dummy-token*)))
      (expect kind :to-be :ok)
      (expect (json-alist-value (field fields "stderr") "head") :to-equal '("auth: Bearer [REDACTED_SECRET]"))
      (expect (field fields "redactions") :to-be 1)))

  (it "reports a program that cannot start as environment.unavailable with a sys tools repair"
    (multiple-value-bind (kind fields) (run-with (list :unavailable) :argv '("no-such-tool"))
      (expect kind :to-be :error)
      (expect (field fields :code) :to-equal "environment.unavailable")
      (expect (repair-commands fields) :to-equal '("aitools sys tools no-such-tool"))))

  (it "refuses --stdout-to with environment.unavailable when no workspace host is wired"
    (multiple-value-bind (kind fields world) (run-with (list :exited :exit-code 0) :stdout-to "out.txt")
      (expect kind :to-be :error)
      (expect (field fields :code) :to-equal "environment.unavailable")
      (expect (fake-world-run-calls world) :to-equal '())))

  (it-each (((:argv nil) "argument.invalid")
            ((:timeout "10") "argument.invalid")
            ((:timeout "0s") "argument.invalid")
            ((:grep "(") "input.syntax-error"))
      "rejects ~S with ~A before running anything"
      (arguments code)
    (multiple-value-bind (kind fields world)
        (apply #'run-with (list :exited :exit-code 0) arguments)
      (expect kind :to-be :error)
      (expect (field fields :code) :to-equal code)
      (expect (repair-commands fields) :not :to-equal nil)
      (expect (fake-world-run-calls world) :to-equal '())))

  (it "reports input.syntax-error when --grep exhausts the matcher's step budget on a line"
    (multiple-value-bind (kind fields)
        (run-with (list :exited :exit-code 0 :stdout (format nil "~Ab~%" (make-string 40 :initial-element #\a)))
                  :grep "^(a+)+\\1$")
      (expect kind :to-be :error)
      (expect (field fields :code) :to-equal "input.syntax-error")
      (expect (repair-commands fields) :to-equal '("aitools schema run")))))

;;; ------------------------------------------------------------------ bg

(defun bg-start (world &optional (argv '("sleep" "60")) name)
  (run-flow #'aitools.process.application:bg-start/k (fake-ports world) argv name))

(defun bg-path (name)
  (merge-pathnames name #p"/state/ws/bg/"))

(describe "aitools.process.application bg-start/k"
  (it "records the process under a fresh id and returns id, pid, and log"
    (let ((world (make-fake-world)))
      (multiple-value-bind (kind fields) (bg-start world '("sleep" "60") "web")
        (expect kind :to-be :ok)
        (expect (field fields "id") :to-equal "bg-1")
        (expect (field fields "pid") :to-be 1001)
        (expect (field fields "log") :to-equal "/state/ws/bg/bg-1.log")
        (let ((record (aitools.process.domain:parse-bg-record (fake-file world (bg-path "bg-1.json")) "bg-1")))
          (expect (aitools.process.domain:bg-record-argv record) :to-equal '("sleep" "60"))
          (expect (aitools.process.domain:bg-record-name record) :to-equal "web")))
      (expect (field (nth-value 1 (bg-start world)) "id") :to-equal "bg-2")))

  (it "skips an id whose log file already exists"
    (let ((world (make-fake-world)))
      (setf (fake-file world (bg-path "bg-1.log")) "")
      (expect (field (nth-value 1 (bg-start world)) "id") :to-equal "bg-2")))

  (it "fails with environment.unavailable, leaving no files, when the launcher is missing"
    (let ((world (make-fake-world :launch-behavior :unavailable)))
      (multiple-value-bind (kind fields) (bg-start world)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "environment.unavailable")
        (expect (repair-commands fields) :to-equal '("aitools sys tools cl-process-kit-spawn"))
        (expect (hash-table-count (fake-world-files world)) :to-be 0))))

  (it "fails with environment.unavailable when no state directory is wired"
    (let ((world (make-fake-world :bg-directory nil)))
      (multiple-value-bind (kind fields) (bg-start world)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "environment.unavailable")
        (expect (fake-world-launches world) :to-equal '()))))

  (it "rejects an empty argv and a control character in --name"
    (let ((world (make-fake-world)))
      (expect (field (nth-value 1 (bg-start world '())) :code) :to-equal "argument.invalid")
      (expect (field (nth-value 1 (bg-start world '("true") (format nil "a~%b"))) :code)
              :to-equal "argument.invalid")
      (expect (fake-world-launches world) :to-equal '())))

  (it "fails with environment.busy, launching nothing, when every ID it tries is already taken"
    (let ((world (make-fake-world :create-fails t)))
      (multiple-value-bind (kind fields) (bg-start world)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "environment.busy")
        (expect (repair-commands fields) :to-equal '("aitools bg start -- sleep 60")))
      (expect (fake-world-launches world) :to-equal '())))

  (it "kills a started process whose record cannot be written, so nothing untracked keeps running"
    (let ((world (make-fake-world :replace-fails t)))
      (multiple-value-bind (kind fields) (bg-start world)
        (expect kind :to-be :error)
        (expect (field fields :code) :to-equal "environment.io")
        (expect (field fields :message)
                :to-equal "writing /state/ws/bg/bg-1.json failed: No space left on device; the process was killed because it could not be recorded")
        (expect (repair-commands fields) :to-equal '("aitools bg status")))
      (expect (fake-world-signals world) :to-equal '((1001 . 9)))
      (expect (getf (gethash 1001 (fake-world-processes world)) :alive) :to-be nil))))
