(in-package #:aitools.env.test)

(describe "aitools.env.application sys procs"
  (it "lists Darwin processes from ps only, with started from elapsed time"
    (let ((host (darwin-host)))
      (multiple-value-bind (kind fields) (run-flow #'sys-procs/k (make-fake-env-boundaries :host host))
        (expect kind :to-be :ok)
        (expect (fake-host-runs host)
                :to-equal '(("/bin/ps" ("-axww" "-o" "pid=,ppid=,user=,etime=,command="))))
        (let ((last (third (field fields "items"))))
          (expect (object-field last "pid") :to-be 900)
          (expect (object-field last "started") :to-equal "2026-03-08T06:24:53Z")))))

  (it "lists Linux processes from /proc with the same fields"
    (multiple-value-bind (kind fields) (run-flow #'sys-procs/k (linux-proc-boundaries) :pattern "HTTP")
      (expect kind :to-be :ok)
      (expect (field fields "total") :to-be 1)
      (let ((item (first (field fields "items"))))
        (expect (mapcar #'car (aitools.protocol.domain:json-object-members item))
                :to-equal '("pid" "ppid" "user" "command" "started"))
        (expect (object-field item "user") :to-equal "take")
        (expect (object-field item "command") :to-equal "python3 -m http.server")
        (expect (object-field item "started") :to-equal "2023-11-14T22:13:22.500Z"))))

  (it "runs no command at all on Linux"
    (let ((host (make-fake-host)))
      (run-flow #'sys-procs/k (linux-proc-boundaries :host host))
      (expect (fake-host-runs host) :to-equal nil)))

  (it "uses explicit and default process limits"
    (multiple-value-bind (kind fields)
        (run-flow #'sys-procs/k
                  (make-fake-env-boundaries :host (darwin-host))
                  :limit 2)
      (expect kind :to-be :partial)
      (expect (length (field fields "items")) :to-be 2)
      (expect (field fields "total") :to-be 3)
      (expect (field fields "truncated") :to-be t)
      (expect (field fields "next_commands")
              :to-equal (list "aitools sys procs --limit 3")))
    (let* ((output
             (with-output-to-string (stream)
               (loop for pid from 1 to 51
                     do (format stream "~D 1 tester 00:01 worker~D~%" pid pid))))
           (host
             (make-fake-host
              :programs (list (cons "/bin/ps" (exits-with output))))))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-procs/k
                    (make-fake-env-boundaries
                     :identity (list "Darwin" "25.6.0" "arm64")
                     :host host))
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 51)
        (expect (length (field fields "items")) :to-be 50)
        (expect (field fields "truncated") :to-be t)
        (expect (field fields "next_commands")
                :to-equal (list "aitools sys procs --limit 51")))))

  (it "is environment.unavailable without a source"
    (expect (first (nth-value 1 (run-flow #'sys-procs/k (make-fake-env-boundaries))))
            :to-equal "environment.unavailable")))
