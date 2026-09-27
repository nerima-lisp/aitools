;;;; t/unit/env/env-commands-test.lisp
;;;;
;;;; The `sys` and `time` commands through cl-cli parsing and the composition
;;;; root's dispatch, against fake ports: argv in, one JSON envelope and an
;;;; exit code out.
(in-package #:aitools.env.test)

(defun dispatch-env (ports &rest argv)
  "(VALUES EXIT-CODE STDOUT STDERR) for `aitools ARGV...`."
  (let ((registry (aitools.protocol.application:make-command-registry)))
    (aitools.env.presentation:register-env-commands registry ports)
    (let ((app (cl-cli:make-app :name "aitools" :version "0.0.0" :require-command t
                                :commands (aitools/cli:finalize-app-commands registry)))
          (stdout (make-string-output-stream))
          (stderr (make-string-output-stream)))
      (values (aitools/cli:dispatch app registry (cons "aitools" argv) :stdout stdout :stderr stderr)
              (get-output-stream-string stdout)
              (get-output-stream-string stderr)))))

(defun json-field (text name)
  (let ((document (json-kit:parse text)))
    (gethash name document)))

(describe "aitools.env.presentation commands"
  (it "registers all eight sys and time commands with schemas"
    (let ((registry (aitools.protocol.application:make-command-registry)))
      (aitools.env.presentation:register-env-commands registry (make-fake-ports))
      (expect (mapcar #'aitools.protocol.domain:command-schema-name
                      (aitools.protocol.application:all-command-schemas registry))
              :to-equal '("sys.info" "sys.env" "sys.tools" "sys.procs" "sys.ports"
                          "time.now" "time.convert" "time.diff"))))

  (it "converts across the DST boundary end to end"
    (multiple-value-bind (code stdout stderr)
        (dispatch-env (make-fake-ports) "time" "convert" "2026-03-08T01:30:00"
                      "--tz" "America/New_York" "--add" "1h")
      (expect code :to-be 0)
      (expect stderr :to-equal "")
      (expect (json-field stdout "result") :to-equal "2026-03-08T03:30:00-04:00")
      (expect (json-field stdout "command") :to-equal "time convert")))

  (it "keeps Asia/Tokyo readable in the written envelope"
    (multiple-value-bind (code stdout) (dispatch-env (make-fake-ports) "time" "now" "--tz" "Asia/Tokyo")
      (expect code :to-be 0)
      (expect (json-field stdout "timezone") :to-equal "Asia/Tokyo")))

  (it "writes input.syntax-error to stderr with exit 1"
    (multiple-value-bind (code stdout stderr) (dispatch-env (make-fake-ports) "time" "convert" "not-a-time")
      (expect code :to-be 1)
      (expect stdout :to-equal "")
      (expect (gethash "code" (json-field stderr "error")) :to-equal "input.syntax-error")))

  (it "rejects an unknown --to value as argument.invalid"
    (expect (dispatch-env (make-fake-ports) "time" "convert" "now" "--to" "rfc2822") :to-be 1))

  (it "masks secret-named variables in sys env output"
    (multiple-value-bind (code stdout)
        (dispatch-env (make-fake-ports :environment '(("API_TOKEN" . "abc") ("EDITOR" . "vi"))) "sys" "env")
      (expect code :to-be 0)
      (expect (search "abc" stdout) :to-be nil)
      (expect (search "[REDACTED_SECRET]" stdout) :to-be-truthy)))

  (it "exits 3 with a partial envelope when sys procs is cut at --limit"
    (multiple-value-bind (code stdout) (dispatch-env (make-fake-ports :host (darwin-host)) "sys" "procs" "--limit" "1")
      (expect code :to-be 3)
      (expect (json-field stdout "status") :to-equal "partial"))))

(describe "aitools.env.presentation commands end to end"
  (it "runs sys info through dispatch"
    (multiple-value-bind (code stdout) (dispatch-env (make-fake-ports :host (darwin-host)) "sys" "info")
      (expect code :to-be 0)
      (expect (json-field stdout "command") :to-equal "sys info")
      (expect (json-field stdout "cpus") :to-be 16)))

  (it "passes sys tools names and --timeout to the flow"
    (let* ((seen nil)
           (host (make-fake-host :programs (list (cons "/b/git" (lambda (arguments timeout)
                                                                  (setf seen (list arguments timeout))
                                                                  (values :exited 0 (format nil "git version 2.55.0~%") "")))))))
      (multiple-value-bind (code stdout)
          (dispatch-env (make-fake-ports :environment '(("PATH" . "/b")) :executables '("/b/git") :host host)
                        "sys" "tools" "git" "--timeout" "2s")
        (expect code :to-be 0)
        (expect (json-field stdout "total") :to-be 1)
        (expect seen :to-equal '(("--version") 2)))))

  (it "runs sys ports through dispatch"
    (multiple-value-bind (code stdout) (dispatch-env (make-fake-ports :host (darwin-host)) "sys" "ports")
      (expect code :to-be 0)
      (expect (json-field stdout "total") :to-be 4)))

  (it "runs time diff through dispatch"
    (multiple-value-bind (code stdout)
        (dispatch-env (make-fake-ports) "time" "diff" "2026-03-08T00:00:00Z" "2026-03-08T01:23:00Z")
      (expect code :to-be 0)
      (expect (json-field stdout "human") :to-equal "1h23m")))

  (it "refuses to register a command whose schema data is missing"
    (expect (handler-case (aitools.env.presentation::%schema-data "sys.no-such-command")
              (error (condition) (princ-to-string condition)))
            :to-equal "no env schema data for \"sys.no-such-command\"")))
