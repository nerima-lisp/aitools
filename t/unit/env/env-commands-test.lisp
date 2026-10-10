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
  (it "emits bounded network JSON and exit 3"
    (multiple-value-bind (code stdout stderr)
        (dispatch-env (make-fake-ports :identity '("Linux" "test" "generic")
                                      :files (list (cons "/proc/net/dev" *network-linux-text*)))
                      "sys" "network" "--limit" "1")
      (expect code :to-be 3)
      (expect stderr :to-equal "")
      (expect (json-field stdout "source") :to-equal "/proc/net/dev")
      (expect (gethash "rx_bytes" (aref (json-field stdout "items") 0)) :to-be 700)))
  (it "emits disk capacity for the explicit path"
    (multiple-value-bind (code stdout) (dispatch-env (make-fake-ports) "sys" "disk" "/fixture")
      (expect code :to-be 0)
      (expect (json-field stdout "path") :to-equal "/fixture")
      (expect (json-field stdout "units") :to-equal "bytes")
      (expect (json-field stdout "available_bytes") :to-be 400)))
  (it "returns metrics partial JSON with exit 3 and explicit units"
    (multiple-value-bind (code stdout stderr)
        (dispatch-env (make-fake-ports :host (metrics-host)) "sys" "metrics" "--limit" "1")
      (expect code :to-be 3)
      (expect stderr :to-equal "")
      (expect (json-field stdout "sample_interval_ms") :to-be 1000)
      (expect (gethash "rss_bytes" (aref (json-field stdout "items") 0)) :to-be 102400)))
  (it "rejects a zero metrics limit"
    (expect (dispatch-env (make-fake-ports :host (metrics-host)) "sys" "metrics" "--limit" "0") :to-be 1))
  (it "dispatches a non-atomic snapshot and rejects an unsupported overall timeout"
    (multiple-value-bind (code stdout stderr) (dispatch-env (make-fake-ports) "sys" "snapshot" "--port-limit" "1")
      (expect code :to-be 3)
      (expect stderr :to-equal "")
      (expect (json-field stdout "scope") :to-equal "host")
      (expect (hash-table-p (json-field stdout "sections")) :to-be t))
    (expect (dispatch-env (make-fake-ports) "sys" "snapshot" "--timeout" "1s") :to-be 1))
  (it "registers all sys and time commands with schemas"
    (let ((registry (aitools.protocol.application:make-command-registry)))
      (aitools.env.presentation:register-env-commands registry (make-fake-ports))
      (expect (mapcar #'aitools.protocol.domain:command-schema-name
                      (aitools.protocol.application:all-command-schemas registry))
              :to-equal '("sys.info" "sys.env" "sys.tools" "sys.procs" "sys.metrics" "sys.network" "sys.disk" "sys.ports" "sys.snapshot"
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

  (it "returns partial ports and snapshot JSON when one Linux family is unavailable"
    (let ((ports (make-fake-ports :identity *linux-identity*
                                 :files (list (cons "/proc/net/tcp" *proc-net-tcp-text*)))))
      (multiple-value-bind (code stdout stderr) (dispatch-env ports "sys" "ports")
        (expect code :to-be 3)
        (expect stderr :to-equal "")
        (expect (json-field stdout "status") :to-equal "partial")
        (expect (json-field stdout "total") :to-be 2)
        (expect (gethash "ipv6" (json-field stdout "coverage")) :to-equal "source_unavailable")
        (expect (aref (json-field stdout "source_unavailable") 0) :to-equal "/proc/net/tcp6"))
      (multiple-value-bind (code stdout) (dispatch-env ports "sys" "snapshot")
        (expect code :to-be 3)
        (expect (gethash "status" (gethash "ports" (json-field stdout "sections")))
                :to-equal "partial"))))

  (it "runs time diff through dispatch"
    (multiple-value-bind (code stdout)
        (dispatch-env (make-fake-ports) "time" "diff" "2026-03-08T00:00:00Z" "2026-03-08T01:23:00Z")
      (expect code :to-be 0)
      (expect (json-field stdout "human") :to-equal "1h23m")))

  (it "refuses to register a command whose schema data is missing"
    (expect (handler-case (aitools.env.presentation::%schema-data "sys.no-such-command")
              (error (condition) (princ-to-string condition)))
            :to-equal "no env schema data for \"sys.no-such-command\"")))
