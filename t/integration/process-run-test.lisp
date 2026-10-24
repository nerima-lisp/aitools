(in-package #:aitools.process.integration-test)

(describe "aitools process trace port outcomes (integration)"
  (it "preserves run values while mapping completion statuses"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (let* ((observed nil)
             (ports
               (aitools.process.infrastructure:make-production-process-ports
                :with-span
                (lambda (kind name operation arguments &key outcome-function)
                  (let* ((values (multiple-value-list (apply operation arguments)))
                         (outcome (funcall outcome-function values)))
                    (push (list kind name outcome) observed)
                    (values-list values)))))
             (run-accessor 'aitools.process.application::process-ports-run-program))
        (multiple-value-bind (status outcome)
            (call-port ports run-accessor
                       (list (program-on-path "sh") "-c" "printf child") 5000)
          (expect status :to-be :exited)
          (expect (aitools.process.domain:process-outcome-stdout outcome) :to-equal "child"))
        (multiple-value-bind (status outcome)
            (call-port ports run-accessor (list (program-on-path "sleep") "2") 20)
          (expect status :to-be :timed-out)
          (expect (aitools.process.domain:process-outcome-timed-out outcome) :to-be t))
        (multiple-value-bind (status message program)
            (call-port ports run-accessor (list "aitools-no-such-program-n3") 5000)
          (expect status :to-be :unavailable)
          (expect (starts-with-p "cannot start aitools-no-such-program-n3: " message) :to-be t)
          (expect program :to-equal "aitools-no-such-program-n3"))
        (let ((target (merge-pathnames "occupied.txt" directory)))
          (with-open-file (stream target :direction :output :if-exists :supersede)
            (write-string "existing" stream))
          (expect (multiple-value-list
                   (call-port ports run-accessor (list (program-on-path "sh") "-c" "printf no")
                              5000 :stdout-path target))
                  :to-equal '(:exists)))
        (expect (reverse observed)
                :to-equal '((:child_process "process.run" :ok)
                            (:child_process "process.run" :timeout)
                            (:child_process "process.run" :unavailable)
                            (:child_process "process.run" :error)))))
    :prefix "aitools-process")))

(describe "aitools run (integration)"
  (it "keeps stdin at EOF unless explicitly requested"
    (let ((*standard-input* (make-string-input-stream "not inherited")))
      (multiple-value-bind (code envelope) (invoke '("run" "--" "cat"))
        (expect code :to-be 0)
        (expect (value envelope "exit_code") :to-be 0)
        (expect (lines envelope "stdout" "head") :to-equal nil))
      (expect (read-line *standard-input*) :to-equal "not inherited")))

  (it "forwards explicit stdin without closing the caller's stream"
    (let ((*standard-input* (make-string-input-stream (format nil "stream input~%"))))
      (multiple-value-bind (code envelope) (invoke '("run" "--stdin" "--" "cat"))
        (expect code :to-be 0)
        (expect (value envelope "exit_code") :to-be 0)
        (expect (lines envelope "stdout" "head") :to-equal '("stream input")))
      (expect (open-stream-p *standard-input*) :to-be t)))

  (it "stops feeding an open input pipe when the child exits or times out"
    (dolist (command '(("true") ("sleep" "2")))
      (let ((producer (sb-ext:run-program (program-on-path "sleep") '("20")
                                         :output :stream :wait nil)))
        (unwind-protect
             (let ((*standard-input* (sb-ext:process-output producer))
                   (started (get-internal-real-time)))
               (multiple-value-bind (code envelope)
                   (invoke (append '("run" "--stdin" "--timeout" "100ms" "--") command))
                 (expect code :to-be 0)
                 (expect (value envelope "timed_out") :to-be
                         (if (string= (first command) "sleep") t json-kit:+json-false+)))
               (expect (< (/ (- (get-internal-real-time) started)
                             internal-time-units-per-second) 2) :to-be t)
               (expect (open-stream-p *standard-input*) :to-be t)
               (expect (count "cl-process-kit stdin feeder" (sb-thread:list-all-threads)
                              :key #'sb-thread:thread-name :test #'equal) :to-be 0))
          (sb-ext:process-kill producer 9)
          (sb-ext:process-wait producer)
          (sb-ext:process-close producer)))))

  (it "forwards available UTF-8 input before the caller sends EOF"
    (let ((producer (sb-ext:run-program
                     (program-on-path "sh")
                     '("-c" "printf '日本語\\n'; exec sleep 20")
                     :output :stream :external-format :utf-8 :wait nil)))
      (unwind-protect
           (let ((*standard-input* (sb-ext:process-output producer)))
             (multiple-value-bind (code envelope)
                 (invoke (list* "run" "--stdin" "--timeout" "1s" "--"
                                (sh "IFS= read -r line; printf '%s\\n' \"$line\"")))
               (expect code :to-be 0)
               (expect (value envelope "timed_out") :to-be json-kit:+json-false+)
               (expect (lines envelope "stdout" "head") :to-equal '("日本語")))
             (expect (open-stream-p *standard-input*) :to-be t))
        (sb-ext:process-kill producer 9)
        (sb-ext:process-wait producer)
        (sb-ext:process-close producer))))

  (it "encodes inline stdin as UTF-8 and accepts explicitly empty input"
    (dolist (text '("日本語" ""))
      (multiple-value-bind (code envelope) (invoke (list "run" "--stdin-data" text "--" "cat"))
        (expect code :to-be 0)
        (expect (value envelope "exit_code") :to-be 0)
        (expect (lines envelope "stdout" "head") :to-equal (if (equal text "") nil (list text))))))

  (it "preserves binary stdin-file bytes and resolves it independently of child cwd"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (let* ((input (merge-pathnames "input.bin" directory))
             (child (merge-pathnames "child/" directory))
             (*default-pathname-defaults* directory))
        (ensure-directories-exist child)
        (with-open-file (stream input :direction :output :element-type '(unsigned-byte 8))
          (write-sequence (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(65 0 255)) stream))
        (multiple-value-bind (code envelope)
            (invoke (list "run" "--stdin-file" "input.bin" "--cwd" (namestring child) "--" "od" "-An" "-tx1"))
          (expect code :to-be 0)
          (expect (value envelope "exit_code") :to-be 0)
          (expect (mapcan (lambda (line)
                            (remove "" (uiop:split-string line :separator '(#\Space #\Tab))
                                    :test #'string=))
                          (lines envelope "stdout" "head"))
                  :to-equal '("41" "00" "ff")))))
    :prefix "aitools-process"))

  (it "rejects a stdin FIFO without waiting for a writer"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (let* ((fifo (merge-pathnames "input.fifo" directory))
             (link (merge-pathnames "input-link" directory))
             (started (get-internal-real-time)))
        (sb-posix:mkfifo (namestring fifo) #o600)
        (sb-posix:symlink (namestring fifo) (namestring link))
        (dolist (path (list fifo link directory))
          (multiple-value-bind (code envelope)
              (invoke (list "run" "--stdin-file" (namestring path) "--timeout" "100ms" "--" "cat"))
            (expect code :to-be 1)
            (expect (value envelope "error" "code") :to-equal "environment.io")))
        (expect (/ (- (get-internal-real-time) started) internal-time-units-per-second) :to-be-less-than 2)))
    :prefix "aitools-process"))

  (it "sets child cwd without changing the caller's cwd"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (let ((before (uiop:getcwd)))
        (multiple-value-bind (code envelope)
            (invoke (list "run" "--cwd" (namestring directory) "--" "pwd"))
          (expect code :to-be 0)
          (expect (value envelope "exit_code") :to-be 0)
          (expect (first (lines envelope "stdout" "head")) :to-equal (string-right-trim "/" (namestring directory))))
        (expect (uiop:getcwd) :to-equal before)))
    :prefix "aitools-process"))

  (it "overlays inherited environment with last-wins, empty, and embedded-equals values"
    (let ((before (uiop:getenv "AITOOLS_PROCESS_TEST_VALUE")))
      (multiple-value-bind (code envelope)
          (invoke (list* "run" "--env" "AITOOLS_PROCESS_TEST_VALUE=first"
                         "--env" "AITOOLS_PROCESS_TEST_VALUE=last=value"
                         "--env" "AITOOLS_PROCESS_TEST_EMPTY=" "--"
                         (sh "printf '%s\\n' \"$AITOOLS_PROCESS_TEST_VALUE\" \"${AITOOLS_PROCESS_TEST_EMPTY+x}\" \"${AITOOLS_PROCESS_TEST_EMPTY:-empty}\"; test -n \"$PATH\"")))
        (expect code :to-be 0)
        (expect (value envelope "exit_code") :to-be 0)
        (expect (lines envelope "stdout" "head") :to-equal '("last=value" "x" "empty")))
      (expect (uiop:getenv "AITOOLS_PROCESS_TEST_VALUE") :to-equal before)))

  (it "reports a missing stdin file as environment.io"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (multiple-value-bind (code envelope)
          (invoke (list "run" "--stdin-file" (namestring (merge-pathnames "absent" directory)) "--" "cat"))
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "environment.io")))
    :prefix "aitools-process"))

  (it "prints stdout lines of a real child"
    (multiple-value-bind (code envelope) (invoke (list "run" "--" "printf" (format nil "a~%b~%")))
      (expect code :to-be 0)
      (expect envelope :to-match-envelope "status" "ok")
      (expect (value envelope "status") :to-equal "ok")
      (expect (lines envelope "stdout" "head") :to-equal '("a" "b"))
      (expect (value envelope "exit_code") :to-be 0)))

  (it "reports the child's exit code while itself exiting 0"
    (multiple-value-bind (code envelope) (invoke (list* "run" "--" (sh "exit 3")))
      (expect code :to-be 0)
      (expect (value envelope "exit_code") :to-be 3)))

  (it "reports a child killed by a signal as that signal with a null exit code"
    (multiple-value-bind (code envelope) (invoke (list* "run" "--" (sh "kill -TERM $$")))
      (expect code :to-be 0)
      (expect (value envelope "signal") :to-be 15)
      (expect (value envelope "exit_code") :to-be json-kit:+json-null+)
      (expect (value envelope "timed_out") :to-be json-kit:+json-false+)))

  (it "kills the child's process group at --timeout and still exits 0"
    (multiple-value-bind (code envelope) (invoke (list* "run" "--timeout" "300ms" "--" (sh "sleep 30 & wait")))
      (expect code :to-be 0)
      (expect (value envelope "timed_out") :to-be t)
      (expect (< (value envelope "duration_ms") 10000) :to-be t)))

  (it "keeps the head and tail and counts every line"
    (multiple-value-bind (code envelope)
        (invoke (list* "run" "--head" "2" "--tail" "2" "--"
                       (sh "i=1; while [ $i -le 10 ]; do echo line$i; i=$((i+1)); done")))
      (expect code :to-be 0)
      (expect (lines envelope "stdout" "head") :to-equal '("line1" "line2"))
      (expect (lines envelope "stdout" "tail") :to-equal '("line9" "line10"))
      (expect (value envelope "stdout" "total_lines") :to-be 10)
      (expect (value envelope "stdout" "truncated") :to-be t)))

  (it "greps a middle line that head and tail dropped"
    (multiple-value-bind (code envelope)
        (invoke (list* "run" "--head" "3" "--tail" "3" "--grep" "^ERROR" "--"
                       (sh "i=1; while [ $i -le 200 ]; do if [ $i -eq 100 ]; then echo ERROR here; else echo ok $i; fi; i=$((i+1)); done")))
      (expect code :to-be 0)
      (expect (value envelope "stdout" "matches" 0 "n") :to-be 100)
      (expect (value envelope "stdout" "matches" 0 "text") :to-equal "ERROR here")))

  (it "exits 3 with status partial when matches exceed --grep-limit"
    (multiple-value-bind (code envelope)
        (invoke (list* "run" "--grep" "x" "--grep-limit" "1" "--" (sh "echo x1; echo x2")))
      (expect code :to-be 3)
      (expect (value envelope "status") :to-equal "partial")
      (expect (value envelope "stdout" "total_matches") :to-be 2)))

  (it "masks a secret written to stderr"
    (multiple-value-bind (code envelope)
        (invoke (list* "run" "--" (sh (format nil "echo auth ~A >&2" *dummy-token*))))
      (expect code :to-be 0)
      (expect (lines envelope "stderr" "head") :to-equal '("auth [REDACTED_SECRET]"))
      (expect (value envelope "redactions") :to-be 1)))

  (it "strips ANSI escapes and \\r redraws unless --no-strip-ansi"
    (let ((script "printf '\\033[31mred\\033[0m\\n10%%\\r100%%\\n'"))
      (multiple-value-bind (code envelope) (invoke (list* "run" "--" (sh script)))
        (expect code :to-be 0)
        (expect (lines envelope "stdout" "head") :to-equal '("red" "100%")))
      (multiple-value-bind (code envelope) (invoke (list* "run" "--no-strip-ansi" "--" (sh script)))
        (expect code :to-be 0)
        (expect (first (lines envelope "stdout" "head"))
                :to-equal (format nil "~C[31mred~C[0m" (code-char 27) (code-char 27))))))

  (it "fails with environment.unavailable when the program does not exist"
    (multiple-value-bind (code envelope) (invoke (list "run" "--" "aitools-no-such-program"))
      (expect code :to-be 1)
      (expect (value envelope "error" "code") :to-equal "environment.unavailable")
      (expect (value envelope "error" "repairs" 0 "command") :to-equal "aitools sys tools aitools-no-such-program"))))
