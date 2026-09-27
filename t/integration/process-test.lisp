;;;; t/integration/process-test.lisp
;;;;
;;;; `run` and `wait`, driven through the real cl-cli
;;;; dispatch with production ports and real child processes (sh, sleep,
;;;; printf) in temporary directories. This file defines the package and the
;;;; helpers process-bg-test.lisp (the `bg` suite) also uses.
(in-package #:cl-user)

(defpackage #:aitools.process.integration-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect #:describe-skip-if))

(in-package #:aitools.process.integration-test)

(defparameter *dummy-token* "ghp_abcdefghijklmnopqrstuvwxyz0123456789"
  "A GitHub-token-shaped dummy value in a format redaction detects, not a real secret.")

(defun call-with-temporary-directory (function)
  (let ((directory (uiop:ensure-directory-pathname
                    (merge-pathnames (format nil "aitools-process-~36R/" (random (expt 36 10) (make-random-state t)))
                                     (uiop:temporary-directory)))))
    (ensure-directories-exist directory)
    (unwind-protect (funcall function (truename directory))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defmacro with-temporary-directory ((variable) &body body)
  `(call-with-temporary-directory (lambda (,variable) ,@body)))

(defun invoke (argv &key state-directory cwd)
  "Dispatch `aitools ARGV` against the process commands with production
ports. CWD, when given, wires in a production workspace host whose working
directory is CWD. Returns (VALUES EXIT-CODE ENVELOPE), ENVELOPE parsed from
whichever stream the command wrote."
  (let ((registry (aitools.protocol.application:make-command-registry)))
    (aitools.process.presentation:register-process-commands
     registry
     (aitools.process.infrastructure:make-production-process-ports
      :state-directory-function (and state-directory (lambda () state-directory))
      :workspace-host (and cwd (aitools.workspace.infrastructure:make-host-workspace-host
                                :current-directory (lambda () (string-right-trim "/" (namestring cwd)))))))
    (let* ((app (cl-cli:make-app :name "aitools" :commands (aitools/cli:finalize-app-commands registry)))
           (stdout (make-string-output-stream))
           (stderr (make-string-output-stream))
           (code (aitools/cli:dispatch app registry (cons "aitools" argv) :stdout stdout :stderr stderr))
           (text (let ((out (get-output-stream-string stdout)))
                   (if (plusp (length out)) out (get-output-stream-string stderr)))))
      (values code (json-kit:parse text)))))

(defun value (envelope &rest path)
  "Follow PATH (strings for object keys, integers for array indexes)."
  (reduce (lambda (node key) (if (integerp key) (aref node key) (gethash key node)))
          path :initial-value envelope))

(defun lines (envelope &rest path)
  (coerce (apply #'value envelope path) 'list))

(defun sh (script)
  (list "sh" "-c" script))

;;; ------------------------------------------------------------------ run

(describe "aitools run (integration)"
  (it "prints stdout lines of a real child"
    (multiple-value-bind (code envelope) (invoke (list "run" "--" "printf" (format nil "a~%b~%")))
      (expect code :to-be 0)
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

(defun file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun call-with-workspace (function)
  "Call FUNCTION with (ROOT STATE): a workspace directory (the working
directory the workspace host reports) and a sibling state directory."
  (with-temporary-directory (directory)
    (let ((root (merge-pathnames "ws/" directory))
          (state (merge-pathnames "state/" directory)))
      (ensure-directories-exist root)
      (ensure-directories-exist (merge-pathnames "tmp/" state))
      (funcall function root state))))

(defmacro with-workspace ((root state) &body body)
  `(call-with-workspace (lambda (,root ,state) (declare (ignorable ,state)) ,@body)))

(defun stdout-to (root state target &rest argv)
  (invoke (list* "run" "--stdout-to" target "--" argv) :cwd root :state-directory state))

(describe "aitools run --stdout-to (integration)"
  (it "writes the child's stdout byte for byte to a new file inside the workspace"
    (with-workspace (root state)
      (multiple-value-bind (code envelope) (stdout-to root state "out.txt" "printf" "\\033[31mx\\ny\\n")
        (expect code :to-be 0)
        (expect (value envelope "stdout" "path") :to-equal "out.txt")
        (expect (value envelope "stdout" "bytes") :to-be 9)
        (expect (value envelope "stdout" "head") :to-be nil)
        (expect (coerce (file-octets (merge-pathnames "out.txt" root)) 'list)
                :to-equal '(27 91 51 49 109 120 10 121 10)))))

  (it "refuses an existing file and leaves it unchanged"
    (with-workspace (root state)
      (with-open-file (out (merge-pathnames "out.txt" root) :direction :output)
        (write-string "keep" out))
      (multiple-value-bind (code envelope) (stdout-to root state "out.txt" "printf" "new")
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "refusal.exists"))
      (expect (uiop:read-file-string (merge-pathnames "out.txt" root)) :to-equal "keep")))

  (it "refuses a path outside the root, through a symlink, and inside .git"
    (with-workspace (root state)
      (let ((outside (merge-pathnames "../outside/" root)))
        (ensure-directories-exist outside)
        (sb-posix:symlink (string-right-trim "/" (namestring (truename outside)))
                          (namestring (merge-pathnames "link" root)))
        (ensure-directories-exist (merge-pathnames ".git/" root))
        (with-open-file (out (merge-pathnames ".git/HEAD" root) :direction :output)
          (write-line "ref: refs/heads/main" out))
        (dolist (target '("../outside/a.txt" "link/b.txt" ".git/c.txt"))
          (multiple-value-bind (code envelope) (stdout-to root state target "printf" "x")
            (expect code :to-be 1)
            (expect (value envelope "error" "code") :to-equal "refusal.outside-workspace")))
        (expect (directory (merge-pathnames "*.txt" outside)) :to-equal '())
        (expect (probe-file (merge-pathnames ".git/c.txt" root)) :to-be nil))))

  (it "allows the mktemp area outside the root"
    (with-workspace (root state)
      (let ((target (namestring (merge-pathnames "tmp/run.out" (truename state)))))
        (multiple-value-bind (code envelope) (stdout-to root state target "printf" "ok")
          (expect code :to-be 0)
          (expect (value envelope "stdout" "bytes") :to-be 2))
        (expect (uiop:read-file-string target) :to-equal "ok"))))

  ;; `--root` is a global option the full aitools app adds, so the flow is
  ;; called directly, with the production ports this suite's INVOKE wires.
  (it "fails with environment.io, running nothing, when --root names a missing directory"
    (with-workspace (root state)
      (let* ((marker (merge-pathnames "ran" root))
             (result (aitools.protocol.application:call-with-command-result/k
                      (lambda (&rest continuations)
                        (apply #'aitools.process.application:run-command/k
                               (aitools.process.infrastructure:make-production-process-ports
                                :state-directory-function (lambda () state)
                                :workspace-host (aitools.workspace.infrastructure:make-host-workspace-host
                                                 :current-directory
                                                 (lambda () (string-right-trim "/" (namestring root)))))
                               (aitools.process.application:make-run-request
                                :argv (list "touch" (namestring marker)) :stdout-to "out.txt" :root "gone")
                               continuations))))
             (fields (aitools.protocol.application:command-result-fields result)))
        (expect (aitools.protocol.application:command-result-kind result) :to-be :error)
        (expect (getf fields :code) :to-equal "environment.io")
        (expect (eql 0 (search (format nil "cannot resolve the workspace root (not-found: ~A"
                                       (namestring (merge-pathnames "gone" root)))
                               (getf fields :message)))
                :to-be t)
        (expect (probe-file marker) :to-be nil)))))

;;; ----------------------------------------------------------------- wait

(defun call-with-listener (function)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
           (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen socket 4)
           (funcall function (nth-value 1 (sb-bsd-sockets:socket-name socket))))
      (sb-bsd-sockets:socket-close socket))))

(describe "aitools wait (integration)"
  (it "waits for a line that a child writes to a file later"
    (with-temporary-directory (directory)
      (let ((path (uiop:native-namestring (merge-pathnames "app.log" directory))))
        (process-kit:spawn "/bin/sh" (list "-c" (format nil "sleep 0.3; echo booting > '~A'; echo ready on 8080 >> '~A'" path path)))
        (multiple-value-bind (code envelope)
            (invoke (list "wait" "--file" path "--pattern" "^ready" "--timeout" "10s"))
          (expect code :to-be 0)
          (expect (value envelope "line") :to-equal "ready on 8080")
          (expect (>= (value envelope "elapsed_ms") 200) :to-be t)))))

  (it "resolves a relative --file against the working directory, not the process's"
    (with-temporary-directory (directory)
      (with-open-file (out (merge-pathnames "app.log" directory) :direction :output)
        (format out "booting~%ready on 8080~%"))
      (multiple-value-bind (code envelope)
          (invoke (list "wait" "--file" "app.log" "--pattern" "^ready" "--timeout" "2s") :cwd directory)
        (expect code :to-be 0)
        (expect (value envelope "line") :to-equal "ready on 8080"))))

  (it "waits for a listening TCP port"
    (call-with-listener
     (lambda (port)
       (multiple-value-bind (code envelope) (invoke (list "wait" "--port" (princ-to-string port) "--timeout" "5s"))
         (expect code :to-be 0)
         (expect (value envelope "port") :to-be port)))))

  (it "fails with environment.timeout, exit 1, when nothing listens"
    (let ((port (call-with-listener #'identity)))
      (multiple-value-bind (code envelope) (invoke (list "wait" "--port" (princ-to-string port) "--timeout" "300ms"))
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "environment.timeout"))))

  (it "waits out --duration"
    (multiple-value-bind (code envelope) (invoke (list "wait" "--duration" "200ms"))
      (expect code :to-be 0)
      (expect (>= (value envelope "elapsed_ms") 200) :to-be t))))
