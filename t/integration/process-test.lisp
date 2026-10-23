;;;; `run` and `wait`, driven through the real cl-cli
;;;; dispatch with production ports and real child processes (sh, sleep,
;;;; printf) in temporary directories. The package and shared temporary
;;;; directory/workspace helpers live in t/support/process-test-support.lisp.
(in-package #:aitools.process.integration-test)
(defparameter *dummy-token* "ghp_abcdefghijklmnopqrstuvwxyz0123456789"
  "A GitHub-token-shaped dummy value in a format redaction detects, not a real secret.")
(defvar *signal-grace-probe-observer* nil)

(defun invoke (argv &key state-directory cwd signal-grace-observer)
  "Dispatch `aitools ARGV` against the process commands with production
ports. CWD, when given, wires in a production workspace host whose working
directory is CWD. Returns (VALUES EXIT-CODE ENVELOPE), ENVELOPE parsed from
whichever stream the command wrote."
  (labels ((observe (phase &optional timestamp)
             (when signal-grace-observer
               (funcall signal-grace-observer phase (or timestamp (get-internal-real-time)))))
           (make-ports-with-observers ()
             (if (null signal-grace-observer)
                 (aitools.process.infrastructure:make-production-process-ports
                  :state-directory-function (and state-directory (lambda () state-directory))
                  :workspace-host (and cwd (aitools.workspace.infrastructure:make-host-workspace-host
                                            :current-directory
                                            (lambda () (string-right-trim "/" (namestring cwd))))))
                 (let* ((signal-symbol 'aitools.process.infrastructure::%signal-process)
                        (clock-symbol 'aitools.process.infrastructure::%monotonic-ms)
                        (original-signal (symbol-function signal-symbol))
                        (original-clock (symbol-function clock-symbol))
                        (term-completed nil)
                        (wait-clock-recorded nil))
                   (cl-weave:with-mocked-functions
                       (((symbol-function signal-symbol)
                         (lambda (&rest arguments)
                           (let ((signal (second arguments)))
                             (prog1 (apply original-signal arguments)
                               (cond
                                 ((eql signal 15)
                                  (setf term-completed (get-internal-real-time))
                                  (setf wait-clock-recorded nil)
                                  (observe :term-completed term-completed))
                                 ((and (eql signal 9) term-completed)
                                  (observe :kill-completed)))))))
                        ((symbol-function clock-symbol)
                         (lambda (&rest arguments)
                           (let ((result (apply original-clock arguments)))
                             (when (and term-completed (not wait-clock-recorded))
                               (setf wait-clock-recorded t)
                               (observe :wait-first-clock-return))
                             result))))
                     (aitools.process.infrastructure:make-production-process-ports
                      :state-directory-function (and state-directory (lambda () state-directory))
                      :workspace-host (and cwd (aitools.workspace.infrastructure:make-host-workspace-host
                                                :current-directory
                                                (lambda () (string-right-trim "/" (namestring cwd)))))))))))
    (let ((invoke-start (get-internal-real-time)))
      (observe :invoke-start invoke-start)
      (let ((registry (aitools.protocol.application:make-command-registry)))
        (observe :registry-created)
        (aitools.process.presentation:register-process-commands registry (make-ports-with-observers))
        (observe :ports-registered)
        (let ((commands (aitools/cli:finalize-app-commands registry)))
          (observe :commands-finalized)
          (let* ((app (cl-cli:make-app :name "aitools" :commands commands))
                 (stdout (make-string-output-stream))
                 (stderr (make-string-output-stream)))
            (observe :app-created)
            (observe :dispatch-start)
            (let* ((code (aitools/cli:dispatch app registry (cons "aitools" argv)
                                               :stdout stdout :stderr stderr))
                   (dispatch-returned (get-internal-real-time))
                   (text (let ((out (get-output-stream-string stdout)))
                           (if (plusp (length out)) out (get-output-stream-string stderr)))))
              (observe :dispatch-returned dispatch-returned)
              (observe :output-extracted)
              (values code (prog1 (json-kit:parse text)
                             (observe :json-parsed))))))))))

(defun value (envelope &rest path)
  "Follow PATH (strings for object keys, integers for array indexes)."
  (reduce (lambda (node key) (if (integerp key) (aref node key) (gethash key node)))
          path :initial-value envelope))

(defun lines (envelope &rest path)
  (coerce (apply #'value envelope path) 'list))

(defun sh (script)
  (list "sh" "-c" script))

(defun program-on-path (name)
  "Return an absolute executable path for SBCL child fixtures. The Nix
sandbox exposes coreutils on PATH rather than at /bin/sleep; keeping NAME as
argv[0] preserves the command name for multi-call executables."
  (or (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
            for candidate = (and (plusp (length directory))
                                 (char= (char directory 0) #\/)
                                 (uiop:native-namestring
                                  (merge-pathnames name (uiop:ensure-directory-pathname directory))))
            when (and candidate
                      (uiop:file-exists-p candidate)
                      (not (uiop:directory-exists-p candidate))
                      (eql 0 (ignore-errors (sb-posix:access candidate sb-posix:x-ok))))
              return candidate)
      (error "~A is not on PATH" name)))

(defun file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun stdout-to (root state target &rest argv)
  (invoke (list* "run" "--stdout-to" target "--" argv) :cwd root :state-directory state))

(describe "aitools run --stdout-to (integration)"
  (it "writes the child's stdout byte for byte to a new file inside the workspace"
    (with-process-workspace (root state)
      (multiple-value-bind (code envelope) (stdout-to root state "out.txt" "printf" "\\033[31mx\\ny\\n")
        (expect code :to-be 0)
        (expect (value envelope "stdout" "path") :to-equal "out.txt")
        (expect (value envelope "stdout" "bytes") :to-be 9)
        (expect (value envelope "stdout" "head") :to-be nil)
        (expect (coerce (file-octets (merge-pathnames "out.txt" root)) 'list)
                :to-equal '(27 91 51 49 109 120 10 121 10)))))

  (it "refuses an existing file and leaves it unchanged"
    (with-process-workspace (root state)
      (with-open-file (out (merge-pathnames "out.txt" root) :direction :output)
        (write-string "keep" out))
      (multiple-value-bind (code envelope) (stdout-to root state "out.txt" "printf" "new")
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "refusal.exists"))
      (expect (uiop:read-file-string (merge-pathnames "out.txt" root)) :to-equal "keep")))

  (it "refuses a path outside the root, through a symlink, and inside .git"
    (with-process-workspace (root state)
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
    (with-process-workspace (root state)
      (let ((target (namestring (merge-pathnames "tmp/run.out" (truename state)))))
        (multiple-value-bind (code envelope) (stdout-to root state target "printf" "ok")
          (expect code :to-be 0)
          (expect (value envelope "stdout" "bytes") :to-be 2))
        (expect (uiop:read-file-string target) :to-equal "ok"))))

  ;; `--root` is a global option the full aitools app adds, so the flow is
  ;; called directly, with the production ports this suite's INVOKE wires.
  (it "fails with environment.io, running nothing, when --root names a missing directory"
    (with-process-workspace (root state)
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
        ;; SPAWN with no :ENVIRONMENT gives the child an empty environment, so
        ;; sh falls back to its built-in PATH; in the Nix build sandbox no
        ;; directory on it holds `sleep`, the sleep failed, and the lines were
        ;; written at once. The child gets this process's environment.
        (process-kit:spawn "/bin/sh" (list "-c" (format nil "sleep 0.3; echo booting > '~A'; echo ready on 8080 >> '~A'" path path))
                           :environment (sb-ext:posix-environ))
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

