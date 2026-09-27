;;;; packages/feature/process/src/application/run-flow.lisp
;;;;
;;;; `run -- <argv...>`. The exit code of aitools is 0 whenever the
;;;; child started, timeout included; only a `--grep-limit` overrun
;;;; makes the result partial (exit 3).
(in-package #:aitools.process.application)

(defstruct (run-request (:constructor make-run-request
                            (&key argv (timeout "120s") (head 50) (tail 150) grep (grep-limit 50)
                                  (strip-ansi t) stdout-to root))
                        (:copier nil))
  (argv nil :type list :read-only t)
  (timeout "120s" :type string :read-only t)
  (head 50 :type (integer 0) :read-only t)
  (tail 150 :type (integer 0) :read-only t)
  (grep nil :type (or null string) :read-only t)
  (grep-limit 50 :type (integer 0) :read-only t)
  (strip-ansi t :type boolean :read-only t)
  (stdout-to nil :type (or null string) :read-only t)
  ;; The global `--root` value, or NIL for the git-root-then-cwd rule.
  (root nil :type (or null string) :read-only t))

(defun %report-run-outcome (request pattern outcome stdout-path on-ok on-partial)
  (flet ((summarize (text)
           (aitools.process.domain:summarize-output
            text :head-count (run-request-head request) :tail-count (run-request-tail request)
                 :strip-p (run-request-strip-ansi request) :pattern pattern
                 :grep-limit (run-request-grep-limit request))))
    (let* ((stdout (summarize (aitools.process.domain:process-outcome-stdout outcome)))
           (stderr (summarize (aitools.process.domain:process-outcome-stderr outcome)))
           (fields (aitools.process.domain:run-result-fields outcome stdout stderr :stdout-path stdout-path))
           (limit (run-request-grep-limit request)))
      (if (or (aitools.process.domain:output-report-grep-exceeded-p stdout limit)
              (aitools.process.domain:output-report-grep-exceeded-p stderr limit))
          (funcall on-partial fields)
          (funcall on-ok fields)))))

(defun %absolute-target (host target)
  "TARGET resolved against the working directory, as a user typing a
relative `--stdout-to` means."
  (aitools.workspace.application:user-path-absolute host target))

(defun %call-with-stdout-target (ports root target on-error continuation)
  "Check `--stdout-to` TARGET against the workspace boundary and call CONTINUATION with
(REAL-PATH REPORTED-PATH), or report the refusal through ON-ERROR."
  (let ((host (process-ports-workspace-host ports)))
    (if (null host)
        (funcall on-error "environment.unavailable"
                 "run --stdout-to needs the workspace boundary, which this aitools build did not wire in"
                 :repairs (list (%repair "capture-instead" "Capture stdout in the result instead."
                                         "aitools schema run")))
        (aitools.workspace.application:call-with-resolved-root/k
         host :root root
         :on-error (lambda (reason path)
                     (funcall on-error "environment.io"
                              (format nil "cannot resolve the workspace root (~(~A~): ~A)" reason path)
                              :repairs (list (%schema-repair "run"))))
         :on-resolved
         (lambda (root)
           (aitools.workspace.application:call-with-workspace-boundary/k
            host root (%absolute-target host target)
            :temporary-root (funcall (process-ports-temporary-directory ports))
            :on-inside (lambda (path verdict)
                         (declare (ignore verdict))
                         (funcall continuation
                                  (aitools.kernel.domain:workspace-path-real path)
                                  (aitools.kernel.domain:workspace-path-relative path)))
            :on-outside (lambda (verdict lexical real)
                          (declare (ignore real))
                          (funcall on-error "refusal.outside-workspace"
                                   (format nil "--stdout-to ~A is outside the workspace (~(~A~))" lexical verdict)
                                   :repairs (list (%repair "use-temporary-file"
                                                           "Create a file in aitools's temporary area and write there."
                                                           "aitools mktemp"))))))))))

(defun %run-program (ports request pattern timeout-ms stdout-real stdout-reported on-ok on-partial on-error)
  (flet ((report (outcome)
           (%reporting-port-errors (on-error "run")
             (%report-run-outcome request pattern outcome stdout-reported on-ok on-partial))))
    (%reporting-port-errors (on-error "run")
      (funcall (process-ports-run-program ports) (run-request-argv request) timeout-ms
               :stdout-path stdout-real
               :on-exited #'report
               :on-timed-out #'report
               :on-unavailable (lambda (message program)
                                 (%unavailable-program-error on-error message program))
               :on-exists (lambda ()
                            (funcall on-error "refusal.exists"
                                     (format nil "--stdout-to ~A already exists; run only creates new files"
                                             stdout-reported)
                                     :repairs (list (%repair "inspect-existing" "Inspect the existing file."
                                                             (aitools.process.domain:command-line
                                                              "aitools" "info" stdout-reported)))))))))

(defun run-command/k (ports request &key on-ok on-partial on-error)
  "Run REQUEST's argv through PORTS' RUN-PROGRAM and report `run`'s
fields. Calls exactly one of ON-OK, ON-PARTIAL, or ON-ERROR."
  (if (null (run-request-argv request))
      (funcall on-error "argument.invalid" "run needs the program and its arguments after --"
               :repairs (list (%repair "run-program" "Pass the program after --."
                                       "aitools run -- printf 'hello\\n'")))
      (%call-with-duration-ms
       (run-request-timeout request) "--timeout" "run" on-error
       (lambda (timeout-ms)
         (%call-with-line-pattern
          (run-request-grep request) "run" on-error
          (lambda (pattern)
            (let ((target (run-request-stdout-to request)))
              (if target
                  (%call-with-stdout-target
                   ports (run-request-root request) target on-error
                   (lambda (real reported)
                     (%run-program ports request pattern timeout-ms real reported on-ok on-partial on-error)))
                  (%run-program ports request pattern timeout-ms nil nil on-ok on-partial on-error))))))
       :allow-zero nil)))
