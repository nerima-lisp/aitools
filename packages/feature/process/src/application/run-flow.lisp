;;;; packages/feature/process/src/application/run-flow.lisp
;;;;
;;;; `run -- <argv...>`. The exit code of aitools is 0 whenever the
;;;; child started, timeout included; only a `--grep-limit` overrun
;;;; makes the result partial (exit 3).
(in-package #:aitools.process.application)

(defstruct (run-request (:constructor make-run-request
                            (&key argv (timeout "120s") (head 50) (tail 150) grep (grep-limit 50)
                                  (strip-ansi t) stdout-to root stdin stdin-data stdin-file cwd env))
                        (:copier nil))
  (argv nil :type list :read-only t)
  (timeout "120s" :type string :read-only t)
  (head 50 :type (integer 0) :read-only t)
  (tail 150 :type (integer 0) :read-only t)
  (grep nil :type (or null string) :read-only t)
  (grep-limit 50 :type (integer 0) :read-only t)
  (strip-ansi t :type boolean :read-only t)
  (stdout-to nil :type (or null string) :read-only t)
  (stdin nil :type boolean :read-only t)
  (stdin-data nil :type (or null string) :read-only t)
  (stdin-file nil :type (or null string) :read-only t)
  (cwd nil :type (or null string) :read-only t)
  (env nil :type list :read-only t)
  ;; The global `--root` value, or NIL for the git-root-then-cwd rule.
  (root nil :type (or null string) :read-only t))

(aitools.kernel.domain:define-flow
    (%report-run-outcome request pattern outcome stdout-path &key on-ok on-partial)
  (flet ((summarize (text)
           (aitools.process.domain:summarize-output
            text
            :head-count (run-request-head request)
            :tail-count (run-request-tail request)
            :strip-p (run-request-strip-ansi request)
            :pattern pattern
            :grep-limit (run-request-grep-limit request))))
    (let* ((stdout
             (summarize (aitools.process.domain:process-outcome-stdout outcome)))
           (stderr
             (summarize (aitools.process.domain:process-outcome-stderr outcome)))
           (fields
             (aitools.process.domain:run-result-fields
              outcome stdout stderr :stdout-path stdout-path))
           (limit (run-request-grep-limit request)))
      (if (or (aitools.process.domain:output-report-grep-exceeded-p stdout limit)
              (aitools.process.domain:output-report-grep-exceeded-p stderr limit))
          (aitools.kernel.domain:flow-continue :on-partial fields)
          (aitools.kernel.domain:flow-continue :on-ok fields)))))

(defun %absolute-target (host target)
  "TARGET resolved against the working directory, as a user typing a
relative `--stdout-to` means."
  (aitools.workspace.application:user-path-absolute host target))

(aitools.kernel.domain:define-flow
    (%call-with-stdout-target ports root target &key on-error on-resolved)
  "Check TARGET against the workspace boundary and call ON-RESOLVED with the real and reported paths, or report refusal through ON-ERROR."
  (let ((host (process-ports-workspace-host ports)))
    (if (null host)
        (aitools.kernel.domain:flow-continue
         :on-error
         "environment.unavailable"
         "run --stdout-to needs the workspace boundary, which this aitools build did not wire in"
         :repairs
         (list (repair "capture-instead"
                       "Capture stdout in the result instead."
                       "aitools schema run")))
        (aitools.kernel.domain:flow-call
            (aitools.workspace.application:call-with-resolved-root/k host :root root)
          (:on-error (reason path)
            (aitools.kernel.domain:flow-continue
             :on-error
             "environment.io"
             (format nil "cannot resolve the workspace root (~(~A~): ~A)" reason path)
             :repairs (list (%schema-repair "run"))))
          (:on-resolved (resolved-root)
            (aitools.kernel.domain:flow-call
                (aitools.workspace.application:call-with-workspace-boundary/k
                 host resolved-root (%absolute-target host target)
                 :temporary-root
                 (funcall (process-ports-temporary-directory ports)))
              (:on-inside (path verdict)
                (declare (ignore verdict))
                (aitools.kernel.domain:flow-continue
                 :on-resolved
                 (aitools.kernel.domain:workspace-path-real path)
                 (aitools.kernel.domain:workspace-path-relative path)))
              (:on-outside (verdict lexical real)
                (declare (ignore real))
                (aitools.kernel.domain:flow-continue
                 :on-error
                 "refusal.outside-workspace"
                 (format nil "--stdout-to ~A is outside the workspace (~(~A~))" lexical verdict)
                 :repairs
                 (list (repair "use-temporary-file"
                               "Create a file in the temporary area and write there."
                               "aitools mktemp"))))))))))

(aitools.kernel.domain:define-flow
    (%run-program ports request pattern timeout-ms stdout-real stdout-reported
                  &key on-ok on-partial on-error)
  (labels ((report (outcome)
             (aitools.kernel.domain:flow-call
                 (%report-run-outcome request pattern outcome stdout-reported)
               (:on-ok (fields)
                 (aitools.kernel.domain:flow-continue :on-ok fields))
               (:on-partial (fields)
                 (aitools.kernel.domain:flow-continue :on-partial fields)))))
    (%reporting-port-errors "run"
      (multiple-value-bind (status value program)
          (funcall (process-ports-run-program ports)
                   (run-request-argv request) timeout-ms
                   :stdout-path stdout-real
                   :stdin (run-request-stdin request)
                   :stdin-data (run-request-stdin-data request)
                   :stdin-file (run-request-stdin-file request)
                   :cwd (run-request-cwd request)
                   :env (run-request-env request))
        (ecase status
          ((:exited :timed-out)
           (report value))
          (:unavailable
           (aitools.kernel.domain:flow-call
               (%unavailable-program-error value program)
             (:on-error (code text &key repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code text :repairs repairs))))
          (:exists
           (aitools.kernel.domain:flow-continue
            :on-error "refusal.exists"
            (format nil "--stdout-to ~A already exists; run only creates new files"
                    stdout-reported)
            :repairs
            (list
             (repair "inspect-existing" "Inspect the existing file."
                     (aitools.protocol.domain:command-line
                      "aitools" "info" stdout-reported))))))))))

(defun %run-input-error (request)
  (cond
    ((> (count-if #'identity (list (run-request-stdin request)
                                  (run-request-stdin-data request)
                                  (run-request-stdin-file request))) 1)
     "--stdin, --stdin-data, and --stdin-file are mutually exclusive")
    ((some (lambda (entry)
             (let ((separator (position #\= entry)))
               (or (null separator) (zerop separator) (find #\Null entry))))
           (run-request-env request))
     "--env needs KEY=VALUE with a nonempty key and no NUL characters")
    ((and (run-request-cwd request)
          (or (zerop (length (run-request-cwd request)))
              (find #\Null (run-request-cwd request))))
     "--cwd needs a nonempty directory path without NUL characters")))

(aitools.kernel.domain:define-flow
    (run-command/k ports request &key on-ok on-partial on-error)
  "Run REQUEST's argv through PORTS' RUN-PROGRAM and report the run fields. Calls exactly one of ON-OK, ON-PARTIAL, or ON-ERROR."
  (flet ((execute (timeout-ms pattern stdout-real stdout-reported)
           (aitools.kernel.domain:flow-call
               (%run-program ports request pattern timeout-ms stdout-real stdout-reported)
             (:on-ok (fields)
               (aitools.kernel.domain:flow-continue :on-ok fields))
             (:on-partial (fields)
               (aitools.kernel.domain:flow-continue :on-partial fields))
             (:on-error (code message &key repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code message :repairs repairs)))))
    (let ((input-error (%run-input-error request)))
      (cond
        (input-error
         (aitools.kernel.domain:flow-continue
          :on-error "argument.invalid" input-error
          :repairs (list (%schema-repair "run"))))
        ((null (run-request-argv request))
         (aitools.kernel.domain:flow-continue
          :on-error "argument.invalid"
          "run needs the program and its arguments after --"
          :repairs
          (list (repair "run-program" "Pass the program after --."
                        "aitools run -- printf 'hello\\n'"))))
        (t
         (aitools.kernel.domain:flow-call
             (%call-with-duration-ms (run-request-timeout request)
                                      "--timeout" "run" :allow-zero nil)
           (:on-value (timeout-ms)
             (aitools.kernel.domain:flow-call
                 (%call-with-line-pattern (run-request-grep request) "run")
               (:on-pattern (pattern)
                 (let ((target (run-request-stdout-to request)))
                   (if target
                       (aitools.kernel.domain:flow-call
                           (%call-with-stdout-target
                            ports (run-request-root request) target)
                         (:on-resolved (stdout-real stdout-reported)
                           (execute timeout-ms pattern stdout-real stdout-reported))
                         (:on-error (code message &key repairs)
                           (aitools.kernel.domain:flow-continue
                            :on-error code message :repairs repairs)))
                       (execute timeout-ms pattern nil nil))))
               (:on-error (code message &key repairs)
                 (aitools.kernel.domain:flow-continue
                  :on-error code message :repairs repairs))))
           (:on-error (code message &key repairs)
             (aitools.kernel.domain:flow-continue
              :on-error code message :repairs repairs))))))))
