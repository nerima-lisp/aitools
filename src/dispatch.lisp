;;;;
;;;; Calls cl-cli:PARSE-ARGV directly rather than RUN-APP: RUN-APP's own
;;;; usage-error and --help/--version handling all print plain text, which
;;;; would break the rule that all output is JSON. Every exit from DISPATCH -- success, partial, error,
;;;; usage error, or an uncaught Lisp condition -- writes exactly one JSON
;;;; envelope and returns the exit code as an integer; nothing else in
;;;; aitools writes to STDOUT/STDERR or calls SB-EXT:EXIT.
;;;;
;;;; Recovery of interrupted operations runs before every dispatched command, through
;;;; CALL-WITH-INVOCATION-WORKSPACE (src/workspace-context.lisp); its result
;;;; appears in the response envelope's top-level `recovered` member.
(in-package #:aitools/cli)

(defun %write-error (command-name code message stderr
                     &key repairs candidates diagnostics conflicts recovered (trace-p t))
  (let ((envelope (aitools.protocol.domain:make-error-envelope
                   (or (%display-name command-name) "aitools") code message
                   :repairs (or repairs (list (%generic-repair command-name)))
                   :candidates candidates :diagnostics diagnostics :conflicts conflicts
                   :recovered recovered
                   :meta (when trace-p (%trace-envelope-meta :error)))))
    (aitools.protocol.infrastructure:write-envelope envelope stderr))
  (aitools.protocol.domain:error-code-exit-code code))

(defun %write-command-result (command-name result stdout stderr &key recovered)
  (ecase (aitools.protocol.application:command-result-kind result)
    (:ok
     (aitools.protocol.infrastructure:write-envelope
      (aitools.protocol.domain:make-ok-envelope
       (%display-name command-name) (aitools.protocol.application:command-result-fields result)
       :recovered recovered :meta (%trace-envelope-meta :ok))
      stdout)
     0)
    (:partial
     (aitools.protocol.infrastructure:write-envelope
      (aitools.protocol.domain:make-ok-envelope
       (%display-name command-name) (aitools.protocol.application:command-result-fields result)
       :status "partial" :recovered recovered :meta (%trace-envelope-meta :partial))
      stdout)
     3)
    (:error
     (let ((fields (aitools.protocol.application:command-result-fields result)))
       (%write-error command-name (getf fields :code) (getf fields :message) stderr
                     :repairs (getf fields :repairs) :candidates (getf fields :candidates)
                     :diagnostics (getf fields :diagnostics) :conflicts (getf fields :conflicts)
                     :recovered recovered)))))

(defun %write-schema-envelope (registry name stdout)
  (let ((schema (and name (find-command-schema registry name))))
    (aitools.protocol.infrastructure:write-envelope
     (if schema
         (aitools.protocol.application:render-command-detail schema)
         (aitools.protocol.domain:json-object-from-alist
          (list (cons "schema_version" 2) (cons "status" "ok") (cons "command" "schema")
                (cons "commands" (mapcar #'aitools.protocol.application:render-command-summary
                                         (all-command-schemas registry))))))
     stdout))
  0)

(defun %write-version-envelope (app stdout)
  (aitools.protocol.infrastructure:write-envelope
   (aitools.protocol.domain:make-ok-envelope
    "version" (list (cons "name" "aitools") (cons "version" (or (app-version app) "0.0.0"))))
   stdout)
  0)

(defun %store-error-path (condition)
  (when (typep condition 'aitools.store.application:store-io-error)
    (aitools.store.application:store-io-error-path condition)))

(defun %write-recovery-error (command-name condition root stderr)
  "Startup recovery raised a store error before the command could run.
Report ENVIRONMENT.IO named for the command, with a repair pointing at the
path that blocked recovery, rather than letting the error escape as
INTERNAL.UNEXPECTED and lock every command out. A STORE-COMMITTED-ERROR is
past its commit point, so its message says recovery will complete it."
  (let* ((committed (typep condition 'aitools.store.application:store-committed-error))
         (op-id (and committed (aitools.store.application:store-committed-error-op-id condition)))
         (path (or (%store-error-path condition) root))
         (message (if committed
                      (format nil (concatenate 'string
                              "operation ~A is past its commit point at ~A; "
                              "recovery will complete it on the next run")
                              op-id path)
                      (format nil "workspace recovery could not complete: ~A" condition))))
    (%write-error command-name "environment.io" message stderr
                  :repairs (list (repair "inspect-path"
                                          "Inspect the path that blocked recovery."
                                          (format nil "aitools info ~A" path))))))

(defun %retry-command (invocation)
  "The invocation to retry, as a runnable `aitools ...` line. Drop argv0
(the absolute program path) so the repair names the typed command rather than
`aitools /abs/path/to/aitools ...`."
  (format nil "aitools~{ ~A~}" (rest (cl-cli:invocation-raw-argv invocation))))

(defun %run-dispatched-command (invocation stdout stderr)
  (let* ((command (invocation-command invocation))
         (handler (if command (command-handler command) (app-handler (invocation-app invocation))))
         (name (%full-command-name invocation)))
    ;; NAME is resolved before the protected form so an uncaught condition
    ;; from the handler names the failed command: the envelope
    ;; and its `aitools schema <command>` repair identify the failed command
    ;; instead of the generic "aitools"/"aitools schema" the top-level handler emits
    ;; for a failure with no command in scope.
    (handler-case
        (if (null handler)
            (%write-error name "argument.invalid" (format nil "~A requires a subcommand" name) stderr)
            (aitools.kernel.domain:flow-call
                (call-with-invocation-workspace invocation)
              (:on-ready (recovered)
                (%write-command-result name (funcall handler invocation) stdout stderr
                                       :recovered recovered))
              (:on-busy ()
                (%write-error name "environment.busy"
                              "the workspace lock could not be acquired within --lock-timeout"
                              stderr
                              :repairs (list (repair "retry"
                                                     "Retry once the other writer finishes."
                                                     (%retry-command invocation)))))
              (:on-invalid-timeout (text)
                (%write-error name "argument.invalid"
                              (format nil "--lock-timeout ~S is not a duration (<n>ms|s|m|h|d)" text)
                              stderr))
              (:on-recovery-error (condition root)
                (%write-recovery-error name condition root stderr))))
      ;; A resource exhaustion the handler could not carry
      ;; is not an ERROR, so it needs its own clause: the command still owes one
      ;; envelope, and it names the failed command and reports
      ;; ENVIRONMENT.UNAVAILABLE rather than escaping.
      ;; WRITE-ENVELOPE signals this before writing anything, so standard
      ;; output stays empty and this error envelope is the only output.
      (aitools.protocol.infrastructure:envelope-too-large (condition)
        (%write-error name "refusal.too-large" (princ-to-string condition) stderr
                      :repairs (list (repair "narrow-output"
                                              "Ask for less: a smaller --max-lines or --limit, or a narrower selector."
                                              (getf (%generic-repair name) :command)))
                      :trace-p nil))
      (storage-condition (condition)
        (%write-error name "environment.unavailable"
                      (format nil "the command ran out of memory or stack: ~A" condition)
                      stderr))
      (error (condition)
        (%write-error name "internal.unexpected" (princ-to-string condition) stderr)))))

(defun dispatch (app registry argv &key (stdout *standard-output*) (stderr *error-output*))
  "Parse ARGV against APP, run the resolved command, write exactly one JSON
  envelope, and return the exit code. REGISTRY is the COMMAND-REGISTRY
  BUILD-APP returned alongside APP."
  (handler-case
      (let ((invocation (parse-argv app argv)))
        (ecase (invocation-action invocation)
          (:version (%write-version-envelope app stdout))
          (:help (%write-schema-envelope registry (%full-command-name invocation) stdout))
          (:dispatch
           (let ((name (%full-command-name invocation)))
             (%call-with-command-trace
              name (option-value invocation :trace)
              (lambda () (%run-dispatched-command invocation stdout stderr)))))))
    (cli-unknown-command (condition)
      (let ((name (cli-unknown-command-name condition)))
        (%write-error name "argument.invalid"
                     (if name (format nil "unknown command ~A" name) "no command given")
                     stderr
                     :repairs (%unknown-command-repairs registry argv name))))
    (cli-usage-error (condition)
      (let ((name (%usage-error-command-name registry (cli-usage-error-command condition))))
        (%write-error name "argument.invalid" (cli-error-message condition) stderr)))
    (error (condition)
      (%write-error nil "internal.unexpected" (princ-to-string condition) stderr))))
