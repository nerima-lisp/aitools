;;;; src/dispatch.lisp
;;;;
;;;; Calls cl-cli:PARSE-ARGV directly rather than RUN-APP: RUN-APP's own
;;;; usage-error and --help/--version handling all print plain text, which
;;;; would break the rule that all output is JSON. Every exit from DISPATCH -- success, partial, error,
;;;; usage error, or an uncaught Lisp condition -- writes exactly one JSON
;;;; envelope and returns the exit code as an integer; nothing else in
;;;; aitools writes to STDOUT/STDERR or calls SB-EXT:EXIT.
;;;;
;;;; Recovery of interrupted operations runs before every dispatched command, through
;;;; CALL-WITH-INVOCATION-WORKSPACE/K (src/workspace-context.lisp); what it
;;;; recovered appears as the success envelope's `recovered`.
(in-package #:aitools/cli)

(defun %full-command-name (invocation)
  "The invoked command's full dotted dispatch name (\"read\", \"json.get\"),
or NIL when no command was dispatched (bare `--help`/`--version`)."
  (let ((path (invocation-command-path invocation)))
    (when path
      (format nil "~{~A~^.~}" (mapcar #'command-name path)))))

(defun %display-name (command-name)
  "COMMAND-NAME as the agent types it (\"json get\") for envelopes and
repairs; registry lookups keep the dotted dispatch name (\"json.get\")."
  (and command-name (substitute #\Space #\. command-name)))

(defun %generic-repair (command-name)
  (schema-repair
   (if command-name
       (format nil "aitools schema ~A" (%display-name command-name))
       "aitools schema")))

(defun %recovered-json (recovered)
  "RECOVERED, a list of (:op-id ID :action ACTION) plists, as the JSON objects
MAKE-OK-ENVELOPE's `recovered[]` uses. Built here because MAKE-ERROR-ENVELOPE
takes no :RECOVERED key: the composition root appends the field itself through
the exported JSON seams so a recovery followed by a failing command still
reports what it recovered."
  (mapcar (lambda (entry)
            (aitools.protocol.domain:json-object-from-alist
             (list (cons "op_id" (getf entry :op-id))
                   (cons "action" (getf entry :action)))))
          recovered))

(defun %write-error (command-name code message stderr
                     &key repairs candidates diagnostics conflicts recovered)
  (let ((envelope (aitools.protocol.domain:make-error-envelope
                   (or (%display-name command-name) "aitools") code message
                   :repairs (or repairs (list (%generic-repair command-name)))
                   :candidates candidates :diagnostics diagnostics :conflicts conflicts)))
    (aitools.protocol.infrastructure:write-envelope
     (if recovered
         (aitools.protocol.domain:json-object-from-alist
          (append (aitools.protocol.domain:json-object-members envelope)
                  (list (cons "recovered" (%recovered-json recovered)))))
         envelope)
     stderr))
  (aitools.protocol.domain:error-code-exit-code code))

(defun %write-command-result (command-name result stdout stderr &key recovered)
  (ecase (aitools.protocol.application:command-result-kind result)
    (:ok
     (aitools.protocol.infrastructure:write-envelope
      (aitools.protocol.domain:make-ok-envelope
       (%display-name command-name) (aitools.protocol.application:command-result-fields result)
       :recovered recovered)
      stdout)
     0)
    (:partial
     (aitools.protocol.infrastructure:write-envelope
      (aitools.protocol.domain:make-ok-envelope
       (%display-name command-name) (aitools.protocol.application:command-result-fields result)
       :status "partial" :recovered recovered)
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
          (list (cons "schema_version" 1) (cons "status" "ok") (cons "command" "schema")
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

;;; ------------------------------------------------------ name resolution

(defun %groups-with-subcommand (registry name)
  "The group names whose subcommand list contains a command named NAME, sorted
so the repairs read in a stable order (`bg`, `git`, `tx` for `status`)."
  (sort (loop for group being the hash-keys of (command-registry-group-commands registry)
                using (hash-value subcommands)
              when (find name subcommands :key #'command-name :test #'string=)
                collect group)
        #'string<))

(defun %group-repair-command (argv name group)
  "ARGV rebuilt as a runnable `aitools ...` line with GROUP inserted before the
command token NAME, so `--root R get /a f.json` becomes
`aitools --root R json get /a f.json`. Drops argv0 and shell-quotes every
word so an operand with spaces stays one argument."
  (let* ((tokens (rest argv))
         (pos (position name tokens :test #'string=))
         (rebuilt (if pos
                      (append (subseq tokens 0 pos) (list group) (subseq tokens pos))
                      (list* group tokens))))
    (format nil "aitools ~A" (aitools.protocol.domain:command-line rebuilt))))

(defun %group-subcommand-repairs (registry argv name)
  "The repair for a group subcommand typed without its group
(`get` for `json get`, `uuid` for `util uuid`): one `run-instead` repair per
group whose subcommand list contains a command named NAME, each the grouped
form of the invocation ARGV named, keeping its globals and operands. NIL when
NAME is no group's subcommand -- the caller then falls back to the
correspondence table."
  (loop for group in (%groups-with-subcommand registry name)
        collect (repair "run-instead"
                         (format nil "`~A` is the `~A` group's subcommand; include the group name."
                                 name group)
                         (%group-repair-command argv name group))))

(defun %unknown-name-repairs (name)
  "The correspondence-table repairs for the unknown dispatch NAME, verbatim from
the table. The repair is the canonical aitools command to use instead
(`cat` -> `aitools read`) and does not carry NAME's operands: the
correspondence data is a static hint, and the meta suite pins each repair to
the table's exact command."
  (repairs-for-unknown-name name))

(defun %command-group (registry command)
  "The group name whose subcommand list contains COMMAND by identity, or NIL."
  (loop for group being the hash-keys of (command-registry-group-commands registry)
          using (hash-value subcommands)
        when (member command subcommands :test #'eq)
          return group))

(defun %usage-error-command-name (registry command)
  "The dotted dispatch name of the command in scope for a usage error. A
group subcommand keeps its group (`bg.logs`, not `logs`) so the envelope and
its `aitools schema bg logs` repair name the command the agent actually
invoked. NIL when no command was in scope."
  (when command
    (let ((group (%command-group registry command)))
      (if group
          (format nil "~A.~A" group (command-name command))
          (command-name command)))))

;;; ---------------------------------------------- startup recovery failure

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
                      (format nil "operation ~A is past its commit point at ~A; recovery will complete it on the next run"
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
    ;; and its `aitools schema <command>` repair identify the command instead
    ;; of the generic "aitools"/"aitools schema" the top-level handler emits
    ;; for a failure with no command in scope.
    (handler-case
        (if (null handler)
            (%write-error name "argument.invalid" (format nil "~A requires a subcommand" name) stderr)
            (call-with-invocation-workspace/k
             invocation
             :on-ready (lambda (recovered)
                         (%write-command-result name (funcall handler invocation) stdout stderr
                                                :recovered recovered))
             :on-busy (lambda ()
                        (%write-error name "environment.busy"
                                      "the workspace lock could not be acquired within --lock-timeout"
                                      stderr
                                      :repairs (list (repair "retry"
                                                              "Retry once the other writer finishes."
                                                              (%retry-command invocation)))))
             :on-invalid-timeout (lambda (text)
                                   (%write-error name "argument.invalid"
                                                 (format nil "--lock-timeout ~S is not a duration (<n>ms|s|m|h|d)" text)
                                                 stderr))
             :on-recovery-error (lambda (condition root)
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
                                              (getf (%generic-repair name) :command)))))
      (storage-condition (condition)
        (%write-error name "environment.unavailable"
                      (format nil "the command ran out of memory or stack: ~A" condition)
                      stderr))
      (error (condition)
        (%write-error name "internal.unexpected" (princ-to-string condition) stderr)))))

(defun %unknown-command-repairs (registry argv name)
  "The repairs for an unresolved dispatch NAME (docs/src/reference/errors.md,
Unknown command names). A foreign name in the correspondence table gets the table's canonical command
verbatim (`fmt` -> `aitools transform`, `uuid` -> `aitools util uuid`), without
operands, even when a group also has a subcommand of that name; the meta suite
pins each to the table. A bare group subcommand NOT in the table (`get`)
points at its grouped form and carries the globals and operands so the repair
runs as given."
  (cond ((null name) (list (%generic-repair nil)))
        ((aitools.protocol.domain:correspondence-name-p name) (%unknown-name-repairs name))
        ((%group-subcommand-repairs registry argv name))
        (t (%unknown-name-repairs name))))

(defun dispatch (app registry argv &key (stdout *standard-output*) (stderr *error-output*))
  "Parse ARGV against APP, run the resolved command, write exactly one JSON
envelope, and return the exit code. REGISTRY is the COMMAND-REGISTRY
BUILD-APP returned alongside APP."
  (handler-case
      (let ((invocation (parse-argv app argv)))
        (ecase (invocation-action invocation)
          (:version (%write-version-envelope app stdout))
          (:help (%write-schema-envelope registry (%full-command-name invocation) stdout))
          (:dispatch (%run-dispatched-command invocation stdout stderr))))
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
