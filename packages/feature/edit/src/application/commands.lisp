;;;; packages/feature/edit/src/application/commands.lisp
;;;;
;;;; The entry points presentation and `tx rebase` use: RUN-EDIT-COMMAND runs
;;;; a command from its parsed positionals and options through the write
;;;; pipeline; REPLAY-EDIT-OP re-runs a recorded op against a rebuilt tx
;;;; view, registered with the journal context for every command whose
;;;; ops can be replayable.
(in-package #:aitools.edit.application)

(defun run-edit-command (ports name positionals options &key root lock-timeout display-argv on-ok on-partial on-error)
  "Run edit command NAME (a dispatch name) and call ON-OK (fields) or
ON-ERROR (code message &key candidates diagnostics conflicts repairs).
POSITIONALS are strings; OPTIONS a plist keyed by the spec's option keys
with string (or list, or T) values. ROOT and LOCK-TIMEOUT are the global
options; DISPLAY-ARGV the words typed after the program name, for repair
commands."
  (declare (ignore on-partial) (type function on-ok on-error))
  (let* ((argv (or display-argv (options-argv name positionals options))))
    (flet ((fail (code message &key candidates diagnostics conflicts repairs (path (first positionals)))
             (funcall on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts
                      :repairs (or repairs (default-repairs code name argv path)))))
      (if (string= name "mktemp")
          (mktemp-flow ports options :root root :on-ok on-ok :on-error #'fail)
          (let ((host (edit-ports-workspace-host ports)))
            (aitools.workspace.application:call-with-resolved-root/k
             host :root root
             :on-error (lambda (reason path)
                         (fail (if (eq reason :not-found) "input.not-found" "argument.invalid")
                               (format nil "workspace root ~A ~:[is not a directory~;does not exist~]" path
                                       (eq reason :not-found))
                               :repairs (list (repair "inspect-schema" "Pass an existing directory as --root." "aitools schema"))))
             :on-resolved
             (lambda (workspace-root)
               (funcall (gethash name *preparers*)
                        ports (make-command-env host workspace-root ports) positionals options
                        (lambda (plan)
                          (run-write-command/k ports plan :root root :lock-timeout lock-timeout
                                                          :dry-run (getf options :dry-run) :tx (getf options :tx)
                                                          :display-argv display-argv
                                                          :on-ok on-ok :on-error on-error))
                        #'fail))))))))

(defparameter +replayable-commands+
  '("edit" "insert" "replace" "apply" "transform" "move-lines"
    "json.set" "json.delete" "json.merge" "json.patch" "json.fmt")
  "The commands whose ops `tx rebase` may re-run: the ones registered with the
journal below, and the only ones REPLAY-EDIT-OP prepares without ports or a
workspace env.")

(defun replay-edit-op (argv view commit reject)
  "The `tx rebase` replayer for every command of +REPLAYABLE-COMMANDS+: parse
the recorded ARGV and re-run the op's plan against VIEW."
  (declare (type function commit reject))
  (parse-recorded-argv
   argv
   :on-invalid (lambda (message) (funcall reject "argument.invalid" message))
   :on-parsed
   (lambda (name positionals options)
     (let ((preparer (gethash name *preparers*)))
       (if (not (member name +replayable-commands+ :test #'string=))
           (funcall reject "argument.invalid" (format nil "~A cannot be replayed" name))
           (funcall preparer nil nil positionals options
                    (lambda (plan)
                      (if (not (write-plan-replayable plan))
                          (funcall reject "refusal.target-changed" (format nil "~A is position-based and is not replayed" name))
                          (run-plan (make-write-context :view view
                                                        :paths (mapcar #'write-target-path (write-plan-targets plan))
                                                        :command name
                                                        :expect-count (parse-count (getf options :expect-count)))
                                    (write-plan-plan plan)
                                    (lambda (requests extra)
                                      (declare (ignore extra))
                                      (funcall commit requests))
                                    reject)))
                    (lambda (code message &rest keys)
                      (declare (ignore keys))
                      (funcall reject code message))))))))

(dolist (name +replayable-commands+)
  (aitools.journal.application:register-tx-replayer name #'replay-edit-op))
