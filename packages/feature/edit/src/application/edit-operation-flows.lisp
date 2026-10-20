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
                             :repairs (list (repair "inspect-schema" "Pass an existing directory as --root."
                                                    "aitools schema"))))
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
