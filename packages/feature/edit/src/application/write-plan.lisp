;;;; packages/feature/edit/src/application/write-plan.lisp
;;;;
;;;; The write-command model RUN-WRITE-COMMAND/K (pipeline.lisp) consumes:
;;;; WRITE-TARGET, WRITE-PLAN, the WRITE-CONTEXT a PLAN function sees, the
;;;; plan-side guard helpers (--expect-count, the state-dependent
;;;; --expect-hash), and the repairs every write error carries.
(in-package #:aitools.edit.application)

(defstruct (write-target (:constructor make-write-target (path &key (follow t) (base :cwd))) (:copier nil))
  "PATH as the user wrote it: absolute, or with BASE :CWD (the default)
relative to the working directory (AITOOLS.WORKSPACE.APPLICATION:
USER-PATH-ABSOLUTE); BASE :ROOT marks a workspace-relative path a flow
derived itself (a scan result, a diff's file name). FOLLOW NIL checks the
parent directory and keeps the last component itself, for commands acting
on a symlink rather than what it points at (delete, move, link)."
  (path "" :type string :read-only t)
  (follow t :read-only t)
  (base :cwd :type (member :cwd :root) :read-only t))

(defun %target-absolute (host workspace-root path base)
  (ecase base
    (:cwd (aitools.workspace.application:user-path-absolute host path))
    (:root (aitools.workspace.domain:normalize-path
            (aitools.workspace.domain:join-path (aitools.workspace.application:workspace-root-path workspace-root)
                                                path)))))

(defstruct (write-plan (:constructor make-write-plan
                           (&key command targets inputs guard-requirements expect-hashes expect-count
                              replayable plan record-options record-positionals))
                       (:copier nil))
  "What one write command wants, independent of how it runs.

COMMAND: dispatch name (\"edit\", \"json.set\").
TARGETS: WRITE-TARGETs, boundary-checked in order; the plan sees their
  workspace-relative paths as CONTEXT-PATHS.
INPUTS: strings or octet vectors checked for the redaction placeholder.
GUARD-REQUIREMENTS: (:EXPECT-HASH user-path) and (:EXPECT-COUNT) entries
  that the guard rules make mandatory for this invocation.
EXPECT-HASHES: the raw --expect-hash arguments; EXPECT-COUNT the raw
  --expect-count text or NIL.
REPLAYABLE: true when `tx rebase` may re-run the op from its argv.
PLAN: (lambda (context commit reject)); COMMIT takes change requests and
  an optional alist of extra output fields, REJECT takes (code message
  &key candidates diagnostics conflicts repairs).
RECORD-OPTIONS / RECORD-POSITIONALS: the option plist (or a function of the
  resolved target paths returning it) and a function of those paths
  returning the positionals, from which the journal/tx argv is built
  (OPTIONS-ARGV)."
  (command "" :type string :read-only t)
  (targets '() :type list :read-only t)
  (inputs '() :type list :read-only t)
  (guard-requirements '() :type list :read-only t)
  (expect-hashes '() :type list :read-only t)
  (expect-count nil :read-only t)
  (replayable nil :read-only t)
  (plan nil :type function :read-only t)
  (record-options '() :read-only t)
  (record-positionals (constantly '()) :type function :read-only t))

(defstruct (write-context (:copier nil))
  "What a PLAN function sees. VIEW reads the state the write applies to;
PATHS are the resolved targets. ROOT and HOST are NIL during `tx rebase`,
where recorded paths are already workspace-relative."
  (view nil :read-only t)
  (root nil :read-only t)
  (host nil :read-only t)
  (ports nil :read-only t)
  (paths '() :type list :read-only t)
  (command "" :type string :read-only t)
  (command-line "" :type string :read-only t)
  (expect-count nil :read-only t)
  ;; resolved paths some --expect-hash names
  (hash-paths '() :type list :read-only t)
  (dry-run nil :read-only t)
  (tx nil :read-only t)
  (temporary-root nil :read-only t)
  ;; The count CHECK-EXPECT-COUNT/K last compared, reported by a --dry-run
  ;; as the value to pass as --expect-count.
  (selected-count nil))

(defun context-path (context &optional (index 0))
  (nth index (write-context-paths context)))

;;; ------------------------------------------------------------ repairs

(defun command-line (argv)
  "`aitools` followed by ARGV, shell-quoted (AITOOLS.PROTOCOL.DOMAIN)."
  (aitools.protocol.domain:command-line (cons "aitools" argv)))

(defun dry-run-command-line (argv)
  "The runnable `aitools ...` line re-running ARGV (the words after the
program name) as one --dry-run without --expect-count: a dry run counts the
selection itself, and a stale --expect-count would fail it again. Words after
`--` are operands and stay as typed."
  (let ((separator (position "--" argv :test #'string=)))
    (labels ((strip (words)
               (cond
                 ((null words) '())
                 ((string= (first words) "--dry-run") (strip (rest words)))
                 ((string= (first words) "--expect-count") (strip (cddr words)))
                 ((eql 0 (search "--expect-count=" (first words))) (strip (rest words)))
                 (t (cons (first words) (strip (rest words)))))))
      (command-line (append (strip (subseq argv 0 separator))
                            (list "--dry-run")
                            (and separator (subseq argv separator)))))))

(defun default-repairs (code command argv &optional path)
  "The repairs of a write error CODE without its own. ARGV is the command as
typed after the program name (or its canonical argv)."
  (let ((path (and path (aitools.protocol.domain:shell-quote path)))
        (schema (repair "inspect-schema" "Show this command's arguments and rules."
                        (format nil "aitools schema ~A" (command-display-name command)))))
    (flet ((with-path (action detail control)
             (if path (list (repair action detail (format nil control path))) (list schema))))
      (cond
        ((string= code "environment.busy")
         (list (repair "retry" "Run the same command again once the other writer finishes." (command-line argv))))
        ((string= code "selection.count-mismatch")
         (list (repair "count" "Count the selection without writing, then pass that --expect-count."
                       (dry-run-command-line argv))))
        ((member code '("refusal.target-changed" "refusal.exists" "refusal.not-a-file" "environment.io")
                 :test #'string=)
         (with-path "info" "Look at the path's current state and hash." "aitools info ~A"))
        ((string= code "refusal.redacted-input")
         (with-path "read" "Read the real text: [REDACTED_SECRET] marks masked output, not file content." "aitools read ~A"))
        ((member code '("selection.no-match" "selection.ambiguous" "input.unsupported-language")
                 :test #'string=)
         (with-path "read" "Read the file to choose a unique, current selection." "aitools read ~A"))
        ((string= code "input.not-utf8")
         (with-path "transcode" "Convert the file to UTF-8 before editing." "aitools transcode ~A --to utf-8"))
        ((string= code "input.not-found")
         (if path
             (list (repair "find" "Look for the path." (format nil "aitools find ~A" path)))
             (list schema)))
        ((string= code "refusal.outside-workspace")
         (list (repair "mktemp" "Temporary files belong in the mktemp area, which writes may use."
                       "aitools mktemp")))
        (t (list schema))))))

;;; --------------------------------------------------------- plan helpers

(defun check-expect-count/k (context actual reject on-ok)
  "Compare --expect-count with ACTUAL: call ON-OK (), or REJECT with
selection.count-mismatch and the actual count in `diagnostics`."
  (declare (type function reject on-ok))
  (let ((expected (write-context-expect-count context)))
    (setf (write-context-selected-count context) actual)
    (if (or (null expected) (= expected actual))
        (funcall on-ok)
        (funcall reject "selection.count-mismatch"
                 (format nil "expected ~D, found ~D" expected actual)
                 :diagnostics (list (aitools.protocol.domain:json-object-from-alist
                                     (list (cons "expected" expected) (cons "actual" actual))))))))

(defun require-expect-hash/k (context path reject on-ok)
  "The state-dependent guard requirement (replacing an existing destination):
ON-OK when an --expect-hash was given, else REJECT argument.invalid."
  (declare (type function reject on-ok))
  (if (member path (write-context-hash-paths context) :test #'equal)
      (funcall on-ok)
      (funcall reject "argument.invalid"
               (format nil "~A exists; replacing it needs --expect-hash ~A=<hash>" path path)
               :repairs (list (repair "get-hash" "Read the current hash, then pass it as --expect-hash."
                                      (format nil "aitools info ~A" (aitools.protocol.domain:shell-quote path)))))))
