;;;; docs/tools/generate-reference.lisp
;;;;
;;;; Regenerates the data-derived parts of the documentation:
;;;;
;;;;     sbcl --script docs/tools/generate-reference.lisp
;;;;
;;;; with CL_SOURCE_REGISTRY naming every sibling dependency, as for
;;;; run-tests.lisp. It rewrites only the text between each
;;;; `<!-- BEGIN GENERATED: name -->` / `<!-- END GENERATED: name -->` pair:
;;;;
;;;;   commands        docs/src/reference/commands.md, from the COMMAND-SCHEMA
;;;;                   values each context registers (what `aitools schema
;;;;                   --all` prints), grouped by the context that registers
;;;;                   them.
;;;;   correspondence  docs/src/guide/agents.md, from
;;;;                   AITOOLS.DATA:*CORRESPONDENCE-TABLE*, the table the
;;;;                   unknown-command repairs are drawn from.
;;;;   errors          docs/src/reference/errors.md, from
;;;;                   AITOOLS.DATA:*PROTOCOL-ERROR-CODES* and each command
;;;;                   schema's declared error codes.

(require :asdf)

(defparameter *root*
  (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*)))

(asdf:initialize-source-registry
 `(:source-registry (:directory ,*root*) :inherit-configuration))

(let ((*error-output* (make-broadcast-stream)))
  (asdf:load-system "aitools/cli"))

(defun cell (text)
  "TEXT made safe for one Markdown table cell."
  (with-output-to-string (out)
    (loop for char across (princ-to-string text)
          do (case char
               (#\| (write-string "\\|" out))
               (#\Newline (write-char #\Space out))
               (t (write-char char out))))))

(defun display-name (schema)
  (substitute #\Space #\. (aitools.protocol.domain:command-schema-name schema)))

(defun anchor (name)
  (string-downcase (substitute #\- #\Space name)))

(defun argument-label (arg)
  (let ((name (getf arg :name)))
    (if (equal (getf arg :kind) "positional")
        (format nil "`~A` (positional~:[~;, required~])" name (getf arg :required))
        (format nil "`~A`~:[~;, required~]" name (getf arg :required)))))

(defun argument-type (arg)
  (format nil "~A~@[: ~{`~A`~^, ~}~]~:[~;, repeatable~]~@[, ~D values~]"
          (getf arg :type) (getf arg :choices)
          (or (getf arg :repeatable) (getf arg :multiple))
          (getf arg :value-count)))

(defun write-command (schema out)
  (let ((name (display-name schema))
        (summary (aitools.protocol.domain:command-schema-summary schema))
        (description (aitools.protocol.domain:command-schema-description schema))
        (args (aitools.protocol.domain:command-schema-args schema))
        (fields (aitools.protocol.domain:command-schema-output-fields schema))
        (codes (aitools.protocol.domain:command-schema-error-codes schema)))
    (format out "### `~A` {#~A}~%~%~A~%~%" name (anchor name) summary)
    (when (and description (string/= description summary))
      (format out "~A~%~%" description))
    (when args
      (format out "| Argument | Type | Default | Description |~%|---|---|---|---|~%")
      (dolist (arg args)
        (format out "| ~A | ~A | ~@[`~A`~] | ~A |~%"
                (argument-label arg) (cell (argument-type arg))
                (let ((default (getf arg :default))) (and default (cell default)))
                (cell (or (getf arg :description) ""))))
      (terpri out))
    (when fields
      (format out "| Output field | Description |~%|---|---|~%")
      (dolist (field fields)
        (format out "| `~A` | ~A |~%" (getf field :name) (cell (or (getf field :description) ""))))
      (terpri out))
    (when codes
      (format out "Errors: ~{`~A`~^, ~}.~%~%" codes))))

(defun doc-context-registrations ()
  "((CONTEXT REGISTER-FUNCTION MAKE-PORTS) ...) in feature context order,
mirroring `register-all-context-commands' in src/context-registration.lisp.
Each context is registered into its own registry so its schemas can be grouped
under the matching `## The <context> context' heading."
  (list (list "search" #'aitools.search.presentation:register-search-commands
              #'aitools.search.infrastructure:make-production-search-ports)
        (list "inspect" #'aitools.inspect.presentation:register-inspect-commands
              #'aitools.inspect.infrastructure:make-production-inspect-ports)
        (list "edit" #'aitools.edit.presentation:register-edit-commands
              #'aitools.edit.infrastructure:make-production-edit-ports)
        (list "journal" #'aitools.journal.presentation:register-journal-commands
              #'aitools.journal.infrastructure:make-production-journal-ports)
        (list "process" #'aitools.process.presentation:register-process-commands
              #'aitools.process.infrastructure:make-production-process-ports)
        (list "vcs" #'aitools.vcs.presentation:register-vcs-commands
              #'aitools.vcs.infrastructure:make-production-vcs-ports)
        (list "env" #'aitools.env.presentation:register-env-commands
              #'aitools.env.infrastructure:make-production-env-ports)
        (list "util" #'aitools.util.presentation:register-util-commands
              #'aitools.util.infrastructure:make-production-util-ports)))

(defun doc-port-arguments ()
  "The shared keyword arguments every `make-production-<context>-ports'
accepts, as `%port-arguments' in src/context-registration.lisp builds them.
Constructing production ports does no I/O, so it is safe from a doc tool."
  (list :state-directory-function #'aitools/cli::current-state-directory
        :workspace-host (aitools.workspace.infrastructure:make-host-workspace-host)
        :text-source (aitools.text.infrastructure:make-host-text-source)
        :open-store #'aitools.store.infrastructure:make-posix-store))

(defun context-schemas ()
  "((CONTEXT . SCHEMAS) ...) in registration order, one entry per feature
context, then (\"composition root\" . SCHEMAS) for commands src/ registers."
  (let ((arguments (doc-port-arguments))
        (seen (make-hash-table :test 'equal))
        (groups '()))
    (dolist (entry (doc-context-registrations))
      (destructuring-bind (context register-function make-ports) entry
        (let ((registry (aitools.protocol.application:make-command-registry)))
          (funcall register-function registry (apply make-ports arguments))
          (let ((schemas (aitools.protocol.application:all-command-schemas registry)))
            (dolist (schema schemas)
              (setf (gethash (aitools.protocol.domain:command-schema-name schema) seen) t))
            (push (cons context schemas) groups)))))
    (let ((rest (remove-if (lambda (schema) (gethash (aitools.protocol.domain:command-schema-name schema) seen))
                           (aitools.protocol.application:all-command-schemas
                            (nth-value 1 (aitools/cli::build-app))))))
      (when rest (push (cons "composition root" rest) groups)))
    (nreverse groups)))

(defun commands-section ()
  (with-output-to-string (out)
    (let ((groups (context-schemas)))
      (format out "| Command | Context | Summary |~%|---|---|---|~%")
      (loop for (context . schemas) in groups
            do (dolist (schema schemas)
                 (format out "| [`~A`](#~A) | ~A | ~A |~%" (display-name schema) (anchor (display-name schema))
                         context (cell (aitools.protocol.domain:command-schema-summary schema)))))
      (loop for (context . schemas) in groups
            do (if (string= context "composition root")
                   (format out "~%## Composition root~%~%")
                   (format out "~%## The ~A context~%~%" context))
               (dolist (schema schemas) (write-command schema out))))))

(defun correspondence-section ()
  (with-output-to-string (out)
    (format out "| Shell command | aitools command | What it does |~%|---|---|---|~%")
    (dolist (row aitools.data:*correspondence-table*)
      (let ((names (format nil "~{`~A`~^, ~}" (getf row :foreign-names))))
        (dolist (repair (getf row :repairs))
          (format out "| ~A | `~A` | ~A |~%" names (getf repair :command) (cell (getf repair :detail))))))))

(defun errors-section ()
  (with-output-to-string (out)
    (let ((schemas (loop for (nil . schemas) in (context-schemas) append schemas)))
      (format out "| error.code | Exit code | Declared by |~%|---|---|---|~%")
      (dolist (entry aitools.data:*protocol-error-codes*)
        (let* ((code (getf entry :code))
               (users (loop for schema in schemas
                            when (member code (aitools.protocol.domain:command-schema-error-codes schema)
                                         :test #'string=)
                              collect (display-name schema))))
          (format out "| `~A` | ~D | ~:[none~;~:*~{[`~A`](commands.md#~A)~^, ~}~] |~%"
                  code (getf entry :exit-code)
                  (loop for name in users append (list name (anchor name)))))))))

(defun replace-region (relative-path name text)
  (let* ((path (merge-pathnames relative-path *root*))
         (content (uiop:read-file-string path))
         (begin-marker (format nil "<!-- BEGIN GENERATED: ~A -->" name))
         (end-marker (format nil "<!-- END GENERATED: ~A -->" name))
         (begin (search begin-marker content))
         (end (and begin (search end-marker content :start2 begin))))
    (unless (and begin end)
      (error "~A has no ~A ... ~A region" relative-path begin-marker end-marker))
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string (subseq content 0 (+ begin (length begin-marker))) out)
      (format out "~%~A" text)
      (write-string (subseq content end) out))))

(handler-case
    (progn
      (replace-region "docs/src/reference/commands.md" "commands" (commands-section))
      (replace-region "docs/src/guide/agents.md" "correspondence" (correspondence-section))
      (replace-region "docs/src/reference/errors.md" "errors" (errors-section))
      (uiop:quit 0))
  (error (condition)
    (format *error-output* "~&generate-reference failed: ~A~%" condition)
    (uiop:quit 1)))
