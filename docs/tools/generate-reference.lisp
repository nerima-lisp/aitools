;;;;
;;;; Regenerates the data-derived parts of the documentation:
;;;;
;;;;     sbcl --script docs/tools/generate-reference.lisp
;;;;     sbcl --script docs/tools/generate-reference.lisp --check
;;;;
;;;; with CL_SOURCE_REGISTRY naming every sibling dependency, as for
;;;; run-tests.lisp. It rewrites marked regions and generated context pages.
;;;;
;;;;   commands        docs/src/reference/commands/index.md and one page per
;;;;                   context, from the COMMAND-SCHEMA values each context
;;;;                   registers (what `aitools schema --all` prints).
;;;;   correspondence  docs/src/guide/agents.md, from the shell names in
;;;;                   AITOOLS.DATA:*CORRESPONDENCE-TABLE*.
;;;;   retired-document-commands  docs/src/guide/agents.md, from the retired
;;;;                   names and current repairs in the same table.
;;;;   errors          docs/src/reference/errors.md, from
;;;;                   AITOOLS.DATA:*PROTOCOL-ERROR-CODES* and each command
;;;;                   schema's declared error codes.

(require :asdf)

(defparameter *root*
  (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*)))

(asdf:initialize-source-registry `(:source-registry (:directory ,*root*) :inherit-configuration))

(let ((*error-output* (make-broadcast-stream))) (asdf:load-system "aitools/cli"))

(load (merge-pathnames "generate-reference-correspondence.lisp" *load-truename*))
(defun display-name (schema)
  (substitute #\Space #\. (aitools.protocol.domain:command-schema-name schema)))

(defun anchor (name)
  (string-downcase (substitute #\- #\Space name)))

(defun replace-substring (source target replacement)
  (let ((match (search target source)))
    (if match
        (concatenate 'string
                     (subseq source 0 match)
                     replacement
                     (replace-substring (subseq source (+ match (length target)))
                                        target replacement))
        source)))

(defun argument-label (arg)
  (let ((name (getf arg :name)))
    (if (equal (getf arg :kind) "positional")
        (format nil "`~A` (positional~:[~;, required~])" name (getf arg :required))
        (format nil "`~A`~:[~;, required~]" name (getf arg :required)))))

(defun argument-type (arg)
  (format nil "~A~@[: ~{`~A`~^, ~}~]~:[~;, repeatable~]~@[, ~D values~]"
          (getf arg :type) (getf arg :choices)
          (or (getf arg :repeatable) (getf arg :multiple)) (getf arg :value-count)))

(defun write-command (schema out)
  (let* ((name (display-name schema))
         (summary (aitools.protocol.domain:command-schema-summary schema))
         (description (let ((text (aitools.protocol.domain:command-schema-description schema)))
                        (and text (replace-substring text "errors.md#" "../errors.md#"))))
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
  "((CONTEXT REGISTER-FUNCTION DEPENDENCY-FUNCTION) ...) in feature order."
  (loop for (context register-function dependency-function)
          in (aitools/cli::context-command-registrations)
        collect (list (string-downcase (string context))
                      register-function
                      dependency-function)))

(defun context-schemas ()
  "((CONTEXT . SCHEMAS) ...) in registration order, one entry per feature
context, then a composition-root group for commands src/ registers."
  (let ((arguments (aitools/cli::%context-dependency-arguments))
        (seen (make-hash-table :test (function equal)))
        (groups nil))
    (dolist (entry (doc-context-registrations))
      (destructuring-bind (context register-function dependency-function) entry
        (let ((registry (aitools.protocol.application:make-command-registry)))
          (funcall register-function registry (apply dependency-function arguments))
          (let ((schemas (aitools.protocol.application:all-command-schemas registry)))
            (dolist (schema schemas)
              (setf (gethash (aitools.protocol.domain:command-schema-name schema) seen) t))
            (push (cons context schemas) groups)))))
    (let ((rest (remove-if (lambda (schema)
                             (gethash (aitools.protocol.domain:command-schema-name schema) seen))
                           (aitools.protocol.application:all-command-schemas
                            (nth-value 1 (aitools/cli::build-app))))))
      (when rest (push (cons "composition root" rest) groups)))
    (nreverse groups)))

(defun commands-section ()
  (let ((groups (context-schemas)))
    (values
     (with-output-to-string (out)
       (format out "| Command | Context | Summary |~%|---|---|---|~%")
       (loop for (context . schemas) in groups
             do (dolist (schema schemas)
                  (let* ((name (display-name schema))
                         (target (if (string= context "composition root")
                                     (format nil "#~A" (anchor name))
                                     (format nil "~A.md#~A" context (anchor name)))))
                    (format out "| [`~A`](~A) | ~A | ~A |~%"
                            name target context
                            (cell (aitools.protocol.domain:command-schema-summary schema))))))
       (dolist (group groups)
         (when (string= (car group) "composition root")
           (format out "~%## Composition root~%~%")
           (dolist (schema (cdr group))
             (write-command schema out)))))
     (loop for (context . schemas) in groups
           unless (string= context "composition root")
             collect (cons context
                     (with-output-to-string (out)
                       (format out "# The ~A context~%~%[Command index](index.md#command-index).~%~%" context)
                       (dolist (schema schemas)
                         (write-command schema out))))))))

(defun errors-section ()
  (let ((groups (context-schemas)))
    (with-output-to-string (out)
      (format out "| error.code | Exit code | Declared by |~%|---|---|---|~%")
      (dolist (entry aitools.data:*protocol-error-codes*)
        (let* ((code (getf entry :code))
               (users (loop for (context . schemas) in groups append
                        (loop for schema in schemas
                              when (member code (aitools.protocol.domain:command-schema-error-codes schema)
                                           :test #'string=)
                                collect (let ((name (display-name schema)))
                                          (format nil "[`~A`](commands/~A.md#~A)"
                                                  name
                                                  (if (string= context "composition root") "index" context)
                                                  (anchor name)))))))
          (format out "| `~A` | ~D | ~A |~%"
                  code
                  (getf entry :exit-code)
                  (if users (format nil "~{~A~^, ~}" users) "none")))))))

(defun replace-region (relative-path name text check-only)
  (let* ((path (merge-pathnames relative-path *root*))
         (content (uiop:read-file-string path))
         (begin-marker (format nil "<!-- BEGIN GENERATED: ~A -->" name))
         (end-marker (format nil "<!-- END GENERATED: ~A -->" name))
         (begin (search begin-marker content))
         (end (and begin (search end-marker content :start2 begin))))
    (unless (and begin end)
      (error "~A has no ~A ... ~A region" relative-path begin-marker end-marker))
    (let ((expected (concatenate 'string
                                 (subseq content 0 (+ begin (length begin-marker)))
                                 (format nil "~%~A" text)
                                 (subseq content end))))
      (if check-only
          (unless (string= content expected)
            (error "~A generated ~A region is out of date" relative-path name))
          (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
            (write-string expected out))))))

(progn
  (defun write-generated-page (relative-path text check-only)
    (let ((path (merge-pathnames relative-path *root*)))
      (if check-only
          (unless (and (probe-file path)
                       (string= (uiop:read-file-string path) text))
            (error "~A is out of date; regenerate the command reference" relative-path))
          (progn
            (ensure-directories-exist path)
            (with-open-file (out path :direction :output :if-exists :supersede
                                      :external-format :utf-8)
              (write-string text out))))))
  (let ((check-only (member "--check" sb-ext:*posix-argv* :test #'string=)))
    (handler-case
        (progn
          (multiple-value-bind (index pages) (commands-section)
            (replace-region "docs/src/reference/commands/index.md" "commands" index check-only)
            (dolist (page pages)
              (let ((context (car page))
                    (text (cdr page)))
                (write-generated-page
                 (format nil "docs/src/reference/commands/~A.md" context)
                 text check-only))))
          (replace-region "docs/src/guide/agents.md" "correspondence"
                          (correspondence-section) check-only)
          (replace-region "docs/src/guide/agents.md" "retired-document-commands"
                          (retired-document-section) check-only)
          (replace-region "docs/src/reference/errors.md" "errors"
                          (errors-section) check-only)
          (uiop:quit 0))
      (error (condition)
        (format *error-output* "~&generate-reference failed: ~A~%" condition)
        (uiop:quit 1)))))
