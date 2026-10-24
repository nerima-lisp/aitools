;;;; src/schema.lisp
;;;;
;;;; `schema [command...] [--all]`. Composition-root, not a feature
;;;; context's command, because it reads every context's registered
;;;; COMMAND-SCHEMA (`batch` and `schema` live in the composition root; see
;;;; docs/src/reference/commands/index.md).
(in-package #:aitools/cli)

(defvar %schema-registry% nil
  "Bound by REGISTER-SCHEMA-COMMAND to the COMMAND-REGISTRY the running
process built, so %SCHEMA-HANDLER (a cl-cli handler, which only receives the
INVOCATION) can still reach it.")

(defun %dispatch-names (registry words)
  "Turn schema's positional WORDS into dispatch names. A group word followed
by another word names that group's subcommand, as the agent would type it
(`schema json get read` -> \"json.get\", \"read\"); a dotted word is taken
as already joined."
  (let ((groups (command-registry-group-commands registry)))
    (loop while words
          collect (let ((word (pop words)))
                    (if (and words (gethash word groups))
                        (format nil "~A.~A" word (pop words))
                        word)))))

(defun %group-schemas (registry group)
  "Every registered COMMAND-SCHEMA whose dispatch name is GROUP.<subcommand>,
in registration order, when GROUP is a registered group; NIL otherwise. Lets
`schema <group>` list the group's subcommands, so the
`aitools schema <group>` repair a bare group dispatch emits is itself runnable
rather than failing with input.not-found."
  (when (nth-value 1 (gethash group (command-registry-group-commands registry)))
    (let ((prefix (concatenate 'string group ".")))
      (remove-if-not (lambda (schema)
                       (let ((n (aitools.protocol.domain:command-schema-name schema)))
                         (and (> (length n) (length prefix))
                              (string= prefix n :end2 (length prefix)))))
                     (all-command-schemas registry)))))

(defun %typed-name (schema)
  "SCHEMA's name as an agent types it (`json get`), not the dotted dispatch
name (`json.get`). `candidates` must read like render-command-summary's
`name`, not expose the internal dotted form."
  (substitute #\Space #\. (aitools.protocol.domain:command-schema-name schema)))

(defun %collect-requested-schemas (registry names)
  "(values FOUND MISSING) for the dispatch NAMES, both in request order: FOUND
holds the COMMAND-SCHEMAs they resolve to (a group word expands to its
subcommands' schemas), MISSING the names that name neither a command nor a
group."
  (let (found missing)
    (dolist (name (%dispatch-names registry names)
                  (values (nreverse found) (nreverse missing)))
      (let ((schema (find-command-schema registry name))
            (group-schemas (%group-schemas registry name)))
        (cond
          (schema (push schema found))
          (group-schemas (dolist (s group-schemas) (push s found)))
          (t (push name missing)))))))

(aitools.kernel.domain:define-flow
 (%schema-flow registry names all-p &key on-ok on-error)
 "NAMES is the (possibly empty) list of requested command names; ALL-P is
`--all`. Always returns a single \"commands\" field: an array of {name,
summary} plists for the zero-argument case, or of full detail objects for a
named or --all request -- one uniform shape rather than a flat object for a
single name and an array only otherwise."
 (cond
   ((and (null names) (not all-p))
    (aitools.kernel.domain:flow-continue
     :on-ok
     (list (cons "commands"
                 (mapcar (function aitools.protocol.application:render-command-summary)
                         (all-command-schemas registry))))))
   (all-p
    (aitools.kernel.domain:flow-continue
     :on-ok
     (list (cons "commands"
                 (mapcar (function aitools.protocol.application:render-command-detail)
                         (all-command-schemas registry))))))
   (t
    (multiple-value-bind (found missing) (%collect-requested-schemas registry names)
      (if missing
          (aitools.kernel.domain:flow-continue
           :on-error "input.not-found"
           (format nil "unknown command(s): ~{~A~^, ~}" missing)
           :candidates (mapcar (function %typed-name) (all-command-schemas registry))
           :repairs (list (list :action "browse-commands"
                                :detail "List every implemented command."
                                :command "aitools schema")))
          (aitools.kernel.domain:flow-continue
           :on-ok
           (list (cons "commands"
                       (mapcar (function aitools.protocol.application:render-command-detail)
                               found)))))))))

(defun %schema-handler (invocation)
  (let* ((names (positional-value invocation :commands))
         (all-p (option-value invocation :all)))
    (aitools.protocol.application:call-with-command-result/k
     (lambda (&key on-ok on-partial on-error)
       (declare (ignore on-partial))
       (aitools.kernel.domain:flow-call
           (%schema-flow %schema-registry% names all-p)
         (:on-ok (fields)
           (funcall on-ok fields))
         (:on-error (code message &key candidates diagnostics conflicts repairs)
           (funcall on-error code message
                    :candidates candidates
                    :diagnostics diagnostics
                    :conflicts conflicts
                    :repairs repairs)))))))

(defun register-schema-command (registry)
  (setf %schema-registry% registry)
  (aitools.protocol.application:register-command-declarations
   registry
   (list
    (aitools.protocol.application:define-command
        "schema"
      (:command-builder
       (lambda (ports)
         (declare (ignore ports))
         (make-command
          :name "schema"
          :description "List implemented commands, or show one command's full schema."
          :positionals (list (make-positional :name "commands" :key :commands :rest-p t :required-p nil))
          :options (list (make-option :name "all" :kind :flag :description "Show every command's full schema."))
          :handler (function %schema-handler)))
       :schema (aitools.protocol.domain:make-command-schema
                "schema" "List implemented commands, or show one command's full schema."
                :description (concatenate 'string
                                          "With no arguments, list every implemented command as {name, summary}. "
                                          "With command words (a group word "
                                          "followed by its subcommand, e.g. `schema json get`), return "
                                          "those commands' full detail. `--all` returns every command's detail.")
                :args (list (list :name "commands" :type "string" :required nil :repeatable t
                                  :description "Command names as typed, e.g. `read` or `json get`.")
                            (list :name "--all" :type "flag" :required nil
                                  :description "Return every command's full detail."))
                :output-fields (list (list :name "commands"
                                           :description "Array of {name, summary}, or of full detail objects."))
                :error-codes (list "input.not-found" "argument.invalid")))))
   nil)
  registry)
