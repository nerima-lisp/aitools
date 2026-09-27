;;;; packages/core/protocol/src/application/schema-flow.lisp
;;;;
;;;; `schema` rendering: the no-argument summary list ({name, summary} only,
;;;; so `schema` alone stays cheap to read) and the full per-command detail.
;;;; Each context's presentation layer builds the AITOOLS.PROTOCOL.DOMAIN:
;;;; COMMAND-SCHEMA values this consumes (design.md's "Command handler
;;;; contract"); src/schema.lisp collects them across every registered
;;;; command and calls these two renderers.
(in-package #:aitools.protocol.application)

(defun %json-key-name (keyword)
  (substitute #\_ #\- (string-downcase (symbol-name keyword))))

(defun %plist->json-object (plist)
  "Convert a flat plist (:KEY-NAME value ...) into a JSON object, mapping
each keyword to a lowercase, underscore-separated key name."
  (aitools.protocol.domain:json-object-from-alist
   (loop for (key value) on plist by #'cddr
         collect (cons (%json-key-name key) value))))

(defun %invocation-name (schema)
  "The command name as an agent types it: dispatch names use `.` between a
group and its subcommand (\"json.get\"), the CLI uses a space (\"json get\")."
  (substitute #\Space #\. (aitools.protocol.domain:command-schema-name schema)))

(defun render-command-summary (schema)
  (aitools.protocol.domain:json-object-from-alist
   (list (cons "name" (%invocation-name schema))
         (cons "summary" (aitools.protocol.domain:command-schema-summary schema)))))

(defun render-command-detail (schema)
  (aitools.protocol.domain:json-object-from-alist
   (list (cons "name" (%invocation-name schema))
         (cons "summary" (aitools.protocol.domain:command-schema-summary schema))
         (cons "description" (or (aitools.protocol.domain:command-schema-description schema)
                                 (aitools.protocol.domain:command-schema-summary schema)))
         (cons "args" (mapcar #'%plist->json-object (aitools.protocol.domain:command-schema-args schema)))
         (cons "output_fields"
               (mapcar #'%plist->json-object (aitools.protocol.domain:command-schema-output-fields schema)))
         (cons "error_codes" (aitools.protocol.domain:command-schema-error-codes schema)))))
