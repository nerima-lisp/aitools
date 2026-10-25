;;;;
;;;; The two envelope shapes. Built directly as JSON-KIT ordered
;;;; JSON-OBJECTs (:DUPLICATE-KEY-POLICY :PRESERVE) rather than plain alists,
;;;; so member order -- and therefore the "same input, byte-identical
;;;; output" -- is fixed by construction instead of depending on a
;;;; hash-table's unspecified iteration order. json-kit is a pure kit, so
;;;; this stays within what the layer table allows a domain layer to depend
;;;; on (docs/src/reference/architecture.md).
(in-package #:aitools.protocol.domain)

(defun json-object-from-alist (alist)
  "Build an ordered JSON-KIT JSON-OBJECT from ALIST (a list of (STRING .
VALUE) conses), preserving ALIST's order. One of three seams (with
JSON-OBJECT-P and JSON-OBJECT-MEMBERS below) through which
AITOOLS.PROTOCOL.APPLICATION -- which may not depend on json-kit directly,
per the layer table in docs/src/reference/architecture.md -- builds and
walks a JSON object."
  (json-kit:alist->json-object alist :duplicate-key-policy :preserve))

(defun json-object (&rest keys-and-values)
  "Build an ordered JSON-KIT JSON-OBJECT from alternating string KEY and
VALUE arguments, preserving argument order. The plist companion to
JSON-OBJECT-FROM-ALIST for the common literal-key case; every context builds
its result objects through one of these two."
  (json-object-from-alist
   (loop for (key value) on keys-and-values by #'cddr collect (cons key value))))

(defun json-null ()
  "The JSON null value (json-kit's sentinel). NIL would serialize as [], so
an absent scalar is built through this."
  json-kit:+json-null+)

(defun json-or-null (value)
  "VALUE, or JSON null when VALUE is NIL."
  (if (null value) (json-null) value))

(defun json-boolean (value)
  "VALUE as a JSON boolean: T for true, json-kit's false sentinel otherwise
(NIL would serialize as [])."
  (if value t json-kit:+json-false+))

(defun json-object-p (value)
  (json-kit:json-object-p value))

(defun json-object-members (value)
  (json-kit:json-object-members value))

(defun repair (action detail command)
  "Build a repair plist with ACTION, DETAIL, and runnable COMMAND."
  (list :action action :detail detail :command command))

(defun schema-repair (command)
  "Build the standard schema repair for the complete COMMAND string."
  (repair "inspect-schema" "Show this command's arguments and rules." command))

(defun %recovered-entry (entry)
  (json-object-from-alist (list (cons "op_id" (getf entry :op-id)) (cons "action" (getf entry :action)))))

(defun make-ok-envelope (command fields &key (status "ok") next-commands recovered meta)
  "Build a schema version 2 success envelope. COMMAND is the dispatched command's name.
FIELDS is an alist of (STRING . VALUE) command-specific fields, in the order
they should appear after \"command\". STATUS is \"ok\" or \"partial\".
NEXT-COMMANDS, RECOVERED, and META are omitted when NIL; META follows the
payload fields."
  (json-object-from-alist
   (append (list (cons "schema_version" 2) (cons "status" status) (cons "command" command))
           fields
           (when next-commands (list (cons "next_commands" next-commands)))
           (when recovered (list (cons "recovered" (mapcar #'%recovered-entry recovered))))
           (when meta (list (cons "meta" meta))))))

(defun %repair-entry (repair)
  (json-object-from-alist (list (cons "action" (getf repair :action))
                      (cons "detail" (getf repair :detail))
                      (cons "command" (getf repair :command)))))

(defun make-error-envelope (command code message &key repairs candidates diagnostics conflicts recovered meta)
  "Build a schema version 2 error envelope for error.code CODE. REPAIRS is
required and non-empty; every error carries at least one
`repairs[].command` (docs/src/reference/errors.md), as a list of
(:ACTION :DETAIL :COMMAND) plists. CANDIDATES, DIAGNOSTICS, and CONFLICTS are
command-specific JSON values included only when given. RECOVERED and META
follow the error payload when present."
  (unless repairs
    (error "make-error-envelope: ~A/~A has no repairs, but every error requires at least one" command code))
  (json-object-from-alist
   (append
    (list (cons "schema_version" 2) (cons "status" "error") (cons "command" command)
          (cons "error"
                (json-object-from-alist
                 (append (list (cons "code" code) (cons "message" message)
                              (cons "exit_code" (error-code-exit-code code))
                              (cons "repairs" (mapcar #'%repair-entry repairs)))
                         (when candidates (list (cons "candidates" candidates)))
                         (when diagnostics (list (cons "diagnostics" diagnostics)))
                         (when conflicts (list (cons "conflicts" conflicts)))))))
    (when recovered (list (cons "recovered" (mapcar #'%recovered-entry recovered))))
    (when meta (list (cons "meta" meta))))))
