;;;; packages/core/protocol/src/application/command-registry.lisp
;;;;
;;;; The registration API every feature context's presentation layer targets.
;;;; A context exports `register-<context>-commands (registry ports)`, which
;;;; calls REGISTER-COMMAND once per command. The registry lives in protocol,
;;;; not in the composition root, because presentation may reference protocol
;;;; but not aitools/cli, and presentation modules load before aitools/cli.
;;;; CLI-COMMAND values are opaque here: the presentation layer builds them
;;;; with cl-cli, and only the composition root's FINALIZE-APP-COMMANDS
;;;; interprets them, so the "aitools" system stays free of cl-cli.
(in-package #:aitools.protocol.application)

(defstruct (command-declaration
            (:constructor make-command-declaration
                (name &key group command-builder schema))
            (:copier nil))
  "A command's protocol-level declaration.

COMMAND-BUILDER is called with the context's PORTS value and must return the
opaque CLI command. SCHEMA is the already-built COMMAND-SCHEMA. The
declaration deliberately contains no cl-cli types, keeping registration in
the protocol application layer."
  (name nil :type string :read-only t)
  (group nil :type (or null string) :read-only t)
  (command-builder nil :type function :read-only t)
  (schema nil :read-only t))

(defmacro define-command (name (&key group command-builder schema))
  "Construct a COMMAND-DECLARATION for use in a context's declaration list."
  `(make-command-declaration
    ,name :group ,group :command-builder ,command-builder :schema ,schema))

(defstruct (command-registry (:constructor make-command-registry ())
                             (:copier nil))
  "TOP-LEVEL accumulates ungrouped commands' cli-command values.
GROUP-COMMANDS maps a group name (\"json\", \"util\", ...) to the list of its
subcommands' cli-command values. SCHEMAS maps a full dispatch name (\"read\",
\"json.get\") to its AITOOLS.PROTOCOL.DOMAIN:COMMAND-SCHEMA."
  (top-level nil :type list)
  (group-commands (make-hash-table :test 'equal) :type hash-table)
  (schemas (make-hash-table :test 'equal) :type hash-table)
  (schema-order nil :type list))

(defun register-command (registry &key name group cli-command schema)
  "Register one command. NAME is its full dispatch name: the bare command
name (\"read\") when GROUP is NIL, or GROUP followed by a `.` and the
subcommand name (\"json.get\") when GROUP is given. CLI-COMMAND is a cl-cli
command named as the bare subcommand (\"get\", not \"json.get\") when GROUP is
given; the composition root supplies the group wrapper. SCHEMA's own NAME
should equal NAME."
  (check-type name string)
  (if group
      (push cli-command (gethash group (command-registry-group-commands registry)))
      (push cli-command (command-registry-top-level registry)))
  (setf (gethash name (command-registry-schemas registry)) schema)
  (push name (command-registry-schema-order registry))
  registry)

(defun register-command-declaration (registry declaration ports)
  "Build and register DECLARATION using PORTS, returning REGISTRY."
  (check-type declaration command-declaration)
  (register-command registry
                    :name (command-declaration-name declaration)
                    :group (command-declaration-group declaration)
                    :cli-command
                    (funcall (command-declaration-command-builder declaration) ports)
                    :schema (command-declaration-schema declaration)))

(defun register-command-declarations (registry declarations ports)
  "Register DECLARATIONS in list order and return REGISTRY."
  (dolist (declaration declarations registry)
    (register-command-declaration registry declaration ports)))

(defun find-command-schema (registry name)
  (gethash name (command-registry-schemas registry)))

(defun all-command-schemas (registry)
  "Every registered COMMAND-SCHEMA, in registration order."
  (mapcar (lambda (name) (find-command-schema registry name))
          (reverse (command-registry-schema-order registry))))
