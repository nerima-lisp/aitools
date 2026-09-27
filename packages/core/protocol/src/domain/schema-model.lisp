;;;; packages/core/protocol/src/domain/schema-model.lisp
;;;;
;;;; The per-command `schema` value. Deliberately loose on ARGS/OUTPUT-
;;;; FIELDS: each presentation layer knows its own command's arguments and
;;;; output fields best, so this only fixes the four fields every command's
;;;; schema shares (name, one-line summary, longer description, and its
;;;; declared error.codes) and passes the rest through as data.
(in-package #:aitools.protocol.domain)

(defstruct (command-schema
            (:constructor make-command-schema
                (name summary &key description (args nil) (output-fields nil) (error-codes nil)))
            (:copier nil))
  "NAME is the full dispatch name (\"read\", \"json.get\"). ARGS is a list of
plists (:name :type :required :default :description ...) describing
positionals and options, including which selectors/guards the command
accepts. OUTPUT-FIELDS is a list of plists (:name :description) describing
the success envelope's command-specific fields. ERROR-CODES is the list of
error.code strings this command can return."
  (name nil :type string :read-only t)
  (summary nil :type string :read-only t)
  (description nil :type (or null string) :read-only t)
  (args nil :type list :read-only t)
  (output-fields nil :type list :read-only t)
  (error-codes nil :type list :read-only t))
