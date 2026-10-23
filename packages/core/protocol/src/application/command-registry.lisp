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
            (:constructor %make-command-declaration
                          (name &key group command-builder schema))
            (:copier nil)
            (:predicate nil))
           "A command's protocol-level declaration.

COMMAND-BUILDER is called with the context's PORTS value and must return the
opaque CLI command. SCHEMA is the already-built COMMAND-SCHEMA. The
declaration deliberately contains no cl-cli types, keeping registration in
the protocol application layer."
           (name nil :type string :read-only t)
           (group nil :type (or null string) :read-only t)
           (command-builder nil :type function :read-only t)
           (schema nil :read-only t))

(defun %validate-command-declaration-values (name group command-builder schema)
  (labels ((valid-dotted-path-p (path)
             (and (stringp path)
                  (plusp (length path))
                  (not (char= #\. (char path 0)))
                  (not (char= #\. (char path (1- (length path)))))
                  (not (search ".." path))
                  (notany (lambda (character)
                            (find character '(#\Space #\Tab #\Newline #\Return)))
                          path))))
    (unless (and (stringp name) (plusp (length name)))
      (error "A command name must be a non-empty string, got ~S." name))
    (unless (or (null group) (and (stringp group) (plusp (length group))))
      (error "A command group must be NIL or a non-empty string, got ~S." group))
    (unless (functionp command-builder)
      (error "A command builder must be a function, got ~S." command-builder))
    (unless (aitools.protocol.domain:command-schema-p schema)
      (error "A command schema must be a COMMAND-SCHEMA, got ~S." schema))
    (unless (string= name (aitools.protocol.domain:command-schema-name schema))
      (error "Command name ~S does not match schema name ~S."
             name (aitools.protocol.domain:command-schema-name schema)))
    (when group
      (unless (and (valid-dotted-path-p group)
                   (valid-dotted-path-p name)
                   (let ((prefix (concatenate 'string group ".")))
                     (and (< (length prefix) (length name))
                          (string= prefix name :end2 (length prefix))
                          (not (position #\. name :start (length prefix))))))
        (error "Grouped command name ~S must have the form ~S.<command>." name group)))
    (values name group command-builder schema)))

(defun make-command-declaration (name &key group command-builder schema)
  (%validate-command-declaration-values name group command-builder schema)
  (%make-command-declaration name :group group
                             :command-builder command-builder :schema schema))

(defun %command-literal-form-value (form)
  (cond
   ((or (stringp form) (numberp form) (characterp form) (keywordp form)
        (vectorp form) (member form '(nil t)))
    (values t form))
   ((and (consp form)
         (eq (car form) 'quote)
         (consp (cdr form))
         (null (cddr form)))
    (values t (cadr form)))
   (t (values nil nil))))

(defun %validate-command-literal-form (field form)
  (multiple-value-bind (literal value) (%command-literal-form-value form)
    (when literal
      (let ((valid-p
             (case field
               (:name (and (stringp value) (plusp (length value))))
               (:group (or (null value)
                           (and (stringp value) (plusp (length value))))))))
        (unless valid-p
          (error "Invalid literal for DEFINE-COMMAND ~S: ~S." field value)))))
  form)

(defun %parse-define-command-options (options)
  (unless (and (listp options) (evenp (length options)))
    (error "DEFINE-COMMAND options must be a property list."))
  (let ((group nil)
        (command-builder nil)
        (schema nil)
        (seen nil))
    (loop for (key value) on options by #'cddr
          do (unless (member key '(:group :command-builder :schema) :test #'eq)
               (error "Unknown DEFINE-COMMAND option ~S." key))
          (when (member key seen :test #'eq)
            (error "Duplicate DEFINE-COMMAND option ~S." key))
          (push key seen)
          (case key
            (:group (setf group value))
            (:command-builder (setf command-builder value))
            (:schema (setf schema value))))
    (unless (member :command-builder seen :test #'eq)
      (error "DEFINE-COMMAND requires :COMMAND-BUILDER."))
    (unless (member :schema seen :test #'eq)
      (error "DEFINE-COMMAND requires :SCHEMA."))
    (values group command-builder schema)))

(defmacro define-command (name options)
  "Construct a validated COMMAND-DECLARATION from a command name and options."
  (multiple-value-bind (group command-builder schema)
      (%parse-define-command-options options)
    (%validate-command-literal-form :name name)
    (%validate-command-literal-form :group group)
    (let ((name-var (gensym "COMMAND-NAME-"))
          (group-var (gensym "COMMAND-GROUP-"))
          (builder-var (gensym "COMMAND-BUILDER-"))
          (schema-var (gensym "COMMAND-SCHEMA-")))
      `(let* ((,name-var ,name)
              (,group-var ,group)
              (,builder-var ,command-builder)
              (,schema-var ,schema))
         (make-command-declaration
          ,name-var :group ,group-var
          :command-builder ,builder-var :schema ,schema-var)))))

(defstruct (command-registry (:constructor make-command-registry ())
                             (:copier nil))
  "TOP-LEVEL accumulates ungrouped commands' cli-command values.
GROUP-COMMANDS maps each dot-separated group path, including implicit parent
paths, to its direct subcommands' cli-command values. SCHEMAS maps a full
dispatch name (\"read\", \"json.get\", or \"org.todo.set\") to its
AITOOLS.PROTOCOL.DOMAIN:COMMAND-SCHEMA."
  (top-level nil :type list)
  (group-commands (make-hash-table :test 'equal) :type hash-table)
  (schemas (make-hash-table :test 'equal) :type hash-table)
  (schema-order nil :type list))

(defun register-command (registry &key name group cli-command schema)
  "Register one command. NAME is the bare command when GROUP is NIL, or GROUP
followed by a dot and one subcommand token. GROUP is a dot-separated path;
the registry creates any missing parent paths for nested groups. CLI-COMMAND
is named as the final token, and the composition root supplies the wrappers."
  (check-type name string)
  (if group
      (let ((groups (command-registry-group-commands registry))
            (start 0))
        (loop for separator = (position #\. group :start start)
              while separator
              do (let ((prefix (subseq group 0 separator)))
                   (unless (nth-value 1 (gethash prefix groups))
                     (setf (gethash prefix groups) nil)))
                 (setf start (1+ separator)))
        (push cli-command (gethash group groups)))
      (push cli-command (command-registry-top-level registry)))
  (setf (gethash name (command-registry-schemas registry)) schema)
  (push name (command-registry-schema-order registry))
  registry)

(defun %validate-command-declaration (declaration)
  (check-type declaration command-declaration)
  (%validate-command-declaration-values
   (command-declaration-name declaration)
   (command-declaration-group declaration)
   (command-declaration-command-builder declaration)
   (command-declaration-schema declaration))
  declaration)

(defun %ensure-command-name-available (registry name)
  (when (nth-value 1 (gethash name (command-registry-schemas registry)))
    (error "Command ~S is already registered." name)))

(defun register-command-declarations (registry declarations ports)
  "Register DECLARATIONS in list order and return REGISTRY."
  (check-type registry command-registry)
  (check-type declarations list)
  (let ((seen nil))
    (dolist (declaration declarations)
      (%validate-command-declaration declaration)
      (let ((name (command-declaration-name declaration)))
        (%ensure-command-name-available registry name)
        (when (member name seen :test #'string=)
          (error "Command ~S is declared more than once." name))
        (push name seen))))
  (let ((built
         (mapcar (lambda (declaration)
                   (cons declaration
                         (funcall (command-declaration-command-builder declaration) ports)))
                 declarations)))
    (dolist (entry built registry)
      (let ((declaration (car entry)))
        (register-command registry
                          :name (command-declaration-name declaration)
                          :group (command-declaration-group declaration)
                          :cli-command (cdr entry)
                          :schema (command-declaration-schema declaration))))))

(defun find-command-schema (registry name)
  (gethash name (command-registry-schemas registry)))

(defun all-command-schemas (registry)
  "Every registered COMMAND-SCHEMA, in registration order."
  (mapcar (lambda (name) (find-command-schema registry name))
          (reverse (command-registry-schema-order registry))))
