;;;; packages/feature/inspect/src/presentation/registry.lisp
;;;;
;;;; How inspect's command files plug into REGISTER-INSPECT-COMMANDS: each
;;;; file declares its commands with DEFINE-INSPECT-COMMAND at top level
;;;; (a name, its group, the builder of its cl-cli command, and the schema
;;;; table holding its entry), and registration walks those declarations in
;;;; load order. Shared option constructors and the flow runner live here.
(in-package #:aitools.inspect.presentation)

(defvar *inspect-commands* '()
  "(name group builder schema-table) per command, in declaration order.")

(defun define-inspect-command (name group builder schema-table)
  "Declare command NAME (dotted when GROUP is given). BUILDER is a symbol
naming a function of PORTS returning the cl-cli command. SCHEMA-TABLE is
the list of schema plists holding NAME's entry."
  (setf *inspect-commands*
        (append (remove name *inspect-commands* :key #'first :test #'string=)
                (list (list name group builder schema-table))))
  name)

(defun %schema-entry (name table)
  (or (find name table :key (lambda (entry) (getf entry :name)) :test #'string=)
      (error "no schema entry for ~A" name)))

(defun %schema (name table)
  (let ((entry (%schema-entry name table)))
    (aitools.protocol.domain:make-command-schema
     name (getf entry :summary)
     :description (getf entry :description)
     :args (append (getf entry :args)
                   (when (getf entry :selectors) aitools.data:*inspect-selector-args*)
                   (unless (getf entry :no-tx) aitools.data:*inspect-common-args*))
     :output-fields (getf entry :output-fields)
     :error-codes (getf entry :error-codes))))

(defun command-summary (name table)
  (getf (%schema-entry name table) :summary))

(defun run-flow (flow &rest arguments)
  "Call FLOW with ARGUMENTS and the three command-result continuations and
return the COMMAND-RESULT."
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&rest continuations)
     (apply flow (append arguments continuations)))))

(defun context-arguments (invocation)
  "The global `--root` and `--lock-timeout` and the command's `--tx`, as
flow keyword arguments."
  (list :root (option-value invocation :root)
        :lock-timeout (option-value invocation :lock-timeout)
        :tx (option-value invocation :tx)))

(defun tx-option ()
  (make-option :name "tx" :kind :value :value-name "TX" :description "Read the tx's state."))

(defun integer-option (name default description &key (min 1))
  (make-option :name name :kind :value :type :integer :min min :default default :description description))

(defun flag-option (name description)
  (make-option :name name :kind :flag :description description))

(defun value-option (name description &key choices default value-name)
  (apply #'make-option :name name :kind :value :description description
         (append (when choices (list :choices choices))
                 (when default (list :default default))
                 (when value-name (list :value-name value-name)))))

(defun selector-options ()
  "The selectors other than --old, built from the shared
AITOOLS.DATA:*SELECTOR-OPTIONS* table."
  (mapcar (lambda (option)
            (let ((name (getf option :name))
                  (description (getf option :description))
                  (value-name (getf option :value-name)))
              (ecase (getf option :kind)
                (:flag (flag-option name description))
                (:value (value-option name description :value-name value-name))
                (:pair (make-option :name name :kind :value :value-count 2 :value-name value-name
                                    :description description)))))
          aitools.data:*selector-options*))

(defun selector-arguments (invocation)
  (list :range (option-value invocation :range)
        :symbol (option-value invocation :symbol)
        :kind (option-value invocation :kind)
        :between (option-value invocation :between)
        :exclusive (option-value invocation :exclusive)
        :match (option-value invocation :match)
        :invert (option-value invocation :invert)))

(defun register-inspect-commands (registry ports)
  "Register every declared inspect command on REGISTRY with PORTS, the
AITOOLS.INSPECT.APPLICATION:INSPECT-PORTS the composition root built."
  (loop for (name group builder table) in *inspect-commands*
        do (aitools.protocol.application:register-command
            registry :name name :group group
                     :cli-command (funcall builder ports)
                     :schema (%schema name table)))
  registry)
