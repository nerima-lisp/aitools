;;;; packages/feature/edit/src/presentation/commands.lisp
;;;;
;;;; cl-cli commands and `schema` entries for every edit command, built from
;;;; the application's command table (the same one `tx rebase` parses
;;;; recorded argv with). Each handler gathers the positionals and options
;;;; into a plist and hands them to RUN-EDIT-COMMAND.
(in-package #:aitools.edit.presentation)

(defun %error-codes (spec)
  (let ((include (getf spec :include)))
    (append aitools.data:*edit-common-error-codes*
            (and (member :hash include) '("refusal.target-changed"))
            (and (member :count include) '("selection.count-mismatch"))
            (and (member :selectors include)
                 '("selection.no-match" "selection.ambiguous" "input.syntax-error" "input.unsupported-language"))
            (and (member (getf spec :name) '("edit" "insert" "replace" "apply" "transform" "move-lines"
                                             "json.set" "json.delete" "json.merge" "json.patch" "json.fmt" "table.set")
                         :test #'string=)
                 '("input.not-utf8"))
            (and (member (getf spec :name) '("edit" "apply" "json.patch") :test #'string=)
                 '("selection.no-match" "selection.ambiguous"))
            (and (member (getf spec :name) '("transcode" "json.set" "json.delete" "json.merge" "json.patch" "json.fmt"
                                             "table.set" "archive.extract")
                         :test #'string=)
                 '("input.unsupported-format" "input.syntax-error"))
            (and (member (getf spec :name) '("copy" "archive.extract") :test #'string=) '("refusal.too-large")))))

(defun %schema (spec)
  (aitools.protocol.domain:make-command-schema
   (getf spec :name) (getf spec :summary)
   :description (getf spec :description)
   :args (append
          (mapcar (lambda (positional)
                    (list :name (getf positional :name) :kind "positional" :type "string"
                          :required (not (or (getf positional :optional) (getf positional :rest)))
                          :description (if (getf positional :rest) "Repeatable." "")))
                  (getf spec :positionals))
          (mapcar (lambda (option)
                    (append (list :name (format nil "--~A" (getf option :name))
                                  :type (ecase (getf option :kind)
                                          (:flag "flag") (:value "string") (:multi "string, repeatable")
                                          (:pair "two strings"))
                                  :description (getf option :description))
                            (and (getf option :default) (list :default (getf option :default)))))
                  (getf spec :options)))
   :output-fields (append (and (member :write (getf spec :include)) aitools.data:*edit-write-output-fields*)
                          (and (member :count (getf spec :include)) aitools.data:*edit-count-output-fields*)
                          (getf spec :output-fields))
   :error-codes (remove-duplicates (%error-codes spec) :test #'string= :from-end t)))

(defun %cli-option (option)
  (let ((name (getf option :name)) (key (getf option :key)) (description (getf option :description)))
    (ecase (getf option :kind)
      (:flag (make-option :key key :name name :kind :flag :description description))
      (:value (make-option :key key :name name :kind :value :description description))
      (:multi (make-option :key key :name name :kind :value :multiple-p t :description description))
      (:pair (make-option :key key :name name :kind :value :value-count 2 :description description)))))

(defun %cli-positional (positional)
  (make-positional :key (getf positional :key) :name (getf positional :name)
                   :required-p nil :rest-p (and (getf positional :rest) t)))

(defun %invocation-arguments (spec invocation)
  "(values positionals options) of INVOCATION for SPEC."
  (values (loop for positional in (getf spec :positionals)
                for value = (positional-value invocation (getf positional :key))
                if (getf positional :rest) append value
                else if value collect value)
          (loop for option in (getf spec :options)
                for value = (option-value invocation (getf option :key))
                when value append (list (getf option :key) value))))

(defun %handler (spec ports)
  (lambda (invocation)
    (multiple-value-bind (positionals options) (%invocation-arguments spec invocation)
      (aitools.protocol.application:call-with-command-result/k
       (lambda (&key on-ok on-partial on-error)
         (aitools.edit.application:run-edit-command
          ports (getf spec :name) positionals options
          :root (option-value invocation :root)
          :lock-timeout (option-value invocation :lock-timeout)
          :display-argv (rest (cl-cli:invocation-raw-argv invocation))
          :on-ok on-ok :on-partial on-partial :on-error on-error))))))

(defun register-edit-commands (registry ports)
  "Register every edit command (text edits, file operations, and the write
side of the format groups) on REGISTRY. PORTS is the AITOOLS.EDIT.APPLICATION:EDIT-PORTS the composition
root built."
  (dolist (spec (aitools.edit.application:edit-command-specs) registry)
    (let* ((name (getf spec :name))
           (words (aitools.edit.application:command-words name)))
      (aitools.protocol.application:register-command
       registry :name name :group (and (rest words) (first words))
                :cli-command (make-command :name (car (last words))
                                           :description (getf spec :summary)
                                           :positionals (mapcar #'%cli-positional (getf spec :positionals))
                                           :options (mapcar #'%cli-option (getf spec :options))
                                           :handler (%handler spec ports))
                :schema (%schema spec)))))
