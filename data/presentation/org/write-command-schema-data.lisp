(in-package #:aitools.data)

(defparameter *org-time-context-args*
  '((:name "--at" :type "string" :required t
     :description "Zone-less local date-time in YYYY-MM-DDTHH:MM:SS form; seconds must be 00.")
    (:name "--tz" :type "string" :required t
     :description "Resolvable IANA time zone name.")
    (:name "--dst-policy" :type "enum" :choices ("compatible" "earlier" "later" "strict")
     :default "compatible"
     :description "DST gap and overlap policy for local timestamps.")))

(defparameter *org-heading-selector-args*
  '((:name "--match" :type "string"
     :description "Org match expression selecting exactly one heading. Give exactly one of --match and --heading-path.")
    (:name "--heading-path" :type "string"
     :description "The heading_path of one `org query` match, passed as the JSON array it reports, for example [{\"title\":\"Project\",\"index\":1},{\"title\":\"Tasks\",\"index\":2}]. Selects the heading whose ancestor titles and same-title sibling indices (one-based) match; selection.no-match when none does.")))

(defparameter *org-selector-time-args*
  (append *org-heading-selector-args* *org-time-context-args*))

(defparameter *org-write-control-args*
  '((:name "--dry-run" :type "flag"
     :description "Validate and show the diff without changing disk or a transaction overlay.")
    (:name "--tx" :type "string"
     :description "Stage the write in this transaction.")))

(defparameter *org-write-output-fields*
  '((:name "changes" :description "Standard write changes with path, action, hashes, and a diff when available.")
    (:name "op_id" :description "Journal operation id for undo; null when nothing changed.")
    (:name "tx" :description "Transaction receiving the staged write.")
    (:name "tx_op" :description "Staged operation number in the transaction.")
    (:name "dry_run" :description "True when the write was validated without being applied.")
    (:name "next_commands" :description "Command to read a full diff when changes.diff_truncated is true.")))

(defparameter *org-common-read-error-codes*
  '("input.not-found" "input.not-utf8" "input.unsupported-format"
    "input.syntax-error" "refusal.too-large" "environment.io"
    "internal.unexpected"))

(defparameter *org-common-write-error-codes*
  '("argument.invalid" "input.not-found" "input.not-utf8"
    "input.unsupported-format" "input.syntax-error"
    "refusal.not-a-file" "refusal.outside-workspace" "refusal.target-changed"
    "refusal.redacted-input" "refusal.too-large"
    "environment.busy" "environment.io" "internal.unexpected"))

(defparameter *org-selection-error-codes*
  '("selection.no-match" "selection.ambiguous"))

(setf *org-command-schemas*
      (append
       *org-command-schemas*
       (list
        (list :name "org.todo.set"
              :summary "Change the TODO keyword on one matching heading."
              :description "The state must be defined in the document TODO workflow. The optional note follows the document TODO logging policy."
              :args (append '((:name "path" :kind "positional" :type "string" :required t
                               :description "Org file inside the selected workspace root."))
                            *org-selector-time-args*
                            '((:name "--state" :type "string" :required t
                               :description "TODO keyword defined by the document.")
                              (:name "--note" :type "string"
                               :description "Optional TODO logging note."))
                            *org-write-control-args*)
              :output-fields (append
                              '((:name "previous_state" :description "Previous TODO keyword, or null.")
                                (:name "new_state" :description "New TODO keyword.")
                                (:name "changed" :description "True when the selected heading changed."))
                              *org-write-output-fields*)
              :error-codes (append *org-common-write-error-codes*
                                   *org-selection-error-codes*))
        (list :name "org.property.set"
              :summary "Set one property on one matching heading."
              :description "Adds an unregistered property when needed. The property value must be a single line."
              :args (append '((:name "path" :kind "positional" :type "string" :required t
                               :description "Org file inside the selected workspace root."))
                            *org-selector-time-args*
                            '((:name "--name" :type "string" :required t
                               :description "Property name.")
                              (:name "--value" :type "string" :required t
                               :description "Single-line property value."))
                            *org-write-control-args*)
              :output-fields (append
                              '((:name "property" :description "Property name.")
                                (:name "changed" :description "True when the property changed."))
                              *org-write-output-fields*)
              :error-codes (append *org-common-write-error-codes*
                                   *org-selection-error-codes*))
        (list :name "org.property.delete"
              :summary "Delete one property from one matching heading."
              :description "A missing property is a no-op. Duplicate same-name properties in the drawer are rejected as ambiguous."
              :args (append '((:name "path" :kind "positional" :type "string" :required t
                               :description "Org file inside the selected workspace root."))
                            *org-selector-time-args*
                            '((:name "--name" :type "string" :required t
                               :description "Property name."))
                            *org-write-control-args*)
              :output-fields (append
                              '((:name "property" :description "Property name.")
                                (:name "changed" :description "True when the property was removed."))
                              *org-write-output-fields*)
              :error-codes (append *org-common-write-error-codes*
                                   *org-selection-error-codes*))
        (list :name "org.planning.set"
              :summary "Set one planning timestamp on one matching heading."
              :description "Accepts one active timestamp for scheduled or deadline and one inactive timestamp for closed. Weekday tokens, diary expressions, date ranges, multiple repeater or warning tokens, and multiline values are rejected. The heading stores the timestamp with the weekday Org writes after the date, so `<2026-10-20>` becomes `<2026-10-20 Tue>`."
              :args (append '((:name "path" :kind "positional" :type "string" :required t
                               :description "Org file inside the selected workspace root."))
                            *org-selector-time-args*
                            '((:name "--kind" :type "enum" :required t
                               :choices ("scheduled" "deadline" "closed")
                               :description "Planning kind to set.")
                              (:name "--timestamp" :type "string" :required t
                               :description "One timestamp in the supported Org planning subset."))
                            *org-write-control-args*)
              :output-fields (append
                              '((:name "kind" :description "Planning kind.")
                                (:name "timestamp" :description "Timestamp written to the selected heading.")
                                (:name "changed" :description "True when the planning timestamp changed."))
                              *org-write-output-fields*)
              :error-codes (append *org-common-write-error-codes*
                                   *org-selection-error-codes*))
        (list :name "org.planning.delete"
              :summary "Delete one planning timestamp from one matching heading."
              :description "A missing timestamp of the requested kind is a no-op."
              :args (append '((:name "path" :kind "positional" :type "string" :required t
                               :description "Org file inside the selected workspace root."))
                            *org-selector-time-args*
                            '((:name "--kind" :type "enum" :required t
                               :choices ("scheduled" "deadline" "closed")
                               :description "Planning kind to delete."))
                            *org-write-control-args*)
              :output-fields (append
                              '((:name "kind" :description "Planning kind.")
                                (:name "changed" :description "True when the timestamp was removed."))
                              *org-write-output-fields*)
              :error-codes (append *org-common-write-error-codes*
                                   *org-selection-error-codes*)))))
