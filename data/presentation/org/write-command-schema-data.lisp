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
              :description "Accepts one active timestamp for scheduled or deadline and one inactive timestamp for closed. Weekday tokens, diary expressions, date ranges, multiple repeater or warning tokens, and multiline values are rejected."
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
                                   *org-selection-error-codes*))
        (list :name "org.agenda"
              :summary "List Org agenda entries in an inclusive date range."
              :description (concatenate 'string
                                        "Both range endpoints are required. Future repeats default to all. The explicit time context controls relative timestamps. "
                                        *org-paths-description*)
              :args (append (list *org-paths-arg*)
                            '((:name "--from" :type "string" :required t
                               :description "Inclusive start date in YYYY-MM-DD form.")
                              (:name "--through" :type "string" :required t
                               :description "Inclusive end date in YYYY-MM-DD form.")
                              (:name "--future-repeats" :type "enum"
                               :choices ("all" "next" "none") :default "all"
                               :description "Include all future occurrences, the next one, or none."))
                            *org-time-context-args*)
              :output-fields '((:name "from" :description "Inclusive start date.")
                               (:name "through" :description "Inclusive end date.")
                               (:name "entries" :description "Entries are [{date,kind,line,title,todo,tags,path}] in date order within each file. Entries are grouped by path in lexicographic order.")
                               (:name "count" :description "Number of agenda entries."))
              :error-codes (append *org-common-read-error-codes* '("argument.invalid" "refusal.outside-workspace" "refusal.not-a-file")))
        (list :name "org.clock.in"
              :summary "Start a clock on one matching heading."
              :description "Requires an explicit local time context. A heading with an existing running clock is rejected."
              :args (append '((:name "path" :kind "positional" :type "string" :required t
                               :description "Org file inside the selected workspace root."))
                            *org-selector-time-args*
                            *org-write-control-args*)
              :output-fields (append
                              '((:name "timestamp" :description "Timestamp opened for the clock.")
                                (:name "changed" :description "True when a clock was started."))
                              *org-write-output-fields*)
              :error-codes (append *org-common-write-error-codes*
                                   *org-selection-error-codes*))
        (list :name "org.clock.out"
              :summary "Stop the running clock on one matching heading."
              :description "The selected heading must contain exactly one running clock."
              :args (append '((:name "path" :kind "positional" :type "string" :required t
                               :description "Org file inside the selected workspace root."))
                            *org-selector-time-args*
                            *org-write-control-args*)
              :output-fields (append
                              '((:name "interval" :description "Closed clock interval and elapsed duration.")
                                (:name "changed" :description "True when the running clock was closed."))
                              *org-write-output-fields*)
              :error-codes (append *org-common-write-error-codes*
                                   *org-selection-error-codes*))
        (list :name "org.clock.report"
              :summary "Summarize clock time for one heading and its descendants."
              :description "The range is half-open: from is included and through is excluded. Running clocks are omitted."
              :args (append '((:name "path" :kind "positional" :type "string" :required t
                               :description "Org file to read.")
                              (:name "--from" :type "string" :required t
                               :description "Inclusive start of the half-open local date-time range.")
                              (:name "--through" :type "string" :required t
                               :description "Exclusive end of the half-open local date-time range."))
                            *org-heading-selector-args*
                            *org-time-context-args*)
              :output-fields '((:name "from" :description "Inclusive range start.")
                               (:name "through" :description "Exclusive range end.")
                               (:name "total_minutes" :description "Total minutes across selected rows.")
                               (:name "rows" :description "[{level,title,tags,minutes}] by heading."))
              :error-codes (append *org-common-read-error-codes*
                                   *org-selection-error-codes*
                                   '("argument.invalid")))
        (list :name "org.table.align"
              :summary "Align one numbered Org table."
              :description "Selects the table by its one-based document order. No table is selected implicitly."
              :args (append '((:name "path" :kind "positional" :type "string" :required t
                               :description "Org file inside the selected workspace root.")
                              (:name "--table" :type "integer" :required t :min 1
                               :description "One-based table number in the document."))
                            *org-write-control-args*)
              :output-fields (append
                              '((:name "table" :description "One-based table number.")
                                (:name "changed" :description "True when table spacing changed."))
                              *org-write-output-fields*)
              :error-codes (append *org-common-write-error-codes*
                                   '("argument.invalid")))
        (list :name "org.table.sort"
              :summary "Sort the standard rows of one numbered Org table."
              :description "Rows are one-based and exclude horizontal rules. Type defaults to lexical and order to ascending. Start and end must be supplied together and must remain inside one horizontal-rule-delimited section."
              :args (append '((:name "path" :kind "positional" :type "string" :required t
                               :description "Org file inside the selected workspace root.")
                              (:name "--table" :type "integer" :required t :min 1
                               :description "One-based table number in the document.")
                              (:name "--column" :type "integer" :required t :min 1
                               :description "One-based column to compare.")
                              (:name "--type" :type "enum" :choices ("lexical" "numeric")
                               :default "lexical" :description "Cell comparison type.")
                              (:name "--order" :type "enum" :choices ("ascending" "descending")
                               :default "ascending" :description "Sort direction.")
                              (:name "--start" :type "integer" :min 1
                               :description "First standard row in the inclusive sort range.")
                              (:name "--end" :type "integer" :min 1
                               :description "Last standard row in the inclusive sort range."))
                            *org-write-control-args*)
              :output-fields (append
                              '((:name "table" :description "One-based table number.")
                                (:name "changed" :description "True when row order changed."))
                              *org-write-output-fields*)
              :error-codes (append *org-common-write-error-codes*
                                   '("argument.invalid")))
        (list :name "org.table.recalculate"
              :summary "Recalculate formulas for one numbered Org table."
              :description "Uses the first formula directive following the selected table. No directive is a successful no-op. Formula failures reject the whole command."
              :args (append '((:name "path" :kind "positional" :type "string" :required t
                               :description "Org file inside the selected workspace root.")
                              (:name "--table" :type "integer" :required t :min 1
                               :description "One-based table number in the document."))
                            *org-write-control-args*)
              :output-fields (append
                              '((:name "table" :description "One-based table number.")
                                (:name "changed" :description "True when a formula changed cell values."))
                              *org-write-output-fields*)
              :error-codes *org-common-write-error-codes*))))
(setf *org-command-schemas* (append *org-command-schemas* (list (list :name "org.priority.set" :summary "Set the priority of one heading." :description "Set the priority of one heading." :args (append '((:name "path" :kind "positional" :type "string" :required t :description "Org file inside the selected workspace root.")) *org-selector-time-args* '((:name "--priority" :type "string" :required t :description "Priority value, for example A.")) *org-write-control-args*) :output-fields (append '((:name "changed" :description "True when the document changed.")) *org-write-output-fields*) :error-codes (append *org-common-write-error-codes* *org-selection-error-codes*))
(list :name "org.priority.delete" :summary "Remove the priority of one heading." :description "Remove the priority of one heading." :args (append '((:name "path" :kind "positional" :type "string" :required t :description "Org file inside the selected workspace root.")) *org-selector-time-args* nil *org-write-control-args*) :output-fields (append '((:name "changed" :description "True when the document changed.")) *org-write-output-fields*) :error-codes (append *org-common-write-error-codes* *org-selection-error-codes*))
(list :name "org.tags.add" :summary "Add local tags to one heading." :description "Add local tags to one heading." :args (append '((:name "path" :kind "positional" :type "string" :required t :description "Org file inside the selected workspace root.")) *org-selector-time-args* '((:name "--tags" :type "string" :required t :description "Comma-separated tag names.")) *org-write-control-args*) :output-fields (append '((:name "changed" :description "True when the document changed.")) *org-write-output-fields*) :error-codes (append *org-common-write-error-codes* *org-selection-error-codes*))
(list :name "org.tags.remove" :summary "Remove local tags from one heading." :description "Remove local tags from one heading." :args (append '((:name "path" :kind "positional" :type "string" :required t :description "Org file inside the selected workspace root.")) *org-selector-time-args* '((:name "--tags" :type "string" :required t :description "Comma-separated tag names.")) *org-write-control-args*) :output-fields (append '((:name "changed" :description "True when the document changed.")) *org-write-output-fields*) :error-codes (append *org-common-write-error-codes* *org-selection-error-codes*))
(list :name "org.tags.replace" :summary "Replace local tags on one heading." :description "Replace local tags on one heading." :args (append '((:name "path" :kind "positional" :type "string" :required t :description "Org file inside the selected workspace root.")) *org-selector-time-args* '((:name "--tags" :type "string" :required t :description "Comma-separated tag names; an empty value clears them.")) *org-write-control-args*) :output-fields (append '((:name "changed" :description "True when the document changed.")) *org-write-output-fields*) :error-codes (append *org-common-write-error-codes* *org-selection-error-codes*))
(list :name "org.heading.rename" :summary "Change the text of one heading." :description "Change the text of one heading." :args (append '((:name "path" :kind "positional" :type "string" :required t :description "Org file inside the selected workspace root.")) *org-selector-time-args* '((:name "--title" :type "string" :required t :description "New heading text; existing statistics cookies are retained.")) *org-write-control-args*) :output-fields (append '((:name "changed" :description "True when the document changed.")) *org-write-output-fields*) :error-codes (append *org-common-write-error-codes* *org-selection-error-codes*))
(list :name "org.checkbox.toggle"
      :summary "Toggle one checkbox and update statistics cookies."
      :description "Changes [X] to [ ], and [ ] or [-] to [X]. Recalculates every statistics cookie owned by the selected heading. Parent item checkboxes are not changed."
      :args (append '((:name "path" :kind "positional" :type "string" :required t
                       :description "Org file inside the selected workspace root."))
                    *org-selector-time-args*
                    '((:name "--item" :type "integer" :required t :min 1
                       :description "One-based checkbox number in document order, including nested items and inline tasks but excluding child headings."))
                    *org-write-control-args*)
      :output-fields (append '((:name "item" :description "One-based checkbox number.")
                               (:name "changed" :description "True when the document changed."))
                             *org-write-output-fields*)
      :error-codes (append *org-common-write-error-codes* *org-selection-error-codes*)))))

(setf *org-command-schemas*
      (append *org-command-schemas*
              (list (list :name "org.archive"
                          :summary "Move one heading subtree to an archive file."
                          :description "Creates the destination file and heading when absent. Archive metadata records the source path relative to the destination."
                          :args (append '((:name "path" :kind "positional" :type "string" :required t :description "Source Org file."))
                                        *org-selector-time-args*
                                        '((:name "--to" :type "string" :required t :description "Destination Org file.")
                                          (:name "--target-heading" :type "string" :required t :description "Destination heading title."))
                                        *org-write-control-args*)
                          :output-fields (append '((:name "changed" :description "True when the subtree was moved.")) *org-write-output-fields*)
                          :error-codes (append *org-common-write-error-codes* *org-selection-error-codes*)))))
