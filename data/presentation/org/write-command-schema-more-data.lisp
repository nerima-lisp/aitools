(in-package #:aitools.data)

(setf *org-command-schemas*
      (append
       *org-command-schemas*
       (list
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
              :description "The range is half-open: from is included and through is excluded. Running clocks are omitted. A CLOCK line that cannot be parsed is left out of the totals and reported in warnings."
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
                               (:name "rows" :description "[{level,title,tags,minutes}] by heading.")
                               (:name "warnings" :description "CLOCK lines left out of the totals because they could not be parsed; each has code, message, path (workspace-relative), line, and source. Always present, empty when none."))
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
(list :name "org.heading.rename" :summary "Change the text of one heading." :description "Change the text of one heading." :args (append '((:name "path" :kind "positional" :type "string" :required t :description "Org file inside the selected workspace root.")) *org-selector-time-args* '((:name "--title" :type "string" :required t :description "New heading text. If it contains a statistics cookie, it replaces the existing cookies; otherwise existing cookies are appended.")) *org-write-control-args*) :output-fields (append '((:name "changed" :description "True when the document changed.")) *org-write-output-fields*) :error-codes (append *org-common-write-error-codes* *org-selection-error-codes*))
(list :name "org.checkbox.toggle"
      :summary "Toggle one checkbox and update statistics cookies."
      :description "Changes [X] to [ ], and [ ] or [-] to [X]. For an item without child checkboxes, repeating the command toggles it again; use --dry-run to inspect the change. An item with child checkboxes is recalculated from its children and may not flip. Parent item checkboxes are updated from their direct children, then every statistics cookie owned by the selected heading is recalculated."
      :args (append '((:name "path" :kind "positional" :type "string" :required t
                       :description "Org file inside the selected workspace root."))
                    *org-selector-time-args*
                    '((:name "--item" :type "integer" :required t :min 1
                       :description "One-based checkbox number in document order, including nested items but excluding child headings. The numbering of checkboxes in inline tasks is undefined."))
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

(setf *org-command-schemas*
      (append *org-command-schemas*
              (list
               (list :name "org.capture"
                     :summary "Add a heading under a selected heading or date."
                     :description "For a headline target, select exactly one heading. For a datetree target, --at supplies the date. Body text must not contain Org headings."
                     :args (append '((:name "path" :kind "positional" :type "string" :required t
                                      :description "Org file inside the selected workspace root."))
                                   (append
                                    (quote ((:name "--match" :type "string"
                                             :description "For headline targets, an Org match expression selecting exactly one heading; give exactly one of --match and --heading-path. Omit for datetree targets.")
                                            (:name "--heading-path" :type "string"
                                             :description "For headline targets, the heading_path JSON array returned by org query; give exactly one of --match and --heading-path. Omit for datetree targets.")))
                                    *org-time-context-args*)
                                   '((:name "--target" :type "enum" :required t :choices ("headline" "datetree")
                                      :description "Target kind.")
                                     (:name "--title" :type "string" :required t :description "New heading title.")
                                     (:name "--body" :type "string" :description "Optional heading body."))
                                   *org-write-control-args*)
                     :output-fields (append '((:name "changed" :description "True when the file changed."))
                                            *org-write-output-fields*)
                     :error-codes (append *org-common-write-error-codes* *org-selection-error-codes*)))))

(setf *org-command-schemas*
      (append *org-command-schemas*
              (list
               (list :name "org.dynamic-block.update"
                     :summary "Update one numbered Org dynamic block."
                     :description "Select a block by its one-based document order. The clocktable :scope must be nil (the default), file, agenda, subtree, tree, or treeN with N of 1 or more; file-with-archives, agenda-with-archives, a file name, and a list of files are rejected as argument.invalid. Agenda globs are relative to the workspace root; only .org files are aggregated, and .git entries are skipped. The agenda files are read only when the block asks for the agenda scope, and the staged content applies under --tx. An agenda scope with no matching files is invalid. Clocktable :link headings are relative file links computed from the workspace-relative paths of the block document and each aggregated document. A CLOCK line that cannot be parsed is left out of the totals and reported in warnings."
                     :args (append '((:name "path" :kind "positional" :type "string" :required t
                                      :description "Org file inside the selected workspace root.")
                                     (:name "--block" :type "integer" :required t :min 1
                                      :description "One-based dynamic block number in document order.")
                                     (:name "--agenda-glob" :type "string, repeatable"
                                      :description "Workspace-relative glob for agenda files."))
                                   *org-time-context-args*
                                   *org-write-control-args*)
                     :output-fields (append '((:name "block" :description "One-based dynamic block number.")
                                              (:name "changed" :description "True when the block changed.")
                                              (:name "warnings" :description "CLOCK lines left out of the totals because they could not be parsed; each has code, message, path (workspace-relative), line, and source. Always present, empty when none."))
                                            *org-write-output-fields*)
                     :error-codes *org-common-write-error-codes*))))
