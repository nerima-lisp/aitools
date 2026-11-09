(in-package #:aitools.data)

(defparameter *md-command-schemas*
  '((:name "md.headings"
           :summary
           "List parsed Markdown headings with one-based section ordinals."
           :description
           "Reads the whole UTF-8 file up to 16 MiB. The parser uses GFM mode and selects ATX and setext headings. It excludes a front-matter block at the start of the file, delimited by `---`, and headings inside fenced code. Binary input returns `input.unsupported-format`; parser failures return `input.syntax-error`. The hash is the SHA-256 of the original file bytes."
           :args
           ((:name "path" :kind "positional" :type "string" :required t :description "Markdown file path."))
           :output-fields
           ((:name "path" :description "Path as given.")
            (:name "hash" :description "SHA-256 of the complete original file.")
            (:name "headings"
                   :description
                   "[{section,heading,level,line}] in document order; section is a one-based ordinal and line is one-based."))
           :error-codes
           ("argument.invalid" "input.not-found"
                               "input.not-utf8"
                               "input.unsupported-format"
                               "input.syntax-error"
                               "refusal.too-large"
                               "environment.io"
                               "internal.unexpected"))
    (:name "md.read"
           :summary
           "Read one complete Markdown section by its one-based heading ordinal."
           :description
           "Returns the selected heading and all content up to, but not including, the next heading at the same or a higher level. The whole UTF-8 file is parsed up to 16 MiB."
           :args
           ((:name "path" :kind "positional" :type "string" :required t :description "Markdown file path.")
            (:name "section"
                   :kind
                   "positional"
                   :type
                   "integer"
                   :required
                   t
                   :description
                   "One-based ordinal from md headings; duplicate titles have separate ordinals."))
           :output-fields
           ((:name "path" :description "Path as given.")
            (:name "hash" :description "SHA-256 of the complete original file.")
            (:name "section" :description "Selected one-based heading ordinal.")
            (:name "heading" :description "Selected heading text.")
            (:name "level" :description "Selected heading level.")
            (:name "line" :description "One-based source line.")
            (:name "content" :description "Complete selected section including its heading."))
           :error-codes
           ("argument.invalid" "input.not-found"
                               "input.not-utf8"
                               "input.unsupported-format"
                               "input.syntax-error"
                               "selection.no-match"
                               "refusal.too-large"
                               "environment.io"
                               "internal.unexpected"))
    (:name "md.replace"
           :summary
           "Replace one Markdown section from standard input."
           :description
           "Requires --stdin and --expect-hash. Writes a complete replacement section through the journal or stages it with --tx. Include the selected heading in standard input to retain it; omitting it removes that heading. It parses the whole UTF-8 file up to 16 MiB and preserves every original byte outside the selected section span."
           :args
           ((:name "path" :kind "positional" :type "string" :required t :description "Workspace file path.")
            (:name "section"
                   :kind
                   "positional"
                   :type
                   "integer"
                   :required
                   t
                   :description
                   "One-based ordinal from md headings.")
            (:name "--expect-hash" :type "string" :required t :description "Required SHA-256 of the current target.")
            (:name "--stdin" :type "flag" :required t :description "Read the replacement section from standard input.")
            (:name "--dry-run" :type "flag" :description "Validate the replacement and show a diff without writing.")
            (:name "--tx" :type "string" :description "Stage the write in this transaction."))
           :output-fields
           ((:name "changes"
                   :description
                   "Standard write changes with path, action, hashes, and a diff when available.")
            (:name "op_id" :description "Journal operation id for undo; null when nothing changed.")
            (:name "tx" :description "Transaction receiving the staged write.")
            (:name "tx_op" :description "Staged operation number in the transaction.")
            (:name "dry_run" :description "True when validated without applying the write.")
            (:name "next_commands" :description "Command to read a complete diff when changes.diff_truncated is true.")
            (:name "section" :description "Selected one-based heading ordinal.")
            (:name "heading" :description "Selected heading text before replacement.")
            (:name "level" :description "Selected heading level.")
            (:name "line" :description "One-based source line of the selected heading."))
           :error-codes
           ("argument.invalid" "input.not-found"
                               "input.not-utf8"
                               "input.unsupported-format"
                               "input.syntax-error"
                               "selection.no-match"
                               "refusal.not-a-file"
                               "refusal.outside-workspace"
                               "refusal.target-changed"
                               "refusal.redacted-input"
                               "refusal.too-large"
                               "environment.busy"
                               "environment.io"
                               "internal.unexpected"))))

(export '(*md-command-schemas*))
