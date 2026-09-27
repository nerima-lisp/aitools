;;;; data/presentation/vcs/command-schema-data.lisp
;;;;
;;;; Schema text for the `git` group, one entry per
;;;; command. AITOOLS.VCS.PRESENTATION builds each COMMAND-SCHEMA from this
;;;; table; the cl-cli option definitions live beside the handlers.
(in-package #:aitools.data)

(defparameter *vcs-command-schemas*
  '((:name "git.status"
     :summary "Show the branch, upstream distance, and staged, unstaged, and untracked paths."
     :args ()
     :output-fields ((:name "branch" :description "Current branch, or \"(detached)\".")
                     (:name "upstream" :description "Tracked upstream branch, or null.")
                     (:name "ahead" :description "Commits ahead of upstream, or null without one.")
                     (:name "behind" :description "Commits behind upstream, or null without one.")
                     (:name "staged" :description "[{path,status,from?}] index changes; status is git's one-letter code.")
                     (:name "unstaged" :description "[{path,status}] work-tree changes; unmerged paths have status U.")
                     (:name "untracked" :description "Untracked paths."))
     :error-codes ("environment.unavailable" "environment.io"))
    (:name "git.log"
     :summary "List the newest commits, optionally only those touching a path."
     :args ((:name "path" :kind "positional" :type "string" :description "Limit to commits touching this path.")
            (:name "--limit" :type "integer" :default 20 :description "Maximum number of commits."))
     :output-fields ((:name "items" :description "[{sha,author,date,subject}], newest first; date is ISO 8601 in the author's offset.")
                     (:name "total" :description "Number of matching commits.")
                     (:name "truncated" :description "True when total exceeds --limit (status partial, exit 3)."))
     :error-codes ("environment.unavailable" "input.not-found" "environment.io"))
    (:name "git.diff"
     :summary "Show changed files with line counts and, in hunks mode, their hunks."
     :args ((:name "path" :kind "positional" :type "string" :description "Limit to this path.")
            (:name "--staged" :type "flag" :description "Compare the index with HEAD instead of the work tree with the index.")
            (:name "--ref" :type "string" :description "Revision or range to compare, such as HEAD~1 or A..B.")
            (:name "--output" :type "enum" :choices ("hunks" "stat") :default "hunks" :description "hunks adds each file's hunks; stat gives counts only.")
            (:name "--max-lines" :type "integer" :default 400 :description "Hunk lines returned before later files become mode summary."))
     :output-fields ((:name "mode" :description "hunks or stat.")
                     (:name "files" :description "[{path,from?,mode,added,deleted,binary?,hunks?}]; file mode is hunks, stat, or summary (over --max-lines).")
                     (:name "truncated" :description "True when a file was summarized (status partial, exit 3).")
                     (:name "approx_tokens" :description "ceil(characters of returned hunk lines / 4)."))
     :error-codes ("argument.invalid" "environment.unavailable" "input.not-found" "environment.io"))
    (:name "git.blame"
     :summary "Show who last changed each line of a work-tree file."
     :args ((:name "path" :kind "positional" :type "string" :required t :description "File to blame.")
            (:name "--range" :type "string" :description "Selector: lines S:E, S:, or N. Without a selector, the first 80 lines.")
            (:name "--symbol" :type "string" :description "Selector: the definition NAME (see code outline); --kind narrows it.")
            (:name "--kind" :type "string" :description "With --symbol: narrow to one definition kind.")
            (:name "--between" :type "string" :value-count 2 :description "Selector: from a line matching RE1 to the next matching RE2; --exclusive drops both.")
            (:name "--exclusive" :type "flag" :description "With --between: leave out the two boundary lines.")
            (:name "--match" :type "string" :description "Selector: every line matching RE; --invert selects the rest.")
            (:name "--invert" :type "flag" :description "With --match: the lines that do not match."))
     :output-fields ((:name "start_line" :description "First returned line number.")
                     (:name "lines" :description "[{n,sha,author,date,text}].")
                     (:name "total_lines" :description "Lines in the file.")
                     (:name "truncated" :description "True when the default 80-line window ended before the file did."))
     :error-codes ("argument.invalid" "selection.no-match" "selection.ambiguous" "input.syntax-error" "input.unsupported-language" "environment.unavailable" "input.not-found" "environment.io"))
    (:name "git.show"
     :summary "Read a file as stored at a revision, in read's shape."
     :args ((:name "object" :kind "positional" :type "string" :required t :description "<rev>:<path>, such as HEAD:src/a.lisp.")
            (:name "--range" :type "string" :description "Selector: lines S:E, S:, or N.")
            (:name "--symbol" :type "string" :description "Selector: the definition NAME (see code outline); --kind narrows it.")
            (:name "--kind" :type "string" :description "With --symbol: narrow to one definition kind.")
            (:name "--between" :type "string" :value-count 2 :description "Selector: from a line matching RE1 to the next matching RE2; --exclusive drops both.")
            (:name "--exclusive" :type "flag" :description "With --between: leave out the two boundary lines.")
            (:name "--match" :type "string" :description "Selector: every line matching RE; --invert selects the rest; lines then come with line_numbers.")
            (:name "--invert" :type "flag" :description "With --match: the lines that do not match.")
            (:name "--max-lines" :type "integer" :default 80 :description "Maximum lines returned."))
     :output-fields ((:name "rev" :description "Revision part of the object name.")
                     (:name "path" :description "Path part of the object name.")
                     (:name "start_line" :description "First returned line number.")
                     (:name "lines" :description "Line texts without terminators or BOM.")
                     (:name "line_numbers" :description "With --match, the number of each returned line.")
                     (:name "encoding_errors" :description "Malformed UTF-8 sequences replaced by U+FFFD.")
                     (:name "total_lines" :description "Lines in the blob.")
                     (:name "hash" :description "SHA-256 of the blob bytes.")
                     (:name "truncated" :description "True when --max-lines stopped before the requested end.")
                     (:name "approx_tokens" :description "ceil(characters of returned lines / 4).")
                     (:name "binary" :description "Present and true for a blob with a NUL in its first 8 KiB; lines are then omitted and size given."))
     :error-codes ("argument.invalid" "selection.no-match" "selection.ambiguous" "input.syntax-error" "input.unsupported-language" "environment.unavailable" "input.not-found" "environment.io")))
  "One plist per `git` subcommand: :NAME, :SUMMARY, :ARGS, :OUTPUT-FIELDS,
:ERROR-CODES, in COMMAND-SCHEMA terms.")

(export '(*vcs-command-schemas*))
