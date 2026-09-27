;;;; data/application/edit/command-spec-data.lisp
;;;;
;;;; The edit context's commands (text edits, file operations, and the write
;;;; side of the format groups) as data: positionals, options, output fields and error codes. The
;;;; application layer reads it twice: to re-parse a recorded tx op's argv
;;;; for `tx rebase`, and to publish the option list the presentation
;;;; layer turns into cl-cli options and `schema` entries, so the two parsers
;;;; cannot drift apart.
;;;;
;;;; :INCLUDE names shared option groups the application expands (see
;;;; +OPTION-GROUPS+ in application/command-spec.lisp). Option :KIND is :FLAG,
;;;; :VALUE, :MULTI (repeatable value) or :PAIR (two values). Every value
;;;; stays a string until the flow parses it, so a replayed argv and a live
;;;; invocation go through the same validation.
(in-package #:aitools.data)

(defparameter *edit-command-specs*
  '((:name "edit"
     :summary "Replace text selected by --old or a selector; --new '' deletes it."
     :description "--old matches exactly first, then line-wise ignoring leading and trailing whitespace (re-indenting --new to the file). Line selectors replace whole lines including their newline. --stdin reads {\"old\",\"new\"} or {\"edits\":[{old|between|match..., new}]} applied in order, all or nothing; edits[] accept only content-based selectors."
     :positionals ((:key :path :name "path"))
     :options ((:key :old :name "old" :kind :value :description "Text to replace; exact, then whitespace-insensitive.")
               (:key :new :name "new" :kind :value :description "Replacement; '' deletes the selection."))
     :include (:selectors :stdin :hash :count :write)
     :output-fields ((:name "strategy" :description "exact or whitespace, for --old.")))
    (:name "insert"
     :summary "Insert content at the start or end of a file, or before/after the lines a selector picks."
     :description "--at end ends a last line lacking a newline before appending. --before/--after with --match inserts at every matching line and needs --expect-count."
     :positionals ((:key :path :name "path"))
     :options ((:key :at :name "at" :kind :value :description "start or end.")
               (:key :before :name "before" :kind :flag :description "Insert before the selection.")
               (:key :after :name "after" :kind :flag :description "Insert after the selection."))
     :include (:selectors :content :stdin :hash :count :write)
     :output-fields ((:name "inserted_at" :description "Line numbers (after the write) where each inserted block starts.")))
    (:name "replace"
     :summary "Regex (or --fixed) replace across files or within one file's selection, with $1/${name} replacement templates."
     :description "Replaces every non-overlapping match per line (whole file with --multiline). Templates: $0 $1 $name ${name} $$ and ${1:filter:...} with upper lower capitalize snake camel kebab trim inc dec padN. \\1-style references are refused unless --literal-replacement, which inserts the replacement verbatim. All files are validated before anything is written. --stdin reads {\"pattern\",\"replacement\"}; positionals are then all paths."
     :positionals ((:key :arguments :name "pattern replacement [path...]" :rest t))
     :options ((:key :fixed :name "fixed" :kind :flag :description "PATTERN is a literal string.")
               (:key :ignore-case :name "ignore-case" :kind :flag :description "Case-insensitive match.")
               (:key :word :name "word" :kind :flag :description "Match whole words only.")
               (:key :multiline :name "multiline" :kind :flag :description "Match across lines; . still excludes newlines unless (?s).")
               (:key :nth :name "nth" :kind :value :description "Replace only the Nth match of each file.")
               (:key :literal-replacement :name "literal-replacement" :kind :flag :description "Insert REPLACEMENT verbatim, without template expansion."))
     :include (:selectors :scan :stdin :hash :count :write)
     :output-fields ((:name "changes[].count" :description "Replacements made in that file.")))
    (:name "apply"
     :summary "Apply a unified diff from --stdin to the workspace, all files or none."
     :positionals ()
     :options ((:key :fuzz :name "fuzz" :kind :value :default "3" :description "Lines around a hunk's recorded position searched for its context.")
               (:key :reverse :name "reverse" :kind :flag :description "Apply the inverse patch.")
               (:key :strip :name "strip" :kind :value :description "Leading path components dropped; default drops a/ and b/."))
     :include (:stdin :hash :write)
     :output-fields ((:name "applied_hunks" :description "Hunks applied across all files.")))
    (:name "transform"
     :summary "Apply line operations (sort, unique, wrap, comment, ...) to a file or a selection."
     :description "--op repeats and applies in order. Line ops: sort sort-numeric sort-version reverse shuffle unique delete-blank squeeze-blank strip-trailing indent dedent tabs-to-spaces spaces-to-tabs upper lower nfc nfkc wrap reflow comment uncomment. Whole-file ops (no selector): eol-lf eol-crlf final-newline no-final-newline strip-bom. wrap and reflow leave Markdown code fences alone. --width defaults to 2 for indent/dedent (dedent without it removes the common indent) and 8 for tab conversion."
     :positionals ((:key :path :name "path"))
     :options ((:key :op :name "op" :kind :multi :description "Operation; repeat to chain.")
               (:key :width :name "width" :kind :value :description "Indent width or tab stop.")
               (:key :key :name "key" :kind :value :description "Sort/unique key field (1-based).")
               (:key :delimiter :name "delimiter" :kind :value :description "Key field separator regex (default whitespace).")
               (:key :columns :name "columns" :kind :value :default "80" :description "wrap and reflow width.")
               (:key :seed :name "seed" :kind :value :description "Required by shuffle."))
     :include (:selectors :hash :count :write)
     :output-fields ((:name "removed_lines" :description "Lines removed, for ops that remove lines.")))
    (:name "move-lines"
     :summary "Move selected lines to another place in the same file or into another file."
     :positionals ((:key :path :name "src"))
     :options ((:key :to :name "to" :kind :value :description "Destination file (default: the same file).")
               (:key :to-position :name "to-position" :kind :value :default "end"
                :description "start, end, after:N, before:N, after-symbol:NAME or before-symbol:NAME (line numbers of the destination before the move)."))
     :include (:selectors :hash :count :write))
    (:name "write"
     :summary "Create a file from --content, --content-file (repeatable, concatenated) or --stdin."
     :positionals ((:key :path :name "path"))
     :options ((:key :separator :name "separator" :kind :value :description "Inserted between concatenated inputs.")
               (:key :overwrite :name "overwrite" :kind :flag :description "Replace an existing file (needs --expect-hash)."))
     :include (:content-multi :stdin :hash :write))
    (:name "split"
     :summary "Split a file into numbered pieces by line count, before matching lines, or by bytes."
     :positionals ((:key :path :name "path"))
     :options ((:key :lines :name "lines" :kind :value :description "Lines per piece.")
               (:key :at-match :name "at-match" :kind :value :description "Start a piece at each line matching this regex.")
               (:key :bytes :name "bytes" :kind :value :description "Bytes per piece.")
               (:key :prefix :name "prefix" :kind :value :description "Piece path prefix (default <path>.).")
               (:key :suffix-digits :name "suffix-digits" :kind :value :default "3" :description "Digits of the piece number."))
     :include (:write)
     :output-fields ((:name "changes[].start_line" :description "First source line of the piece.")
                     (:name "changes[].lines" :description "Source lines the piece holds.")))
    (:name "transcode"
     :summary "Convert a file between utf-8, shift_jis (CP932), euc-jp, iso-8859-1, utf-16le and utf-16be."
     :positionals ((:key :path :name "path"))
     :options ((:key :from :name "from" :kind :value :description "Source encoding (default: guessed).")
               (:key :to :name "to" :kind :value :default "utf-8" :description "Target encoding.")
               (:key :replace-unmappable :name "replace-unmappable" :kind :flag :description "Write ? for characters the target cannot represent."))
     :include (:hash :write)
     :output-fields ((:name "from" :description "Source encoding.") (:name "to" :description "Target encoding.")
                     (:name "replaced" :description "Characters written as ?.")))
    (:name "move"
     :summary "Move or rename a file or directory; never merges into an existing directory."
     :positionals ((:key :source :name "src") (:key :destination :name "dst"))
     :options ((:key :overwrite :name "overwrite" :kind :flag :description "Replace an existing destination file (needs --expect-hash dst=hash)."))
     :include (:hash :write))
    (:name "copy"
     :summary "Copy a file, or a directory with --recursive, byte for byte."
     :description "--recursive copies everything, ignore rules included; symlinks leading outside the workspace are skipped and listed."
     :positionals ((:key :source :name "src") (:key :destination :name "dst"))
     :options ((:key :overwrite :name "overwrite" :kind :flag :description "Replace an existing destination file (needs --expect-hash dst=hash).")
               (:key :recursive :name "recursive" :kind :flag :description "Copy a directory tree.")
               (:key :max-bytes :name "max-bytes" :kind :value :default "1GiB" :description "Largest total size copied."))
     :include (:hash :write)
     :output-fields ((:name "files" :description "Files copied (directories).")
                     (:name "skipped" :description "[{path,reason}] entries not copied.")))
    (:name "delete"
     :summary "Delete a file, a symlink, or an empty directory."
     :positionals ((:key :path :name "path"))
     :options ()
     :include (:hash :write))
    (:name "mkdir"
     :summary "Create a directory and its parents; an existing directory is a no-op."
     :positionals ((:key :path :name "path"))
     :options ()
     :include (:write))
    (:name "chmod"
     :summary "Set or clear the executable bits, or set an octal mode."
     :positionals ((:key :path :name "path"))
     :options ((:key :exec :name "exec" :kind :flag :description "Add execute permission.")
               (:key :no-exec :name "no-exec" :kind :flag :description "Remove execute permission.")
               (:key :mode :name "mode" :kind :value :description "Octal mode, such as 644."))
     :include (:write)
     :output-fields ((:name "previous_mode" :description "The octal mode before the change.")))
    (:name "link"
     :summary "Create a symlink LINK pointing at TARGET inside the workspace."
     :positionals ((:key :target :name "target") (:key :path :name "link"))
     :options ((:key :overwrite :name "overwrite" :kind :flag :description "Replace an existing symlink (not a file)."))
     :include (:hash :write))
    (:name "touch"
     :summary "Create an empty file, or set an existing file's modification time."
     :description "Journaled like every write: undo deletes a file touch created, or sets an existing file's previous modification time back (refusal.target-changed when its content, mode or mtime changed since). With --tx the time is staged and applied at tx commit."
     :positionals ((:key :path :name "path"))
     :options ((:key :mtime :name "mtime" :kind :value :description "Unix seconds, @seconds, or ISO 8601 (default now)."))
     :include (:write)
     :output-fields ((:name "mtime" :description "The modification time set (ISO 8601 UTC).")))
    (:name "mktemp"
     :summary "Create a temporary file or directory in the workspace's state tmp/ area (not journaled)."
     :positionals ()
     :options ((:key :dir :name "dir" :kind :flag :description "Create a directory.")
               (:key :suffix :name "suffix" :kind :value :description "Name suffix, such as .json."))
     :include ()
     :output-fields ((:name "path" :description "Absolute real path of the new entry; writes below it are allowed.")
                     (:name "hash" :description "For a file (not --dir): the content hash info reports for it, ready for write --overwrite --expect-hash.")))
    (:name "json.set"
     :summary "Set the value at a JSON Pointer (/- appends to an array)."
     :positionals ((:key :path :name "path") (:key :pointer :name "pointer") (:key :value :name "value" :optional t))
     :options ()
     :include (:stdin :hash :write))
    (:name "json.delete"
     :summary "Delete the value at a JSON Pointer."
     :positionals ((:key :path :name "path") (:key :pointer :name "pointer"))
     :options ()
     :include (:hash :write))
    (:name "json.merge"
     :summary "Apply an RFC 7386 JSON Merge Patch read from --stdin."
     :positionals ((:key :path :name "path"))
     :options ()
     :include (:stdin :hash :write))
    (:name "json.patch"
     :summary "Apply an RFC 6902 JSON Patch read from --stdin; all operations or none."
     :positionals ((:key :path :name "path"))
     :options ()
     :include (:stdin :hash :write))
    (:name "json.fmt"
     :summary "Re-indent a JSON file, keeping key order unless --sort-keys."
     :positionals ((:key :path :name "path"))
     :options ((:key :indent :name "indent" :kind :value :description "Indent width (default: the file's own, else 2).")
               (:key :minify :name "minify" :kind :flag :description "Write on one line.")
               (:key :sort-keys :name "sort-keys" :kind :flag :description "Sort object keys."))
     :include (:hash :write))
    (:name "table.set"
     :summary "Set one cell of a CSV or TSV file by data row and column name."
     :positionals ((:key :path :name "path"))
     :options ((:key :row :name "row" :kind :value :description "Data row, 1-based after the header.")
               (:key :column :name "column" :kind :value :description "Header name (or 1-based index).")
               (:key :value :name "value" :kind :value :description "New cell text."))
     :include (:stdin :hash :write)
     :output-fields ((:name "previous" :description "The cell's previous text.")))
    (:name "archive.extract"
     :summary "Extract a zip, tar, tar.gz or gz archive after validating every entry."
     :positionals ((:key :path :name "path"))
     :options ((:key :to :name "to" :kind :value :description "Destination directory (required).")
               (:key :entry :name "entry" :kind :multi :description "Entry to extract; repeat (default: all).")
               (:key :max-bytes :name "max-bytes" :kind :value :default "1GiB" :description "Largest total extracted size.")
               (:key :max-entries :name "max-entries" :kind :value :default "100000" :description "Most entries extracted."))
     :include (:write)
     :output-fields ((:name "total" :description "Changes in the operation; changes lists the first 200.")))
    (:name "archive.create"
     :summary "Create a zip, tar, tar.gz or gz archive from workspace files (ignored files excluded)."
     :positionals ((:key :path :name "path") (:key :sources :name "src" :rest t))
     :options ((:key :format :name "format" :kind :value :description "zip, tar, tar.gz or gz (default: from the extension)."))
     :include (:scan :write)
     :output-fields ((:name "entries" :description "Entries written."))))
  "One plist per edit command; :NAME is the dispatch name.")

(defparameter *edit-option-groups*
  `((:write
     (:key :dry-run :name "dry-run" :kind :flag :description "Validate and show the diff; write nothing.")
     (:key :tx :name "tx" :kind :value :description "Stage the write in this tx instead of the working tree."))
    (:hash
     (:key :expect-hash :name "expect-hash" :kind :multi
      :description "Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text."))
    (:count
     (:key :expect-count :name "expect-count" :kind :value
      :description "Guard: the number of selected lines or replacements."))
    (:selectors
     ,@(mapcar (lambda (option)
                 (list :key (getf option :key) :name (getf option :name)
                       :kind (getf option :kind) :description (getf option :description)))
               *selector-options*))
    (:content
     (:key :content :name "content" :kind :value :description "Content text.")
     (:key :content-file :name "content-file" :kind :value :description "Content read as bytes from this file."))
    (:content-multi
     (:key :content :name "content" :kind :multi :description "Content text; repeat to concatenate.")
     (:key :content-file :name "content-file" :kind :multi :description "Content bytes from this file; repeat to concatenate."))
    (:stdin
     (:key :stdin :name "stdin" :kind :flag :description "Read the input from standard input (never read otherwise).")
     (:key :stdin-data :name "stdin-data" :kind :value
      :description "The --stdin input given inline; history records --stdin writes this way."))
    (:scan
     (:key :glob :name "glob" :kind :multi :description "Only paths matching this glob; repeatable.")
     (:key :lang :name "lang" :kind :value :description "Only files of this language.")
     (:key :no-ignore :name "no-ignore" :kind :flag :description "Include ignored files.")
     (:key :skip-larger-than :name "skip-larger-than" :kind :value :default "10MiB" :description "Skip larger files.")
     (:key :newer :name "newer" :kind :value :description "Only files newer than PATH or a duration ago.")))
  "The shared option groups a command spec's :INCLUDE names; the application
expands each into the command's option list. The :SELECTORS group is spliced
from *SELECTOR-OPTIONS* so the five selectors stay defined once.")

(export '(*edit-command-specs* *edit-option-groups*))
