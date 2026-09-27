;;;; data/presentation/inspect/command-schema-data.lisp
;;;;
;;;; Schema text for `read`, `info`, `check`, and `diff`
;;;; (docs/src/reference/commands.md). AITOOLS.INSPECT.PRESENTATION builds each
;;;; COMMAND-SCHEMA from these entries; the cl-cli options live beside the
;;;; handlers. The `json`, `table`, `archive`, and `snapshot` entries are in
;;;; the sibling data files of this directory.
(in-package #:aitools.data)

(defparameter *inspect-common-args*
  '((:name "--tx" :type "string"
     :description "Read the tx's state. Single-file reads also record the file's disk state in the tx read set."))
  "Arguments every inspect command accepts, appended to each entry's :ARGS.")

(defparameter *inspect-selector-args*
  '((:name "--range" :type "string" :description "Selector: lines S:E, S: (to the end), or N (1-based, inclusive).")
    (:name "--symbol" :type "string" :description "Selector: a definition's lines, found with the `code outline` language table.")
    (:name "--kind" :type "string" :description "With --symbol: only definitions of this kind (function, macro, ...).")
    (:name "--between" :type "string" :value-count 2 :description "Selector: START-RE END-RE; from a line matching START to the first later line matching END.")
    (:name "--exclusive" :type "flag" :description "With --between: leave out the two boundary lines.")
    (:name "--match" :type "string" :description "Selector: every line matching the regular expression (cl-regex-kit, per line).")
    (:name "--invert" :type "flag" :description "With --match: the lines that do not match."))
  "The selectors other than --old; one per call.")

(defparameter *inspect-command-schemas*
  '((:name "read"
     :summary "Read a file's lines with line numbers, a selector, and a line limit."
     :description "One of: a selector, --tail, --as hex, --as strings. --max-lines always applies; when the output stops before the end of the file, next_commands holds the next --range. A binary file read as text returns {binary,size,mime} instead of lines. With --match, line_numbers lists each returned line's number. encoding_errors counts U+FFFD substitutions in the returned lines. approx_tokens is ceil(characters of the returned text / 4). A line longer than 16384 bytes (16384 characters under a non-UTF-8 --encoding) is cut there and listed in cut_lines; the result is partial, and next_commands names the --as hex --bytes dump of the bytes after the cut. A --as strings run longer than 16384 characters is cut the same way and marked cut:true."
     :args ((:name "path" :kind "positional" :type "string" :required t :description "File to read (relative to the working directory).")
            (:name "--tail" :type "integer" :description "The last N lines.")
            (:name "--as" :type "enum" :choices ("text" "hex" "strings") :default "text" :description "Output view; sets mode.")
            (:name "--bytes" :type "string" :default "0:256" :description "With --as hex: byte span S:E or S: (0-based, E exclusive).")
            (:name "--min-length" :type "integer" :default 4 :description "With --as strings: shortest run reported.")
            (:name "--escape-invisible" :type "flag" :description "Show control, zero-width, NBSP, ideographic-space, and line-end CR characters as \\u{XXXX}.")
            (:name "--encoding" :type "enum" :choices ("utf-8" "shift_jis" "euc-jp" "iso-8859-1" "utf-16le" "utf-16be") :description "Decode from this encoding instead of UTF-8.")
            (:name "--max-lines" :type "integer" :default 80 :description "Maximum lines (text), rows of 16 bytes (hex), or strings returned."))
     :selectors t
     :output-fields ((:name "mode" :description "text, hex, or strings (the --as value).")
                     (:name "start_line" :description "text: number of the first returned line.")
                     (:name "lines" :description "text: line texts without terminators or BOM.")
                     (:name "line_numbers" :description "text with --match: the number of each returned line.")
                     (:name "cut_lines" :description "text: numbers of the returned lines cut at 16384 bytes (status partial, exit 3).")
                     (:name "total_lines" :description "text: lines in the file.")
                     (:name "hash" :description "text: SHA-256 of the file bytes (the change-detection hash).")
                     (:name "truncated" :description "True when --max-lines cut the output or a line or string was cut (status partial, exit 3).")
                     (:name "encoding_errors" :description "text: invalid UTF-8 sequences replaced in the returned lines.")
                     (:name "binary" :description "text on a binary file: true, with size and mime instead of lines.")
                     (:name "rows" :description "hex: [{offset,hex}] with 16 bytes per row.")
                     (:name "strings" :description "strings: [{offset,text}] printable runs; cut:true on a run cut at 16384 characters.")
                     (:name "approx_tokens" :description "ceil(characters of returned text / 4).")
                     (:name "next_commands" :description "The command reading the next part, when there is one."))
     :error-codes ("argument.invalid" "input.not-found" "input.unsupported-format" "input.unsupported-language"
                   "input.syntax-error" "selection.no-match" "selection.ambiguous" "refusal.not-a-file"
                   "environment.busy" "environment.io"))
    (:name "info"
     :summary "Describe a path: location, kind, size, lines, words, encoding, mode, hash, and digests."
     :description "Path fields always; content fields for an existing file. --allow-missing turns a missing path into exists:false with the path fields only. digest uses a standard algorithm (compare with sha256sum and friends); hash is aitools' change-detection hash."
     :args ((:name "path" :kind "positional" :type "string" :required t :description "Path to describe.")
            (:name "--digest" :type "enum" :choices ("sha256" "sha1" "md5") :description "Add {algorithm,value} for this digest of the file bytes.")
            (:name "--allow-missing" :type "flag" :description "Succeed with exists:false when the path does not exist."))
     :output-fields ((:name "absolute" :description "Lexical absolute path.")
                     (:name "real" :description "Path with every symlink resolved.")
                     (:name "relative" :description "Path relative to the workspace root, or null outside it.")
                     (:name "exists" :description "Whether the path exists (a dangling symlink does not).")
                     (:name "kind" :description "file, directory, symlink (dangling), other, or null.")
                     (:name "inside_workspace" :description "Whether the real path is below the real workspace root.")
                     (:name "ignored" :description "Whether the workspace ignore rules exclude the path.")
                     (:name "size" :description "Bytes.")
                     (:name "lines" :description "Line count (a final line without a newline counts); null for binary files.")
                     (:name "words" :description "Whitespace-separated words, as wc -w; null for binary files.")
                     (:name "max_line_chars" :description "Characters in the longest line; null for binary files.")
                     (:name "binary" :description "NUL in the first 8 KiB.")
                     (:name "mime" :description "MIME type from magic bytes, then the extension.")
                     (:name "utf8_valid" :description "Whether the bytes are valid UTF-8.")
                     (:name "encoding_guess" :description "utf-8, shift_jis, euc-jp, utf-16le, utf-16be, or unknown.")
                     (:name "line_ending" :description "lf, crlf, mixed, or none; null for binary files.")
                     (:name "trailing_newline" :description "Whether the file ends with a line feed.")
                     (:name "bom" :description "Whether the file starts with a UTF-8 BOM.")
                     (:name "mode" :description "Permission bits as four octal digits.")
                     (:name "mtime" :description "Modification time, ISO 8601 UTC; with --tx, the staged time of a file the tx touched, and null for a file the tx otherwise changed.")
                     (:name "hash" :description "The change-detection hash, the value --expect-hash compares: SHA-256 of the bytes of the regular file the path leads to (symlinks followed); for a symlink that leads to no regular file (dangling, or to a directory), SHA-256 of its target text, reported with --allow-missing for a dangling one; absent for a directory.")
                     (:name "approx_tokens" :description "ceil(characters / 4); null for binary files.")
                     (:name "digest" :description "{algorithm,value} with --digest."))
     :error-codes ("argument.invalid" "input.not-found" "environment.busy" "environment.io"))
    (:name "check"
     :summary "Check a JSON file's syntax or a Lisp file's delimiter balance."
     :description "Lisp balance skips strings, comments, and character literals of the file's dialect (Common Lisp, Emacs Lisp, Scheme, Clojure)."
     :args ((:name "path" :kind "positional" :type "string" :required t :description "File to check.")
            (:name "--format" :type "enum" :choices ("json" "lisp") :description "Format; default from the extension."))
     :output-fields ((:name "valid" :description "Always true on success.")
                     (:name "format" :description "json or lisp."))
     :error-codes ("input.syntax-error" "input.unsupported-format" "input.not-found" "refusal.not-a-file"
                   "environment.busy" "environment.io"))
    (:name "diff"
     :summary "Compare two files or directories, or show an op's full diff."
     :description "Files: unified (identical, diff), stat (added, deleted), or set (only_a, only_b, both_count: comm over distinct lines). Directories: added, removed, modified, identical_count (recursive, .git skipped). --op <op_id>: every change of that journal op with its full diff."
     :args ((:name "a" :kind "positional" :type "string" :description "Old file or directory.")
            (:name "b" :kind "positional" :type "string" :description "New file or directory.")
            (:name "--op" :type "string" :description "Journal op to show instead of two paths.")
            (:name "--context" :type "integer" :default 3 :description "Unchanged lines around each hunk.")
            (:name "--output" :type "enum" :choices ("unified" "stat" "set") :default "unified" :description "File comparison shape; sets mode.")
            (:name "--ignore-whitespace" :type "flag" :description "Compare lines with all whitespace removed (diff -w).")
            (:name "--ignore-eol" :type "flag" :description "Ignore CR before LF and a missing final newline.")
            (:name "--limit" :type "integer" :default 100 :description "Maximum hunks, set lines, directory entries, or op changes returned."))
     :output-fields ((:name "mode" :description "unified, stat, set, directory, or op.")
                     (:name "identical" :description "Files: no difference under the chosen rules (cmp without ignore flags).")
                     (:name "diff" :description "unified: the diff text with ---/+++ headers.")
                     (:name "added" :description "stat: inserted lines; directory: paths only in b.")
                     (:name "deleted" :description "stat: deleted lines.")
                     (:name "only_a" :description "set: distinct lines only in a.")
                     (:name "only_b" :description "set: distinct lines only in b.")
                     (:name "both_count" :description "set: distinct lines in both.")
                     (:name "removed" :description "directory: paths only in a.")
                     (:name "modified" :description "directory: paths in both with different content.")
                     (:name "identical_count" :description "directory: paths in both with equal content.")
                     (:name "changes" :description "op: [{path,action,from?,diff?}] with the full diff of each text change.")
                     (:name "truncated" :description "True when --limit cut a list (status partial, exit 3)."))
     :error-codes ("argument.invalid" "input.not-found" "environment.io")))
  "The schemas of `read`, `info`, `check`, and `diff`.")

(export '(*inspect-common-args* *inspect-selector-args* *inspect-command-schemas*))
