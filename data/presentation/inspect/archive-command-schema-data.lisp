;;;; data/presentation/inspect/archive-command-schema-data.lisp
;;;;
;;;; Schema text for `archive list`, `archive read`, `snapshot create`,
;;;; and `snapshot diff`.
(in-package #:aitools.data)

(defparameter *inspect-archive-command-schemas*
  '((:name "archive.list"
     :summary "List the members of a zip, tar, tar.gz, or gz archive."
     :description "The format is detected from magic bytes (a gzip stream holding a tar is tar.gz). Decompression stops at 256 MiB (refusal.too-large)."
     :args ((:name "path" :kind "positional" :type "string" :required t :description "Archive file.")
            (:name "--limit" :type "integer" :default 200 :description "Maximum members returned."))
     :output-fields ((:name "format" :description "zip, tar, tar.gz, or gz.")
                     (:name "items" :description "[{path,kind,size,mode,mtime}]; kind is file, directory, symlink, hardlink, or other; mode is four octal digits or null; mtime ISO 8601 UTC.")
                     (:name "total" :description "Members in the archive.")
                     (:name "truncated" :description "True when --limit cut the list (status partial, exit 3)."))
     :error-codes ("input.not-found" "input.unsupported-format" "refusal.too-large" "refusal.not-a-file"
                   "environment.busy" "environment.io"))
    (:name "archive.read"
     :summary "Read one archive member in read's shape, with selectors and a line limit."
     :description "A gz archive has one member and takes no entry. Members larger than 64 MiB are refused. A binary member read as text returns {binary,size,mime}. A line longer than 16384 bytes is cut there and listed in cut_lines; the result is partial, and next_commands names the --as hex dump through the cut."
     :args ((:name "path" :kind "positional" :type "string" :required t :description "Archive file.")
            (:name "entry" :kind "positional" :type "string" :description "Member path as archive list shows it.")
            (:name "--as" :type "enum" :choices ("text" "hex") :default "text" :description "Output view; sets mode.")
            (:name "--max-lines" :type "integer" :default 80 :description "Maximum lines (text) or 16-byte rows (hex)."))
     :selectors t
     :output-fields ((:name "mode" :description "text or hex.")
                     (:name "entry" :description "The member read.")
                     (:name "start_line" :description "text: number of the first returned line.")
                     (:name "lines" :description "text: line texts without terminators or BOM.")
                     (:name "cut_lines" :description "text: numbers of the returned lines cut at 16384 bytes (status partial, exit 3).")
                     (:name "total_lines" :description "text: lines in the member.")
                     (:name "hash" :description "text: SHA-256 of the member bytes.")
                     (:name "rows" :description "hex: [{offset,hex}].")
                     (:name "truncated" :description "True when --max-lines cut the output or a line was cut (status partial, exit 3).")
                     (:name "approx_tokens" :description "ceil(characters of returned text / 4).")
                     (:name "next_commands" :description "The command reading the next part, when there is one."))
     :error-codes ("argument.invalid" "input.not-found" "input.unsupported-format" "input.unsupported-language"
                   "input.syntax-error" "selection.no-match" "selection.ambiguous" "refusal.too-large"
                   "refusal.not-a-file" "environment.busy" "environment.io"))
    (:name "snapshot.create"
     :summary "Record every workspace file's size, mtime, and content hash for a later snapshot diff."
     :description "Independent of the journal and of any tx; ignored files are left out unless --no-ignore. The scan options are stored and reused by snapshot diff."
     :no-tx t
     :args ((:name "--glob" :type "string" :multiple t :description "Only paths matching this glob (repeatable).")
            (:name "--lang" :type "string" :description "Only files of this language.")
            (:name "--no-ignore" :type "flag" :description "Include ignored files.")
            (:name "--skip-larger-than" :type "size" :default "10MiB" :description "Leave out larger files.")
            (:name "--newer" :type "string" :description "Only files modified after this path's mtime or within this duration."))
     :output-fields ((:name "snapshot_id" :description "Id for snapshot diff.")
                     (:name "files" :description "Files recorded.")
                     (:name "ignore_source" :description "gitignore, builtin, or none."))
     :error-codes ("argument.invalid" "input.not-found" "environment.io"))
    (:name "snapshot.diff"
     :summary "List files added, removed, or modified since a snapshot."
     :description "A file counts as modified only when its size or mtime changed and its content hash differs from the recorded one."
     :no-tx t
     :args ((:name "snapshot_id" :kind "positional" :type "string" :required t :description "Id from snapshot create.")
            (:name "--limit" :type "integer" :default 100 :description "Maximum paths per list."))
     :output-fields ((:name "added" :description "Paths not in the snapshot.")
                     (:name "removed" :description "Recorded paths now missing.")
                     (:name "modified" :description "Paths whose content changed.")
                     (:name "truncated" :description "True when --limit cut a list (status partial, exit 3)."))
     :error-codes ("input.not-found" "environment.io")))
  "The archive reads and the snapshot commands.")

(export '*inspect-archive-command-schemas*)
