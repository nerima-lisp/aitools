;;;; data/presentation/search/command-schema-data.lisp
;;;;
;;;; Schema text for the search context's commands,
;;;; one entry per command. AITOOLS.SEARCH.PRESENTATION builds each
;;;; COMMAND-SCHEMA from this table; the cl-cli option definitions live beside
;;;; the handlers.
(in-package #:aitools.data)

(defparameter *search-scan-args*
  '((:name "--glob" :type "string" :repeatable t :description "Only paths matching this glob (wildmatch; `!` excludes; no `/` matches the base name).")
    (:name "--lang" :type "string" :description "Only files of this language (the `code` language table).")
    (:name "--no-ignore" :type "flag" :description "Do not apply .gitignore or the builtin excludes. `.git` and `.aitools-*.tmp` are always skipped.")
    (:name "--skip-larger-than" :type "size" :description "Skip files above this size (<n>, <n>KiB, <n>MiB, <n>GiB).")
    (:name "--newer" :type "string" :description "Only entries modified after this path's mtime, or within this duration (<n>ms|s|m|h|d) of now.")
    (:name "--tx" :type "string" :description "Read through this tx: staged files, deletions, and the tx's .gitignore apply."))
  "The workspace scan options, shared by `search` and `find`.")

(defparameter *search-regex-description*
  "Patterns use cl-regex-kit syntax and match the bytes of each file, with `.`, `\\w`, and `[^...]` matching one UTF-8 character. Most syntax runs in linear time on the Pike VM; backreferences, lookaround, atomic groups, possessive repetition, conditionals, subroutine calls, `\\K`, `\\X`, and the `\\G`/`\\Z`/`\\b{...}` anchors run on the bounded advanced executor, and a file on which it exhausts its step budget is listed in `skipped` with reason `regex-limit`. `--ignore-case` and `\\w` follow cl-regex-kit's Unicode defaults (simple case folding; Unicode word characters). `^` and `$` match at line boundaries (before CR LF too). Without `--multiline` no match crosses a line end; with it, matches may span lines and `.` still excludes a newline unless the pattern sets `(?s)`. A UTF-8 BOM is never part of the first line.")

(defparameter *search-command-schemas*
  `((:name "search"
     :summary "Search file contents for a regular expression; results are grouped into blocks with context."
     :description ,*search-regex-description*
     :args ((:name "pattern" :kind "positional" :type "string" :description "The pattern. When --pattern or --stdin supplies the pattern, this positional is the first path instead.")
            (:name "path" :kind "positional" :type "string" :repeatable t :description "Files or directories to search; default the working directory (or the root when it is outside the workspace).")
            (:name "--pattern" :type "string" :repeatable t :description "A pattern; repeat for lines matching any of them (`matches` items then carry pattern_index, 0-based).")
            (:name "--stdin" :type "flag" :description "Read the pattern as raw UTF-8 text from standard input; one trailing newline is dropped.")
            (:name "--fixed" :type "flag" :description "Treat the pattern as literal text.")
            (:name "--ignore-case" :type "flag" :description "Case-insensitive matching.")
            (:name "--word" :type "flag" :description "Match only at word boundaries (the pattern is wrapped in \\b(?:...)\\b).")
            (:name "--line-regexp" :type "flag" :description "Match only whole lines.")
            (:name "--invert" :type "flag" :description "Select the lines that do not match. Not with --output matches.")
            (:name "--multiline" :type "flag" :description "Let matches span lines; each file is matched as one buffer.")
            (:name "--context" :type "integer" :default 2 :description "Context lines before and after each selected line.")
            (:name "--before" :type "integer" :description "Context lines before; overrides --context.")
            (:name "--after" :type "integer" :description "Context lines after; overrides --context.")
            (:name "--output" :type "enum" :choices ("blocks" "matches" "count" "files" "files-without-match") :default "blocks" :description "The result shape; `mode` repeats it.")
            (:name "--limit" :type "integer" :default 15 :description "Selected lines (blocks), matches (matches), or entries (count, files, files-without-match) returned."))
     :output-fields ((:name "mode" :description "The --output value.")
                     (:name "blocks" :description "blocks: [{path,start_line,lines[],match_lines[]}]; blocks whose context touches or overlaps are merged.")
                     (:name "matches" :description "matches: [{path,line,col,text,groups,named?,pattern_index?}]; col counts characters from 1; groups lists captures in order, null for a group that did not participate; named maps capture names to text or null.")
                     (:name "counts" :description "count: [{path,count}] selected lines per file, files with none omitted.")
                     (:name "paths" :description "files / files-without-match: paths of text files with at least one / no selected line.")
                     (:name "total_matches" :description "Exact number of selected lines (matches mode: matches) in every scanned file, also past --limit.")
                     (:name "total" :description "count, files, files-without-match: number of entries before --limit.")
                     (:name "files_scanned" :description "Text files searched.")
                     (:name "skipped" :description "[{path,reason}]; reason is binary, too-large, unreadable, or regex-limit.")
                     (:name "ignore_source" :description "gitignore, builtin, or none (--no-ignore).")
                     (:name "truncated" :description "True when --limit cut the result (status partial, exit 3; next_commands repeats the search with a sufficient --limit).")
                     (:name "approx_tokens" :description "ceil(characters of returned text / 4)."))
     :error-codes ("argument.invalid" "input.syntax-error" "input.not-found" "environment.io" "internal.unexpected"))
    (:name "find"
     :summary "List files and directories by name or path pattern; ls, find, tree, and du in one."
     :args ((:name "pattern" :kind "positional" :type "string" :description "A glob (when it holds *, ?, or [) or a substring. Without `/` it matches the last path component; with `/`, the workspace-relative path. Omit to list everything.")
            (:name "path" :kind "positional" :type "string" :description "Where to start; default the working directory (or the root when it is outside the workspace).")
            (:name "--type" :type "enum" :choices ("file" "dir" "symlink") :description "Only entries of this kind.")
            (:name "--depth" :type "integer" :description "Only entries at most this many levels below the start (1: its direct entries, like ls -la).")
            (:name "--sort" :type "enum" :choices ("path" "mtime" "size") :default "path" :description "path order; mtime newest first; size largest first. Flat output only.")
            (:name "--min-size" :type "size" :description "Only entries at least this large (directories only with --sizes).")
            (:name "--max-size" :type "size" :description "Only entries at most this large (directories only with --sizes).")
            (:name "--empty" :type "flag" :description "Only empty files and directories with no listed entries.")
            (:name "--executable" :type "flag" :description "Only files with an execute bit.")
            (:name "--output" :type "enum" :choices ("flat" "tree") :default "flat" :description "flat items, or a nested tree.")
            (:name "--sizes" :type "flag" :description "Give directories the total size of the files listed below them (du).")
            (:name "--limit" :type "integer" :default 50 :description "Entries returned."))
     :output-fields ((:name "mode" :description "flat or tree.")
                     (:name "items" :description "flat: [{path,kind,size,mode,mtime}]; kind is file, dir, symlink, or other; size is null for a directory without --sizes; mode is octal text; mtime is UTC ISO 8601.")
                     (:name "tree" :description "tree: {name,kind,size?,children[],omitted?}; omitted counts matching entries past --limit below that node.")
                     (:name "total" :description "Matching entries before --limit.")
                     (:name "ignore_source" :description "gitignore, builtin, or none (--no-ignore).")
                     (:name "truncated" :description "True when --limit cut the result (status partial, exit 3)."))
     :error-codes ("argument.invalid" "input.not-found" "internal.unexpected"))
    (:name "code.outline"
     :summary "List the definitions in one source file with their line ranges."
     :description "Definitions come from the language table shared with --symbol: Common Lisp, Emacs Lisp, Scheme, Clojure, Rust, Go, Python, JavaScript, TypeScript, Nix, shell, and Markdown headings. end_line is estimated by balanced parentheses, balanced braces, indentation, or the next heading of the same or a higher level."
     :args ((:name "path" :kind "positional" :type "string" :required t :description "The file.")
            (:name "--limit" :type "integer" :default 200 :description "Symbols returned.")
            (:name "--tx" :type "string" :description "Read the file as staged in this tx."))
     :output-fields ((:name "path" :description "The file, workspace-relative when inside the workspace.")
                     (:name "lang" :description "The language.")
                     (:name "symbols" :description "[{line,end_line,kind,name}] in line order.")
                     (:name "total" :description "Definitions before --limit.")
                     (:name "truncated" :description "True when --limit cut the list (status partial, exit 3)."))
     :error-codes ("input.unsupported-language" "input.unsupported-format" "input.not-found" "internal.unexpected"))
    (:name "code.defs"
     :summary "Find where a name is defined across the workspace's source files."
     :args ((:name "name" :kind "positional" :type "string" :required t :description "The definition name.")
            (:name "path" :kind "positional" :type "string" :description "Where to look; default the working directory.")
            (:name "--prefix" :type "flag" :description "Match names starting with NAME.")
            (:name "--kind" :type "string" :description "Only this kind (function, macro, class, ...; see code outline).")
            (:name "--limit" :type "integer" :default 50 :description "Definitions returned.")
            (:name "--tx" :type "string" :description "Read through this tx."))
     :output-fields ((:name "defs" :description "[{path,line,end_line,kind,name}] in path and line order.")
                     (:name "total" :description "Definitions before --limit.")
                     (:name "truncated" :description "True when --limit cut the list (status partial, exit 3)."))
     :error-codes ("argument.invalid" "input.not-found" "internal.unexpected"))
    (:name "code.refs"
     :summary "Find the lines that mention a name as a whole identifier."
     :args ((:name "name" :kind "positional" :type "string" :required t :description "The name.")
            (:name "path" :kind "positional" :type "string" :description "Where to look; default the working directory.")
            (:name "--limit" :type "integer" :default 50 :description "Lines returned.")
            (:name "--tx" :type "string" :description "Read through this tx."))
     :output-fields ((:name "refs" :description "[{path,line,kind,text}]; kind is def on a line defining the name, else ref. An occurrence counts only when no identifier character of the file's language touches it.")
                     (:name "total" :description "Lines before --limit.")
                     (:name "truncated" :description "True when --limit cut the list (status partial, exit 3).")
                     (:name "approx_tokens" :description "ceil(characters of returned text / 4)."))
     :error-codes ("argument.invalid" "input.not-found" "internal.unexpected"))
    (:name "overview"
     :summary "Summarize the workspace: root, git state, languages, build files, and top-level entries."
     :args ((:name "path" :kind "positional" :type "string" :description "Summarize below this directory; default the root.")
            (:name "--limit" :type "integer" :default 30 :description "Languages returned.")
            (:name "--no-ignore" :type "flag" :description "Include ignored files.")
            (:name "--tx" :type "string" :description "Read through this tx."))
     :output-fields ((:name "root" :description "The workspace root.")
                     (:name "path" :description "The summarized directory, workspace-relative (\"\" for the root).")
                     (:name "ignore_source" :description "gitignore, builtin, or none.")
                     (:name "git" :description "{branch,head,untracked,deleted} read from .git without running git, or null outside a repository. branch is null on a detached HEAD. untracked and deleted count files below path; files modified in place are not counted.")
                     (:name "languages" :description "[{lang,files,lines,bytes}], most lines first.")
                     (:name "languages_total" :description "Languages before --limit.")
                     (:name "build_files" :description "Workspace-relative paths of build and project files (flake.nix, *.asd, Cargo.toml, package.json, ...).")
                     (:name "entries" :description "[{name,kind}] directly below path.")
                     (:name "truncated" :description "True when --limit cut the languages (status partial, exit 3)."))
     :error-codes ("input.not-found" "argument.invalid" "internal.unexpected")))
  "One plist per search-context command: :NAME, :SUMMARY, :DESCRIPTION,
:ARGS, :OUTPUT-FIELDS, :ERROR-CODES, in COMMAND-SCHEMA terms.")

(export '(*search-scan-args* *search-command-schemas*))
