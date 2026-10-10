;;;; data/domain/protocol/correspondence-table-data.lisp
;;;;
;;;; The table of shell operations and their aitools replacements, as data
;;;; shared by three consumers: docs/src/guide/agents.md, the
;;;; unknown-command-name repair (AITOOLS.PROTOCOL.APPLICATION:REPAIRS-FOR-
;;;; UNKNOWN-NAME), and the correspondence e2e tests. Each entry's :FOREIGN-NAMES lists
;;;; the individual tokens a row's backtick-separated shell commands name;
;;;; :REPAIRS is one or more {detail, command} suggestions, since a shell
;;;; command like `sed` maps to different aitools commands depending on what
;;;; it was doing.
;;;;
;;;; The data only has to serve the rule that a name not at the top level
;;;; returns repairs drawn from this table (docs/src/reference/errors.md) --
;;;; it intentionally does not transcribe every cell of the table;
;;;; extend it as later tasks land the commands it points at, then
;;;; regenerate the docs table (docs/src/project/development.md,
;;;; "Regenerating the reference").
(in-package #:aitools.data)

(defparameter *correspondence-table*
  '((:foreign-names ("cat" "head" "tail" "nl")
     :repairs ((:detail "Read a file with line numbers and range control."
                :command "aitools read")))
    (:foreign-names ("grep" "rg" "egrep" "fgrep")
     :repairs ((:detail "Search file contents by pattern."
                :command "aitools search")))
    (:foreign-names ("ls" "fd" "find" "tree" "du")
     :repairs ((:detail "List or find files."
                :command "aitools find")))
    (:foreign-names ("diff" "cmp" "comm")
     :repairs ((:detail "Compare files."
                :command "aitools diff")))
    (:foreign-names ("sed" "perl")
     :repairs ((:detail "Replace a single occurrence by exact old/new text."
                :command "aitools edit")
               (:detail "Replace every occurrence of a pattern, with a required match count."
                :command "aitools replace")))
    (:foreign-names ("awk" "cut")
     :repairs ((:detail "Read delimited or whitespace-separated tabular text."
                :command "aitools table read")))
    (:foreign-names ("sort" "uniq" "tac" "shuf")
     :repairs ((:detail "Reorder, dedupe, reverse, or shuffle lines in place."
                :command "aitools transform")))
    (:foreign-names ("tr" "dos2unix" "expand" "unexpand" "fold" "fmt")
     :repairs ((:detail "Apply a line-level text transform."
                :command "aitools transform")))
    (:foreign-names ("iconv" "nkf")
     :repairs ((:detail "Convert a file's text encoding."
                :command "aitools transcode")))
    (:foreign-names ("cp")
     :repairs ((:detail "Copy a file, or a directory with --recursive."
                :command "aitools copy")))
    (:foreign-names ("mv")
     :repairs ((:detail "Move or rename a file or directory."
                :command "aitools move")))
    (:foreign-names ("rm")
     :repairs ((:detail "Remove one target with journal and undo; trees need --recursive and --expect-count."
                :command "aitools rm")))
    (:foreign-names ("rmdir")
     :repairs ((:detail "Delete a file, a symlink, or an empty directory."
                :command "aitools delete")))
    (:foreign-names ("mkdir")
     :repairs ((:detail "Create a directory and its parents."
                :command "aitools mkdir")))
    (:foreign-names ("chmod")
     :repairs ((:detail "Set executable bits or an octal mode."
                :command "aitools chmod")))
    (:foreign-names ("ln")
     :repairs ((:detail "Create a symbolic link inside the workspace."
                :command "aitools link")))
    (:foreign-names ("touch")
     :repairs ((:detail "Create an empty file or set a file's modification time."
                :command "aitools touch")))
    (:foreign-names ("mktemp")
     :repairs ((:detail "Create a temporary file or directory in the workspace's state tmp/ area."
                :command "aitools mktemp")))
    (:foreign-names ("jq")
     :repairs ((:detail "Read, query, or edit JSON."
                :command "aitools json get")))
    (:foreign-names ("tar" "unzip" "zcat" "gzip" "zip")
     :repairs ((:detail "List, read, extract, or create an archive."
                :command "aitools archive list")))
    (:foreign-names ("git")
     :repairs ((:detail "Read-only git status, log, diff, blame, or show."
                :command "aitools git status")))
    (:foreign-names ("uname" "whoami" "hostname" "nproc")
     :repairs ((:detail "Read system information."
                :command "aitools sys info")))
    (:foreign-names ("env")
     :repairs ((:detail "Read environment variables, with secrets masked."
                :command "aitools sys env")))
    (:foreign-names ("which")
     :repairs ((:detail "Check whether an external tool is available."
                :command "aitools sys tools")))
    (:foreign-names ("ps")
     :repairs ((:detail "List processes (read-only)."
                :command "aitools sys procs")))
    (:foreign-names ("top")
     :repairs ((:detail "Read one noninteractive system metrics snapshot."
                :command "aitools sys metrics")))
    (:foreign-names ("lsof")
     :repairs ((:detail "List listening TCP sockets and their owning processes."
                :command "aitools sys ports")))
    (:foreign-names ("df")
     :repairs ((:detail "Read capacity of the filesystem containing a path, not every mount."
                :command "aitools sys disk")))
    (:foreign-names ("netstat")
     :repairs ((:detail "Read cumulative interface counters, not rates or connectivity."
                :command "aitools sys network")
               (:detail "List listening TCP sockets, not every connection or route."
                :command "aitools sys ports")))
    (:foreign-names ("date")
     :repairs ((:detail "Read or convert the current or a given time."
                :command "aitools time now")))
    (:foreign-names ("base64" "xxd")
     :repairs ((:detail "Encode or decode bytes."
                :command "aitools util encode")))
    (:foreign-names ("bc" "expr")
     :repairs ((:detail "Evaluate an arithmetic expression."
                :command "aitools util calc")))
    (:foreign-names ("uuidgen")
     :repairs ((:detail "Generate a UUID."
                :command "aitools util uuid")))
    (:foreign-names ("uuid")
     :repairs ((:detail "`uuid` is a group subcommand name; the group prefix is required."
                :command "aitools util uuid")))
    (:foreign-names ("openssl")
     :repairs ((:detail "Generate cryptographically random bytes."
                :command "aitools util random"))))
  "Each entry's :FOREIGN-NAMES lists the shell command tokens unknown-name
repair recognizes; :REPAIRS is that entry's list of {:detail :command} suggestions,
in the order they should appear in the JSON error envelope's `repairs`.")

(export '(*correspondence-table*))
