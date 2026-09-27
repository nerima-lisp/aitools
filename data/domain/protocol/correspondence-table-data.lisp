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
    (:foreign-names ("cp" "mv" "rm" "rmdir")
     :repairs ((:detail "Copy, move, or delete a file or empty directory."
                :command "aitools copy")))
    (:foreign-names ("mkdir" "chmod" "ln" "touch" "mktemp")
     :repairs ((:detail "Create a directory, change mode, link, touch, or make a temp path."
                :command "aitools mkdir")))
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
    (:foreign-names ("ps" "lsof")
     :repairs ((:detail "List processes or listening ports."
                :command "aitools sys procs")))
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
