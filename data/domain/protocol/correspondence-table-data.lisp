;;;; The agent guide, protocol domain's unknown-name handling, and
;;;; correspondence tests use this table. Each entry's :FOREIGN-NAMES lists
;;;; names used in its guide rows, including command and subcommand names.
;;;; :REPAIRS contains context-specific suggestions; `sed` can need different
;;;; replacements depending on what it was doing.
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
     :repairs ((:detail "Remove an empty directory with the journaled rm command."
                :command "aitools rm")))
    (:foreign-names ("delete")
     :repairs ((:detail "The former file-removal command was removed; use rm."
                :command "aitools rm")))
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
    (:foreign-names ("jq" "json.get" "yaml.get" "toml.get")
     :repairs ((:detail "Read and query structured documents."
                :command "aitools doc get")))
    (:foreign-names ("tar" "unzip" "zcat" "gzip" "zip")
     :repairs ((:detail "Use archive list, read, extract, or create for the matching operation."
                :command "aitools archive list")))
    (:foreign-names ("git")
     :repairs ((:detail "Run Git through aitools run."
                :command "aitools run -- git")))
    (:foreign-names ("uname" "whoami" "hostname" "nproc")
     :repairs ((:detail "Read system information."
                :command "aitools sys info")))
    (:foreign-names ("uptime")
     :repairs ((:detail "Read system uptime."
                :command "aitools sys info")
               (:detail "Read load averages."
                :command "aitools sys metrics")))
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
    (:foreign-names ("curl" "WebFetch")
     :repairs ((:detail "Fetch an HTTP resource with GET and a bounded response body."
                :command "aitools net get")
               (:detail "Fetch response headers with HEAD."
                :command "aitools net head")))
    (:foreign-names ("dig" "nslookup")
     :repairs ((:detail "Resolve A and AAAA address results through the operating system resolver. CNAME chains and DNS response codes require dig or nslookup."
                :command "aitools net resolve")))
    (:foreign-names ("date")
     :repairs ((:detail "Read or convert the current or a given time."
                :command "aitools time now")))
    (:foreign-names ("base64" "xxd")
     :repairs ((:detail "Encode bytes with util encode; use util decode to decode them."
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
                :command "aitools util random")))
    (:foreign-names ("json.select" "yaml.select" "toml.select")
     :repairs ((:detail "Filter and project a document array."
                :command "aitools doc select")))
    (:foreign-names ("json.diff" "yaml.diff" "toml.diff")
     :repairs ((:detail "Compare two structured documents."
                :command "aitools doc diff")))
    (:foreign-names ("json.set" "yaml.set" "toml.set")
     :repairs ((:detail "Set a value in a structured document."
                :command "aitools doc set")))
    (:foreign-names ("json.delete" "yaml.delete" "toml.delete")
     :repairs ((:detail "Delete a value from a structured document."
                :command "aitools doc delete")))
    (:foreign-names ("json.merge" "yaml.merge" "toml.merge")
     :repairs ((:detail "Apply a merge patch to a structured document."
                :command "aitools doc merge")))
    (:foreign-names ("json.patch" "yaml.patch" "toml.patch")
     :repairs ((:detail "Apply a JSON Patch to a structured document."
                :command "aitools doc patch")))
    (:foreign-names ("json.fmt" "yaml.fmt" "toml.fmt")
     :repairs ((:detail "Format a structured document."
                :command "aitools doc fmt"))))
  "The :FOREIGN-NAMES are matched by REPAIRS-FOR-UNKNOWN-NAME; :REPAIRS
gives the matching details and command suggestions in order.")

(export '(*correspondence-table*))
