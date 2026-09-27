;;;; data/domain/protocol/command-placement-data.lisp
;;;;
;;;; The command placement rule as data: the fixed list of top-level
;;;; verbs, and the fixed list of group names. AITOOLS.PROTOCOL.DOMAIN:TOP-
;;;; LEVEL-COMMAND-P and :GROUP-COMMAND-P consult these to decide whether an
;;;; unrecognized dispatch name is genuinely unknown (ARGUMENT.INVALID with
;;;; correspondence-table repairs) or a bare group subcommand name missing its group prefix
;;;; (`uuid` instead of `util uuid`).
(in-package #:aitools.data)

(defparameter *protocol-top-level-commands*
  '("read" "info" "search" "find" "diff" "check" "edit" "insert" "replace" "apply"
    "transform" "move-lines" "write" "split" "transcode" "move" "copy" "delete"
    "mkdir" "chmod" "link" "touch" "mktemp" "overview" "history" "undo" "run" "wait"
    "batch" "schema")
  "The top-level verbs.")

(defparameter *protocol-command-groups*
  '("json" "table" "archive" "code" "snapshot" "tx" "bg" "git" "sys" "time" "util")
  "The group names.")

(export '(*protocol-top-level-commands* *protocol-command-groups*))
