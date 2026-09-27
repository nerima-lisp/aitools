;;;; src/registry.lisp
;;;;
;;;; Turns the protocol command registry into cl-cli's app command list. The
;;;; registry itself lives in AITOOLS.PROTOCOL.APPLICATION so presentation
;;;; layers can register without referencing this package; this file is the
;;;; one place that interprets registered cli-command values as cl-cli
;;;; commands, wrapping same-group commands (`json get`, `json set`, ...) into
;;;; one parent cl-cli command per group.
(in-package #:aitools/cli)

(defun finalize-app-commands (registry)
  "The list of cl-cli command structs to pass as MAKE-APP's :COMMANDS: every
ungrouped command, plus one MAKE-COMMAND per group wrapping that group's
accumulated subcommands."
  (append (reverse (command-registry-top-level registry))
          (loop for group in (sort (loop for group being the hash-keys of (command-registry-group-commands registry)
                                         collect group)
                                   #'string<)
                collect (make-command :name group
                                      :subcommands (reverse (gethash group (command-registry-group-commands registry)))))))
