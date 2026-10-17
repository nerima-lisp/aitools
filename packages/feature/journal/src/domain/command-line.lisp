;;;; packages/feature/journal/src/domain/command-line.lisp
;;;;
;;;; `repairs[].command`, `next_commands` and `history`'s `command` hold a
;;;; command line an agent can paste into a shell, so every word echoed into
;;;; one is quoted. aitools itself never hands these strings to a shell.
(in-package #:aitools.journal.domain)

(defun argv-command-line (argv)
  "A journal entry's recorded ARGV as the `aitools ...` line that ran it."
  (if (equal (first argv) "aitools")
      (aitools.protocol.domain:command-line argv)
      (aitools.protocol.domain:command-line "aitools" argv)))
