;;;; packages/feature/journal/src/domain/command-line.lisp
;;;;
;;;; `repairs[].command`, `next_commands` and `history`'s `command` hold a
;;;; command line an agent can paste into a shell, so every word echoed into
;;;; one is quoted. aitools itself never hands these strings to a shell.
(in-package #:aitools.journal.domain)

(defun command-line (&rest words)
  "Join WORDS (strings, lists of strings spliced in, or NIL skipped) into one
command line, each word quoted by aitools.protocol.domain:shell-quote."
  (aitools.protocol.domain:command-line
   (loop for word in words
         append (if (listp word) word (list word)))))

(defun argv-command-line (argv)
  "A journal entry's recorded ARGV as the `aitools ...` line that ran it."
  (if (equal (first argv) "aitools")
      (command-line argv)
      (command-line "aitools" argv)))
