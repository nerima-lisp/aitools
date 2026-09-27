;;;; packages/feature/process/src/domain/shell-words.lisp
;;;;
;;;; `repairs[].command` and `next_commands` hold a complete command line an
;;;; agent can paste back into a shell, so any argument echoed into one is
;;;; quoted. aitools itself never hands these strings to a shell.
(in-package #:aitools.process.domain)

(defun command-line (&rest words)
  "Join WORDS (strings, or lists of strings, which are spliced) into one
command line, each word quoted by aitools.protocol.domain:shell-quote."
  (aitools.protocol.domain:command-line
   (loop for word in words
         append (if (listp word) word (list word)))))
