;;;; packages/feature/process/src/domain/shell-words.lisp
;;;;
(in-package #:aitools.process.domain)

(defun command-line (&rest words)
  "Join WORDS (strings, or lists of strings, which are spliced) into one
command line, each word quoted by aitools.protocol.domain:shell-quote."
  (aitools.protocol.domain:command-line
   (loop for word in words
         append (if (listp word) word (list word)))))
