;;;; packages/core/protocol/src/domain/shell-words.lisp
;;;;
;;;; `repairs[].command` and `next_commands` hold a complete command line an
;;;; agent can paste back into a POSIX shell, so every argument echoed into
;;;; one is quoted. aitools itself never hands these strings to a shell.
(in-package #:aitools.protocol.domain)

(defun %shell-safe-char-p (char)
  (or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9)
      (find char "-_./=:,@%+")))

(defun shell-quote (argument)
  "ARGUMENT unchanged when it is nonempty and every character is shell-inert,
else wrapped in single quotes with each embedded quote spelled '\\''."
  (if (and (plusp (length argument)) (every #'%shell-safe-char-p argument))
      argument
      (with-output-to-string (out)
        (write-char #\' out)
        (loop for char across argument
              do (if (char= char #\')
                     (write-string "'\\''" out)
                     (write-char char out)))
        (write-char #\' out))))

(defun command-line (words)
  "WORDS, a list of strings, as one command line with each word quoted by
SHELL-QUOTE."
  (format nil "~{~A~^ ~}" (mapcar #'shell-quote words)))
