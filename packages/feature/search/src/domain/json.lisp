;;;; packages/feature/search/src/domain/json.lisp
;;;;
;;;; JSON value helpers and the command-line text used in `next_commands` and
;;;; `repairs`. json-kit writes NIL as `[]`, so absent scalars and false flags
;;;; need its explicit null and false values.
(in-package #:aitools.search.domain)

(defun %shell-safe-char-p (char)
  (or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9)
      (find char "_./:@%+=,-")))

(defun shell-quote (word)
  "WORD as one POSIX shell word: unchanged when every character is safe,
otherwise single-quoted with embedded quotes spelled '\\''."
  (if (and (plusp (length word)) (every #'%shell-safe-char-p word))
      word
      (with-output-to-string (out)
        (write-char #\' out)
        (loop for char across word
              do (if (char= char #\')
                     (write-string "'\\''" out)
                     (write-char char out)))
        (write-char #\' out))))

(defun command-line (words)
  "Join WORDS (strings; NIL entries are dropped) into one shell command line,
quoting each word that needs it."
  (format nil "~{~A~^ ~}" (mapcar #'shell-quote (remove nil words))))
