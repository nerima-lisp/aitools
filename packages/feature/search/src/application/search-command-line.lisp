(in-package #:aitools.search.application)

(defconstant +stdin-pattern-limit+ (* 1024 1024)
  "Largest pattern `search --stdin` reads.")

(defparameter *output-modes*
  '((:blocks . "blocks") (:matches . "matches") (:count . "count") (:files . "files")
    (:files-without-match . "files-without-match")))

(defun %mode-name (mode)
  (cdr (assoc mode *output-modes*)))

(defun %search-command-line (request &key limit fixed)
  "The `aitools search` command line reproducing REQUEST (a plist of the
flow's keyword arguments), with LIMIT replacing `--limit` and FIXED adding
`--fixed`. The pattern is passed with `--pattern` so it reads back
unambiguously."
  (destructuring-bind (&key patterns paths ignore-case word line-regexp invert multiline
                         before after output glob lang no-ignore skip-larger-than newer tx
                       &allow-other-keys)
      request
    (command-line
     (append (list "aitools" "search")
             (loop for pattern in patterns append (list "--pattern" pattern))
             (when (or fixed (getf request :fixed)) (list "--fixed"))
             (when ignore-case (list "--ignore-case"))
             (when word (list "--word"))
             (when line-regexp (list "--line-regexp"))
             (when invert (list "--invert"))
             (when multiline (list "--multiline"))
             (when (eq output :blocks)
               (list "--before" (princ-to-string before) "--after" (princ-to-string after)))
             (unless (eq output :blocks) (list "--output" (%mode-name output)))
             (list "--limit" (princ-to-string limit))
             (loop for glob in glob append (list "--glob" glob))
             (when lang (list "--lang" lang))
             (when no-ignore (list "--no-ignore"))
             (when skip-larger-than (list "--skip-larger-than" skip-larger-than))
             (when newer (list "--newer" newer))
             (when tx (list "--tx" tx))
             paths))))
