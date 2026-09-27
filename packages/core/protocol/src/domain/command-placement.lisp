;;;; packages/core/protocol/src/domain/command-placement.lisp
;;;;
;;;; Deciding whether a dispatch name is a known top-level
;;;; command, a known group, or genuinely unknown, and what repairs an
;;;; unknown name should offer.
(in-package #:aitools.protocol.domain)

(defparameter *top-level-commands* aitools.data:*protocol-top-level-commands*)
(defparameter *command-groups* aitools.data:*protocol-command-groups*)

(defun top-level-command-p (name)
  (member name *top-level-commands* :test #'string=))

(defun group-command-p (name)
  (member name *command-groups* :test #'string=))

(defun %correspondence-entry (name)
  (find-if (lambda (entry) (member name (getf entry :foreign-names) :test #'string=))
           aitools.data:*correspondence-table*))

(defun correspondence-name-p (name)
  "True when NAME is a foreign name in the correspondence table, so its
repair is the table's canonical command (`fmt` -> `aitools transform`), which
takes precedence over a coincidental group subcommand of the same name."
  (and (%correspondence-entry name) t))

(defun repairs-for-unknown-name (name)
  "Return the `repairs` list (a list of (:action :detail :command) plists)
for an unrecognized dispatch NAME: a name that is a
group's own subcommand name (`uuid`) is pointed at its group form (`aitools
util uuid`); a name in the correspondence table is pointed at the
aitools command(s) that replace it. Returns NIL when NAME matches neither --
the caller still owes ARGUMENT.INVALID at least one repair, so it falls back
to pointing at `aitools schema`."
  (let ((table-entry (%correspondence-entry name)))
    (or (and table-entry
             (mapcar (lambda (repair)
                       (list :action "run-instead" :detail (getf repair :detail)
                             :command (getf repair :command)))
                     (getf table-entry :repairs)))
        (list (list :action "browse-commands" :detail "List every implemented command."
                    :command "aitools schema")))))
