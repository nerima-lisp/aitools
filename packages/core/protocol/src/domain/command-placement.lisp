;;;; Mapping unknown command names to repair suggestions.
(in-package #:aitools.protocol.domain)

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
for an unrecognized dispatch NAME. A group's own subcommand name (`uuid`) is
pointed at its group form (`aitools util uuid`); names in the correspondence
table use that entry's suggestions. Other names get a `browse-commands`
suggestion pointing at `aitools schema`."
  (let ((table-entry (%correspondence-entry name)))
    (or (and table-entry
             (mapcar (lambda (repair)
                       (list :action "run-instead" :detail (getf repair :detail)
                             :command (getf repair :command)))
                     (getf table-entry :repairs)))
        (list (list :action "browse-commands" :detail "List every implemented command."
                    :command "aitools schema")))))
