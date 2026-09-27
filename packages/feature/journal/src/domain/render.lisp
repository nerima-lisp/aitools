;;;; packages/feature/journal/src/domain/render.lisp
;;;;
;;;; JSON shapes of `history`, `tx status`'s tx paths, and the repairs
;;;; of commit conflicts. A tx path's `base` and `staged`, like a
;;;; conflict's, are shown as the content hash of a regular file or null.
(in-package #:aitools.journal.domain)

(defconstant +default-history-limit+ 50)

(defun %null-or (value)
  (if (null value) json-kit:+json-null+ value))

(defun history-item (entry)
  "`history`'s `items[]` element for journal ENTRY: {op_id, command, paths,
time, undoes?}; `undoes` only on an entry written by `undo`."
  (apply #'json-object
         "op_id" (aitools.store.domain:journal-entry-op-id entry)
         "command" (argv-command-line (aitools.store.domain:journal-entry-argv entry))
         "paths" (aitools.store.domain:journal-entry-paths entry)
         "time" (aitools.store.domain:journal-entry-time entry)
         (let ((undoes (aitools.store.domain:journal-entry-undoes entry)))
           (when undoes (list "undoes" undoes)))))

(defun tx-path-action (entry)
  "The write-result action that committing tx path ENTRY would perform, or
\"unchanged\" when its staged state equals its base (an op wrote a file back
to its original content)."
  (let* ((base (aitools.store.domain:tx-path-base entry))
         (staged (aitools.store.domain:tx-path-staged entry))
         (base-kind (aitools.store.domain:entry-state-kind base))
         (staged-kind (aitools.store.domain:entry-state-kind staged)))
    (cond ((not (aitools.store.domain:tx-staged-changed-p entry)) "unchanged")
          ((eq staged-kind :absent) "deleted")
          ((eq staged-kind :symlink) "linked")
          ((eq base-kind :absent) "created")
          ((and (eq base-kind staged-kind)
                (member base-kind '(:file :directory))
                (equal (aitools.store.domain:entry-state-hash base)
                       (aitools.store.domain:entry-state-hash staged)))
           "mode-changed")
          (t "modified"))))

(defun state-hash (state)
  "A tx path state or a conflict side as shown in JSON: the content hash of
a regular file, else null."
  (%null-or (aitools.store.domain:entry-state-hash state)))

(defun repair (action detail command)
  (list :action action :detail detail :command command))

(defun commit-conflict-repairs (tx-id conflicts &key globals)
  "The repairs for `tx commit` CONFLICTS: `aitools tx rebase <tx>` for
write conflicts; for read conflicts, re-reading each path through the tx and
`tx commit --ignore-stale-reads`. GLOBALS are global option words placed
after `aitools`."
  (let ((write (find :write conflicts :key #'aitools.store.domain:conflict-kind))
        (reads (remove :write conflicts :key #'aitools.store.domain:conflict-kind)))
    (append
     (when write
       (list (repair "rebase" "Re-apply the tx's content-based operations onto the current files."
                     (command-line "aitools" globals "tx" "rebase" tx-id))))
     (mapcar (lambda (conflict)
               (repair "reread" "Read the changed file again through the tx to refresh the read set."
                       (command-line "aitools" globals "read" (aitools.store.domain:conflict-path conflict)
                                     "--tx" tx-id)))
             reads)
     (when reads
       (list (repair "ignore-stale-reads" "Commit even though files read through the tx have changed."
                     (command-line "aitools" globals "tx" "commit" tx-id "--ignore-stale-reads")))))))
