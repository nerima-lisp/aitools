;;;; packages/feature/journal/src/application/history-flow.lisp
;;;;
;;;; `history [path]`: journal entries newest first, optionally only
;;;; those that touched PATH or something under it. More entries than
;;;; `--limit` make the result partial, with the command that lists
;;;; them all in `next_commands`.
(in-package #:aitools.journal.application)

(defun %relative-history-path (host root path)
  "PATH (absolute, or relative to the working directory) as a
workspace-relative path, \"\" for the root itself, or NIL when it lies
outside the workspace."
  (aitools.workspace.application:resolve-user-path/k
   host root path
   :on-inside (lambda (absolute relative) (declare (ignore absolute)) relative)
   :on-outside (constantly nil)))

(aitools.kernel.domain:define-flow
    (history-flow ports context &key path (limit nil limit-supplied-p) on-ok on-partial on-error)
  (declare (type function on-ok on-partial on-error))
  (unless limit-supplied-p
    (setf limit +default-history-limit+))
  (aitools.kernel.domain:flow-call
    (%call-with-store ports context "history")
    (:on-error (code message &key candidates diagnostics conflicts repairs)
      (aitools.kernel.domain:flow-continue
       :on-error code message
       :candidates candidates
       :diagnostics diagnostics
       :conflicts conflicts
       :repairs repairs))
    (:on-store (store root timeout)
      (declare (ignore timeout))
      (let ((relative (and path (%relative-history-path (journal-ports-workspace-host ports) root path))))
        (if (and path (null relative))
            (aitools.kernel.domain:flow-continue
             :on-error "argument.invalid" (format nil "~A is outside the workspace" path)
             :repairs (list (repair "list-all" "List every operation in this workspace."
                                    (%aitools context "history"))))
            (let ((items '()) (total 0))
              (aitools.store.application:map-journal-entries
               store
               (lambda (entry)
                 (when (< total limit)
                   (push (history-item entry) items))
                 (incf total)
                 nil)
               :path (and relative (plusp (length relative)) relative))
              (let ((fields (list (cons "items" (nreverse items))
                                  (cons "total" total)
                                  (cons "truncated" (json-boolean (> total limit))))))
                (if (> total limit)
                    (aitools.kernel.domain:flow-continue
                     :on-partial (append fields
                                         (list (cons "next_commands"
                                                     (list (%aitools context "history" path
                                                                     "--limit" (princ-to-string total)))))))
                    (aitools.kernel.domain:flow-continue
                     :on-ok fields)))))))))
