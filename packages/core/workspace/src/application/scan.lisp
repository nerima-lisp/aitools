;;;; packages/core/workspace/src/application/scan.lisp
;;;;
;;;; The ignore-aware workspace walk, shared by search, find, overview,
;;;; snapshot, and archive create. Ignore decisions happen during the walk,
;;;; so an ignored directory is never listed. Entries are emitted
;;;; in path order (see ENTRY-ORDER-KEY); optional per-file WORK runs on the
;;;; host's ordered mapper in batches, and results are still delivered in
;;;; path order, so parallelism never changes the output.
;;;;
;;;; A tx overlay (docs/src/reference/transactions.md) plugs in through WORKSPACE-OVERLAY: it
;;;; can add, remove, or replace directory entries and override the bytes of
;;;; any file the walk reads itself (.gitignore), so staged .gitignore edits
;;;; take effect in scans.
(in-package #:aitools.workspace.application)

(aitools.kernel.domain:define-flow
      (call-with-workspace-scan/k host root
        &key paths glob lang no-ignore
          (skip-larger-than +default-skip-larger-than+)
          newer overlay work emit
          (on-skip (lambda (entry reason) (declare (ignore entry reason)) nil))
          on-complete on-error)
    "Walk the workspace below ROOT and emit accepted entries in path order.

PATHS are absolute or root-relative starting points; the default is the
whole root. An explicitly named file is emitted even when ignored, and GLOB
or LANG do not filter it.
NO-IGNORE disables .gitignore and builtin excludes. Git metadata and
aitools temporary files are always skipped.
SKIP-LARGER-THAN reports larger files to ON-SKIP as :TOO-LARGE; NIL disables
the limit. NEWER keeps entries with a later mtime.
OVERLAY replaces directory listings and file bytes for staged changes.
WORK runs on the host's ordered mapper in batches. EMIT receives each entry
and result; returning :STOP ends the scan.
ON-SKIP receives an entry and :TOO-LARGE or :UNREADABLE.
ON-COMPLETE receives the ignore source and stopped flag.
ON-ERROR receives :OUTSIDE-ROOT or :NOT-FOUND and the failed start."
    (declare (type function emit))
    (let* ((context (load-ignore-context host root :no-ignore no-ignore))
           (source (ignore-context-source context))
           (filter (make-glob-filter glob :casefold (ignore-context-casefold context)))
           (skip (lambda (entry reason)
                   (aitools.kernel.domain:flow-continue :on-skip entry reason)))
           (complete (lambda (ignore-source stopped-p)
                       (aitools.kernel.domain:flow-continue
                        :on-complete ignore-source stopped-p))))
      (multiple-value-bind (starts reason path)
          (%scan-start-entries host root context overlay paths)
        (if reason
            (aitools.kernel.domain:flow-continue :on-error reason path)
            (%run-scan
             host root context overlay filter lang skip-larger-than newer
             work emit skip complete source starts)))))

(defun workspace-path-ignored-p (host root path &key no-ignore overlay)
  "(VALUES IGNORED-P IGNORE-SOURCE) for `info`'s `ignored` field. PATH is
absolute or root-relative; a path outside the root is never ignored. An
ancestor directory being ignored (and not holding tracked files) makes PATH
ignored, as git would never list it."
  (let* ((context (load-ignore-context host root :no-ignore no-ignore))
         (source (ignore-context-source context))
         (relative (%relative-start host root path)))
    (cond
      ((null relative) (values nil source))
      ((some #'%always-skipped-name-p (path-components relative)) (values t source))
      ((eq source :none) (values nil source))
      (t
       (let* ((top-relative (join-path (ignore-context-prefix context) relative))
              (components (path-components top-relative))
              (stack (ignore-context-base-stack context))
              (directory ""))
         (loop for (component . rest) on components
               for child = (join-path directory component)
               do (let ((list (%directory-ignore-list host context overlay directory
                                                      (%gitignore-entry host context overlay directory))))
                    (when list (push list stack)))
                  (let* ((directory-p (if rest
                                          t
                                          (let ((entry (%lookup-entry host context overlay relative)))
                                            (and entry (eq (workspace-entry-kind entry) :directory)))))
                         (tracked (%tracked-p context child directory-p)))
                    (when (and (%ignored-p context stack child directory-p) (not tracked))
                      (return-from workspace-path-ignored-p (values t source))))
                  (setf directory child))
         (values nil source))))))
