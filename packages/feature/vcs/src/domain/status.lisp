;;;; packages/feature/vcs/src/domain/status.lisp
;;;;
;;;; `git status` output from a porcelain-v2 status snapshot. The snapshot
;;;; arrives as a plist (see AITOOLS.VCS.APPLICATION:GIT-PORT) so this layer
;;;; never touches cl-vcs-kit's structs.
(in-package #:aitools.vcs.domain)

(defun %status-change (path code original-path)
  (json-object-from-alist (append (list (cons "path" path) (cons "status" code))
                                  (when original-path (list (cons "from" original-path))))))

(defun %count-or-null (value)
  ;; cl-vcs-kit parses `# branch.ab +A -B` with PARSE-INTEGER, so BEHIND
  ;; arrives negative.
  (if value (abs value) (json-null)))

(defun status-fields (snapshot)
  "The `git status` fields for SNAPSHOT, a plist (:BRANCH :UPSTREAM :AHEAD :BEHIND
:ENTRIES), where each entry is a plist (:KIND :INDEX :WORKTREE :PATH
:ORIGINAL-PATH) using porcelain-v2 one-letter codes and `.` for unchanged.
An unmerged path is reported as unstaged with status `U`."
  (let (staged unstaged untracked)
    (dolist (entry (getf snapshot :entries))
      (destructuring-bind (&key kind index worktree path original-path) entry
        (ecase kind
          (:untracked (push path untracked))
          (:ignored nil)
          (:unmerged (push (%status-change path "U" nil) unstaged))
          ((:ordinary :rename-or-copy)
           (unless (string= index ".")
             (push (%status-change path index original-path) staged))
           (unless (string= worktree ".")
             (push (%status-change path worktree nil) unstaged))))))
    (list (cons "branch" (or (getf snapshot :branch) (json-null)))
          (cons "upstream" (or (getf snapshot :upstream) (json-null)))
          (cons "ahead" (%count-or-null (getf snapshot :ahead)))
          (cons "behind" (%count-or-null (getf snapshot :behind)))
          (cons "staged" (nreverse staged))
          (cons "unstaged" (nreverse unstaged))
          (cons "untracked" (nreverse untracked)))))
