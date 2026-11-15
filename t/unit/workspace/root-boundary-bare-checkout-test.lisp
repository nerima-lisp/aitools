(in-package #:aitools.workspace.test)

(defun %relative-worktree-host (back-pointer &key (cwd "/bare/.worktrees/wt"))
  (make-fake-workspace-boundaries
   :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                (cons "/bare/config" "[core]")
                (cons "/bare/worktrees/wt/HEAD" "0123")
                (cons "/bare/worktrees/wt/commondir" "../..")
                (cons "/bare/worktrees/wt/gitdir" back-pointer)
                (cons "/bare/.worktrees/wt/.git" "gitdir: ../../worktrees/wt"))
   :directories (list "/bare/objects" "/bare/refs" "/bare/.worktrees/wt/src")
   :symlinks (list (cons "/bare/.worktrees/wt/meta" "../../worktrees/wt"))
   :cwd cwd))

(describe "aitools.workspace.application relative linked-worktree registration"
  (it "accepts a back-pointer relative to the worktree metadata directory"
    (let* ((host (%relative-worktree-host "../../.worktrees/wt/.git"))
           (root (resolved-root host))
           (option-root (resolved-root host :root "/bare/.worktrees/wt")))
      (expect (workspace-root-path root) :to-equal "/bare/.worktrees/wt")
      (expect (git-repository-git-dir (workspace-root-repository root))
              :to-equal "/bare/worktrees/wt")
      (dolist (value (list root option-root))
        (expect (boundary-verdict host value "src/new.txt") :to-be :inside)
        (expect (boundary-verdict host value ".git/config") :to-be :git-directory)
        (expect (boundary-verdict host value "../../HEAD") :to-be :outside-root)
        (expect (boundary-verdict host value "src/../../../config") :to-be :outside-root)
        (expect (boundary-verdict host value "../../worktrees/wt/index.lock")
                :to-be :outside-root)
        (expect (boundary-verdict host value "meta/index.lock") :to-be :symlink-escape))))
  (it "refuses a relative back-pointer that names another checkout"
    (let* ((host (%relative-worktree-host "../../.worktrees/other/.git"))
           (root (resolved-root host)))
      (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory)))
  (it "refuses a checkout whose worktree registration was removed"
    (let* ((host
             (make-fake-workspace-boundaries
              :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                           (cons "/bare/.worktrees/gone/.git"
                                 "gitdir: /bare/worktrees/gone"))
              :directories (list "/bare/objects" "/bare/refs" "/bare/.worktrees/gone/src")
              :cwd "/bare/.worktrees/gone"))
           (root (resolved-root host))
           (option-root (resolved-root host :root "/bare/.worktrees/gone")))
      (expect (workspace-root-path root) :to-equal "/bare")
      (expect (boundary-verdict host root ".worktrees/gone/src/new.txt")
              :to-be :git-directory)
      (expect (boundary-verdict host option-root "src/new.txt") :to-be :git-directory))))
