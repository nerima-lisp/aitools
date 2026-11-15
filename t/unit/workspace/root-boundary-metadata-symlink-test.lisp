(in-package #:aitools.workspace.test)

;;; Metadata paths are followed as the kernel follows them: a `..` after a
;;; symlink applies to the link's target directory. /a/link leads into
;;; repository B, so /a/link/../r.git is B's r.git, not A's.
(defun %spoof-jj-host (git-target)
  (make-fake-workspace-boundaries
   :files (list (cons "/a/r.git/HEAD" "ref: refs/heads/main")
                (cons "/b/r.git/HEAD" "ref: refs/heads/main")
                (cons "/jjrepo/store/git_target" git-target)
                (cons "/a/r.git/.worktrees/jw/.jj/repo" "/jjrepo"))
   :directories (list "/a/r.git/objects" "/a/r.git/refs" "/b/r.git/objects" "/b/r.git/refs"
                      "/b/sub" "/jjrepo/op_store" "/jjrepo/op_heads"
                      "/a/r.git/.worktrees/jw/src")
   :symlinks (list (cons "/a/link" "/b/sub") (cons "/a/loop" "loop"))
   :cwd "/a/r.git/.worktrees/jw/src"))

(defun %spoof-worktree-host (back-pointer &key (dot-git "gitdir: ../../worktrees/wt")
                                             (commondir "../.."))
  (make-fake-workspace-boundaries
   :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                (cons "/bare/worktrees/wt/HEAD" "0123")
                (cons "/bare/worktrees/wt/commondir" commondir)
                (cons "/bare/worktrees/wt/gitdir" back-pointer)
                (cons "/bare/.worktrees/wt/.git" dot-git)
                (cons "/other/.worktrees/wt/.git" "gitdir: /other/worktrees/wt"))
   :directories (list "/bare/objects" "/bare/refs" "/bare/.worktrees/wt/src" "/other/sub")
   :symlinks (list (cons "/bare/decoy" "/other/sub")
                   (cons "/bare/worktrees/wt/loop" "loop")
                   (cons "/bare/.worktrees/wt/loop" "loop"))
   :cwd "/bare/.worktrees/wt"))

(describe "aitools.workspace.application metadata paths through symlinks"
  (it "refuses a jj workspace whose git_target reaches another repository through link/.."
    (dolist (target (list "/a/link/../r.git" "/b/r.git" "../../a/link/../r.git"
                          "/a/loop/../r.git"))
      (let* ((host (%spoof-jj-host target))
             (root (resolved-root host)))
        (expect (workspace-root-path root) :to-equal "/a/r.git/.worktrees/jw")
        (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory))))
  (it "allows a jj workspace whose git_target names the enclosing repository"
    (dolist (target (list "/a/r.git" "../../a/r.git" "/a/link/../../a/r.git"))
      (let* ((host (%spoof-jj-host target))
             (root (resolved-root host)))
        (expect (boundary-verdict host root "src/new.txt") :to-be :inside))))
  (it "refuses a worktree whose gitdir back-pointer reaches another checkout through link/.."
    (let* ((host (%spoof-worktree-host "../../decoy/../.worktrees/wt/.git"))
           (root (resolved-root host)))
      (expect (workspace-root-path root) :to-equal "/bare/.worktrees/wt")
      (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory))
    (let* ((host (%spoof-worktree-host "../../.worktrees/wt/.git"))
           (root (resolved-root host)))
      (expect (boundary-verdict host root "src/new.txt") :to-be :inside)))
  (it "follows a .git file and commondir through link/.. as the kernel does"
    (let* ((host
             (make-fake-workspace-boundaries
              :files (list (cons "/w/.git" "gitdir: link/../meta")
                           (cons "/other/meta/HEAD" "0123")
                           (cons "/other/meta/commondir" "link/../common")
                           (cons "/w/meta/HEAD" "0123"))
              :directories (list "/other/sub" "/other/common" "/w/common")
              :symlinks (list (cons "/w/link" "/other/sub")
                              (cons "/other/meta/link" "/other/sub"))
              :cwd "/w"))
           (repository (workspace-root-repository (resolved-root host))))
      (expect (git-repository-git-dir repository) :to-equal "/other/meta")
      (expect (git-repository-common-dir repository) :to-equal "/other/common")))
  (it "grants nothing to a pointer that only resolves by folding .. over a symlink loop"
    (let ((host (%jj-workspace-host "/bare/.worktrees/jw" "loop/../../../../.jj/repo"
                                    :symlinks (list (cons "/bare/.worktrees/jw/.jj/loop" "loop")))))
      (expect (aitools.workspace.application::%metadata-path
               host "/bare/.worktrees/jw/.jj" "loop/../../../../.jj/repo")
              :to-be nil)
      (expect (workspace-root-path (resolved-root host)) :to-equal "/bare")
      (expect (boundary-verdict host (resolved-root host :root "/bare/.worktrees/jw") "src/new.txt")
              :to-be :git-directory))
    (let* ((host (%spoof-worktree-host "loop/../../../.worktrees/wt/.git"))
           (root (resolved-root host)))
      (expect (workspace-root-path root) :to-equal "/bare/.worktrees/wt")
      (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory)))
  (it "keeps a looping .git gitdir or commondir unresolvable instead of folding it"
    (let ((host (%spoof-worktree-host "../../.worktrees/wt/.git"
                                      :dot-git "gitdir: loop/../../../worktrees/wt")))
      (expect (workspace-root-path (resolved-root host)) :to-equal "/bare")
      (expect (boundary-verdict host (resolved-root host :root "/bare/.worktrees/wt") "src/new.txt")
              :to-be :git-directory))
    (let* ((host (%spoof-worktree-host "../../.worktrees/wt/.git" :commondir "loop/../../.."))
           (root (resolved-root host)))
      (expect (git-repository-common-dir (workspace-root-repository root))
              :to-equal "/bare/worktrees/wt/loop/../../..")
      (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory))))
