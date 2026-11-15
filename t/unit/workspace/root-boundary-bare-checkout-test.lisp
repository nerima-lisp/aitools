(in-package #:aitools.workspace.test)

;;; A bare repository at /bare whose own root is also the jj workspace that
;;; created the jj repository, the layout ghq and `jj git init --git-repo .`
;;; leave behind.
(defun %bare-jj-files ()
  (list (cons "/bare/HEAD" "ref: refs/heads/main")
        (cons "/bare/config" "[core]")))

(defun %bare-jj-directories ()
  (list "/bare/objects" "/bare/refs" "/bare/worktrees"
        "/bare/.jj/repo/store" "/bare/.jj/repo/op_store" "/bare/.jj/repo/op_heads"
        "/bare/.jj/working_copy"))

(defun %jj-workspace-host (workspace repo-pointer &key extra-files extra-directories symlinks (cwd workspace))
  (make-fake-workspace-boundaries
   :files (append (%bare-jj-files)
                  (list (cons (format nil "~A/.jj/repo" workspace) repo-pointer))
                  extra-files)
   :directories (append (%bare-jj-directories)
                        (list (format nil "~A/.jj/working_copy" workspace)
                              (format nil "~A/src" workspace))
                        extra-directories)
   :symlinks symlinks
   :cwd cwd))

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

(describe "aitools.workspace.application jj workspace roots"
  (it "roots discovery at a jj workspace below a bare repository and allows writes there"
    (dolist (pointer (list "../../../.jj/repo" "/bare/.jj/repo"))
      (let* ((workspace "/bare/.worktrees/jw")
             (host (%jj-workspace-host workspace pointer :cwd "/bare/.worktrees/jw/src"))
             (root (resolved-root host))
             (option-root (resolved-root host :root workspace)))
        (expect (workspace-root-path root) :to-equal workspace)
        (expect (aitools.workspace.domain::workspace-root-source root) :to-be :jj)
        (expect (git-repository-top (workspace-root-repository root)) :to-equal "/bare")
        (dolist (value (list root option-root))
          (expect (boundary-verdict host value "src/new.txt") :to-be :inside)
          (expect (boundary-verdict host value "new.txt") :to-be :inside)))))
  (it "refuses the workspace's own .jj and every path into the bare metadata"
    (let* ((workspace "/bare/.worktrees/jw")
           (host (%jj-workspace-host
                  workspace "../../../.jj/repo"
                  :symlinks (list (cons "/bare/.worktrees/jw/objects-link" "../../objects")
                                  (cons "/bare/.worktrees/jw/head-link" "/bare/HEAD")
                                  (cons "/bare/.worktrees/jw/state-link" ".jj"))))
           (root (resolved-root host :root workspace)))
      (expect (boundary-verdict host root ".jj/working_copy/x") :to-be :git-directory)
      (expect (boundary-verdict host root ".jj/repo") :to-be :git-directory)
      (expect (boundary-verdict host root ".JJ/x") :to-be :git-directory)
      (expect (boundary-verdict host root "state-link/x") :to-be :git-directory)
      (expect (boundary-verdict host root ".git/config") :to-be :git-directory)
      (dolist (target (list "../../HEAD" "../../config" "../../refs/x" "../../objects/x"
                            "../../worktrees/wt/x" "../../.jj/x" "src/../../../.jj/repo/x"))
        (expect (boundary-verdict host root target) :to-be :outside-root))
      (expect (boundary-verdict host root "objects-link/x") :to-be :symlink-escape)
      (expect (boundary-verdict host root "head-link") :to-be :symlink-escape)))
  (it "refuses a jj workspace whose .jj/repo leads to no jj repository"
    (dolist (pointer (list "../../../.jj/missing" "" "../../../objects"))
      (let* ((workspace "/bare/.worktrees/jw")
             (host (%jj-workspace-host workspace pointer))
             (root (resolved-root host))
             (option-root (resolved-root host :root workspace)))
        (expect (workspace-root-path root) :to-equal "/bare")
        (expect (boundary-verdict host option-root "src/new.txt") :to-be :git-directory)))
    (let* ((workspace "/bare/.worktrees/jw")
           (host (%jj-workspace-host workspace "/half/repo"
                                     :extra-directories (list "/half/repo/store"
                                                              "/half/repo/op_store"))))
      (expect (boundary-verdict host (resolved-root host :root workspace) "src/new.txt")
              :to-be :git-directory))
    (let ((host (make-fake-workspace-boundaries
                 :files (%bare-jj-files)
                 :directories (append (%bare-jj-directories)
                                      (list "/bare/.worktrees/jw/.jj/working_copy"))
                 :symlinks (list (cons "/bare/.worktrees/jw/.jj/repo" "/bare/HEAD")))))
      (expect (boundary-verdict host (resolved-root host :root "/bare/.worktrees/jw") "new.txt")
              :to-be :git-directory)))
  (it "accepts a jj workspace by its metadata, not its directory name"
    (let* ((workspace "/bare/checkouts/jw")
           (host (%jj-workspace-host workspace "../../../.jj/repo"))
           (root (resolved-root host)))
      (expect (workspace-root-path root) :to-equal workspace)
      (expect (boundary-verdict host root "src/new.txt") :to-be :inside)))
  (it "keeps jj workspaces placed in bare metadata refused"
    (dolist (workspace (list "/bare/worktrees/jw" "/bare/refs/jw" "/bare/.jj/jw"
                             "/bare/.jj/repo/store/jw"))
      (let* ((pointer (format nil "~{~A~^/~}/.jj/repo"
                              (make-list (length (path-components workspace))
                                         :initial-element "..")))
             (host (%jj-workspace-host workspace pointer))
             (root (resolved-root host :root workspace)))
        (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory))))
  (it "keeps the bare root's metadata and .jj refused under --root"
    (let* ((host (%jj-workspace-host "/bare/.worktrees/jw" "../../../.jj/repo"))
           (root (resolved-root host :root "/bare")))
      (dolist (target (list "HEAD" "config" "refs/x" "objects/x" "worktrees/wt/x"
                            ".jj/x" ".jj/repo/store/x" ".worktrees/jw/src/new.txt"))
        (expect (boundary-verdict host root target) :to-be :git-directory))))
  (it "leaves an ordinary repository unchanged"
    (let* ((host
             (make-fake-workspace-boundaries
              :files (list (cons "/repo/.git/HEAD" "ref: refs/heads/main"))
              :directories (list "/repo/.git/objects" "/repo/.git/refs" "/repo/.jj/notes"
                                 "/repo/sub")
              :cwd "/repo/sub"))
           (root (resolved-root host)))
      (expect (workspace-root-path root) :to-equal "/repo")
      (expect (aitools.workspace.domain::workspace-root-source root) :to-be :git)
      (expect (boundary-verdict host root "sub/new.txt") :to-be :inside)
      (expect (boundary-verdict host root ".jj/notes/x") :to-be :inside)
      (expect (boundary-verdict host root ".git/config") :to-be :git-directory)))
  (it "protects the .jj of a colocated repository that is also a jj workspace"
    (let* ((host
             (make-fake-workspace-boundaries
              :files (list (cons "/repo/.git/HEAD" "ref: refs/heads/main"))
              :directories (list "/repo/.git/objects" "/repo/.git/refs"
                                 "/repo/.jj/repo/store" "/repo/.jj/repo/op_store"
                                 "/repo/.jj/repo/op_heads" "/repo/src")
              :cwd "/repo/src"))
           (root (resolved-root host)))
      (expect (aitools.workspace.domain::workspace-root-source root) :to-be :git)
      (expect (boundary-verdict host root "src/new.txt") :to-be :inside)
      (expect (boundary-verdict host root ".jj/repo/store/x") :to-be :git-directory))))
