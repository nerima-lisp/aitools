(in-package #:aitools.workspace.test)

(describe "aitools.workspace.application linked-worktree write boundary"
  (it "allows a common-directory child outside metadata and guards Git paths"
      (let* ((host
               (make-fake-workspace-boundaries
                :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                             (cons "/bare/worktrees/wt/HEAD" "0123")
                             (cons "/bare/worktrees/wt/commondir" "../..")
                             (cons "/bare/.worktrees/wt/.git" "gitdir: /bare/worktrees/wt"))
                :directories (list "/bare/.worktrees/wt/src")
                :symlinks (list (cons "/bare/.worktrees/wt/git-dir" "/bare/worktrees/wt"))
                :cwd "/bare/.worktrees/wt"))
             (root (resolved-root host))
             (repository (workspace-root-repository root)))
        (expect (git-repository-top repository) :to-equal "/bare/.worktrees/wt")
        (expect (git-repository-git-dir repository) :to-equal "/bare/worktrees/wt")
        (expect (git-repository-common-dir repository) :to-equal "/bare")
        (expect (boundary-verdict host root "src/new.txt") :to-be :inside)
        (expect (boundary-verdict host root ".git/config") :to-be :git-directory)
        (expect (boundary-verdict host root "git-dir/index.lock") :to-be :symlink-escape)))
  (it "discovers bare Git common directories for explicit roots"
  (let* ((host
           (make-fake-workspace-boundaries
            :files (list (cons "/bare/HEAD" "ref: refs/heads/main"))
            :directories (list "/bare/objects"
                               "/bare/refs"
                               "/bare/objects/pack"
                               "/bare/.worktrees/wt")
            :cwd "/outside"))
         (metadata-root (resolved-root host :root "/bare/objects/pack"))
         (allowed-root (resolved-root host :root "/bare/.worktrees/wt"))
         (repository (workspace-root-repository metadata-root)))
    (expect (git-repository-top repository) :to-equal "/bare")
    (expect (git-repository-git-dir repository) :to-equal "/bare")
    (expect (git-repository-common-dir repository) :to-equal "/bare")
    (expect (boundary-verdict host metadata-root "new.idx") :to-be :git-directory)
    (expect (boundary-verdict host allowed-root "src/new.txt") :to-be :inside)
    (expect (boundary-verdict host allowed-root ".git/config") :to-be :git-directory)
    (expect (boundary-verdict host allowed-root "../../objects/pack/new.idx")
            :to-be :outside-root)))
  (it "blocks roots and temporary writes when an inner repository masks its bare common directory"
  (let* ((host
           (make-fake-workspace-boundaries
            :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                         (cons "/bare/objects/pack/.git"
                               "gitdir: /other/worktrees/wt")
                         (cons "/bare/.worktrees/wt/.git"
                               "gitdir: /other/worktrees/wt")
                         (cons "/other/worktrees/wt/HEAD" "0123"))
            :directories (list "/bare/objects"
                               "/bare/refs"
                               "/bare/objects/pack"
                               "/bare/.worktrees/wt"
                               "/bare/.worktrees/wt/src"
                               "/other/worktrees/wt")
            :cwd "/outside"))
         (metadata-root (resolved-root host :root "/bare/objects/pack"))
         (allowed-root (resolved-root host :root "/bare/.worktrees/wt"))
         (repository (workspace-root-repository metadata-root))
         (temporary-root "/bare/state/aitools/tmp")
         (state-root "/bare/state/aitools"))
    (expect (git-repository-common-dir repository)
            :to-equal "/other/worktrees/wt")
    (expect (boundary-verdict host metadata-root "new.idx")
            :to-be :git-directory)
    (expect (boundary-verdict host allowed-root "src/new.txt") :to-be :inside)
    (expect (boundary-verdict host allowed-root ".git/config")
            :to-be :git-directory)
    (expect (boundary-verdict host allowed-root
                              "../../state/aitools/tmp/file"
                              :temporary-root temporary-root
                              :state-root state-root)
            :to-be :git-directory)))

  (it "allows checkouts in normal common-directory siblings"
      (dolist (checkout (list "/bare/.worktrees/wt"
                              "/bare/checkouts/wt"
                              "/bare/objects-old/wt"
                              "/BARE/.WORKTREES/wt"
                              "/bare/workspace.lock/wt"))
        (let* ((host
                (make-fake-workspace-boundaries
                 :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                              (cons "/bare/worktrees/wt/HEAD" "0123")
                              (cons "/bare/worktrees/wt/commondir" "../..")
                              (cons (format nil "~A/.git" checkout)
                                    "gitdir: /bare/worktrees/wt"))
                 :directories (list (format nil "~A/src" checkout))
                 :cwd checkout))
               (root (resolved-root host))
               (repository (workspace-root-repository root)))
          (expect (git-repository-common-dir repository) :to-equal "/bare")
          (expect (boundary-verdict host root "src/new.txt") :to-be :inside)
          (expect (boundary-verdict host root ".git/config") :to-be :git-directory)
          (expect (boundary-verdict host root "../../worktrees/wt/index.lock")
                  :to-be :outside-root)))))

