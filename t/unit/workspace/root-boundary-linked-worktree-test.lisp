(in-package #:aitools.workspace.test)

(describe "aitools.workspace.application linked-worktree write boundary"
  (it "allows a registered common-directory child outside metadata and guards Git paths"
    (let* ((host
             (make-fake-workspace-boundaries
              :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                           (cons "/bare/worktrees/wt/HEAD" "0123")
                           (cons "/bare/worktrees/wt/commondir" "../..")
                           (cons "/bare/worktrees/wt/gitdir"
                                 "/bare/.worktrees/wt/.git")
                           (cons "/bare/.worktrees/wt/.git"
                                 "gitdir: /bare/worktrees/wt"))
              :directories (list "/bare/objects"
                                 "/bare/refs"
                                 "/bare/.worktrees/wt/src")
              :symlinks (list (cons "/bare/.worktrees/wt/git-dir"
                                    "/bare/worktrees/wt"))
              :cwd "/bare/.worktrees/wt"))
           (root (resolved-root host))
           (repository (workspace-root-repository root)))
      (expect (git-repository-top repository) :to-equal "/bare/.worktrees/wt")
      (expect (git-repository-git-dir repository) :to-equal "/bare/worktrees/wt")
      (expect (git-repository-common-dir repository) :to-equal "/bare")
      (expect (boundary-verdict host root "src/new.txt") :to-be :inside)
      (expect (boundary-verdict host root ".git/config") :to-be :git-directory)
      (expect (boundary-verdict host root "git-dir/index.lock")
              :to-be :symlink-escape)))
  (it "refuses unregistered worktree-shaped directories under a common directory"
    (let* ((host
             (make-fake-workspace-boundaries
              :files (list (cons "/bare/HEAD" "ref: refs/heads/main"))
              :directories (list "/bare/objects"
                                 "/bare/refs"
                                 "/bare/objects/pack"
                                 "/bare/.worktrees/notaworktree/src")
              :cwd "/outside"))
           (metadata-root (resolved-root host :root "/bare/objects/pack"))
           (unregistered-root
             (resolved-root host :root "/bare/.worktrees/notaworktree"))
           (repository (workspace-root-repository metadata-root)))
      (expect (git-repository-top repository) :to-equal "/bare")
      (expect (git-repository-git-dir repository) :to-equal "/bare")
      (expect (git-repository-common-dir repository) :to-equal "/bare")
      (expect (boundary-verdict host metadata-root "new.idx")
              :to-be :git-directory)
      (expect (boundary-verdict host unregistered-root "src/new.txt")
              :to-be :git-directory)
      (expect (boundary-verdict host unregistered-root ".git/config")
              :to-be :git-directory)
      (expect (boundary-verdict host unregistered-root "../../objects/pack/new.idx")
              :to-be :outside-root)
      (let* ((worktree-root "/bare/.worktrees/notaworktree")
             (git-directory "/bare/worktrees/notaworktree")
             (worktree-host
               (make-fake-workspace-boundaries
                :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                             (cons (concatenate (quote string) worktree-root "/.git")
                                   "gitdir: /bare/worktrees/notaworktree")
                             (cons (concatenate (quote string) git-directory "/HEAD") "0123")
                             (cons (concatenate (quote string) git-directory "/commondir")
                                   "../.."))
                :directories (list "/bare/objects"
                                   "/bare/refs"
                                   (concatenate (quote string) worktree-root "/src"))
                :cwd worktree-root))
             (worktree-root-value
               (resolved-root worktree-host :root worktree-root)))
        (expect (git-repository-git-dir
                 (workspace-root-repository worktree-root-value))
                :to-equal git-directory)
        (expect (boundary-verdict worktree-host worktree-root-value "src/new.txt")
                :to-be :git-directory))))
  (it "blocks roots and temporary writes when an inner repository masks its bare common directory"
    (let* ((host
             (make-fake-workspace-boundaries
              :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                           (cons "/bare/objects/pack/.git"
                                 "gitdir: /other/worktrees/wt")
                           (cons "/bare/.worktrees/wt/.git"
                                 "gitdir: /bare/worktrees/wt")
                           (cons "/bare/worktrees/wt/HEAD" "0123")
                           (cons "/bare/worktrees/wt/commondir" "../..")
                           (cons "/bare/worktrees/wt/gitdir"
                                 "/bare/.worktrees/wt/.git")
                           (cons "/other/worktrees/wt/HEAD" "0123"))
              :directories (list "/bare/objects"
                                 "/bare/refs"
                                 "/bare/objects/pack"
                                 "/bare/.worktrees/wt"
                                 "/bare/.worktrees/wt/src"
                                 "/bare/worktrees/wt"
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
      (expect (boundary-verdict host allowed-root "src/new.txt")
              :to-be :inside)
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
                              "/bare/.WORKTREES/wt"
                              "/bare/workspace.lock/wt"))
        (let* ((host
                (make-fake-workspace-boundaries
                 :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                              (cons "/bare/worktrees/wt/HEAD" "0123")
                              (cons "/bare/worktrees/wt/commondir" "../..")
                              (cons "/bare/worktrees/wt/gitdir"
                                    (format nil "~A/.git" checkout))
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
                  :to-be :outside-root))))
  (it "refuses writes inside an ordinary repository git directory"
    (let* ((host
             (make-fake-workspace-boundaries
              :files (list (cons "/repo/.git/HEAD" "ref: refs/heads/main"))
              :directories (list "/repo/.git/objects"
                                 "/repo/.git/refs"
                                 "/repo/.git/rebase-merge")
              :cwd "/outside"))
           (root (resolved-root host :root "/repo/.git/rebase-merge"))
           (repository (workspace-root-repository root)))
      (expect (git-repository-git-dir repository) :to-equal "/repo/.git")
      (expect (git-repository-common-dir repository) :to-equal "/repo/.git")
      (expect (boundary-verdict host root "git-rebase-todo")
              :to-be :git-directory)))
  (it "refuses a registered worktree-shaped root under an ordinary .git directory"
    (let* ((root-path "/repo/.git/.worktrees/wt")
           (host
             (make-fake-workspace-boundaries
              :files (list (cons "/repo/.git/HEAD" "ref: refs/heads/main")
                           (cons "/repo/.git/worktrees/wt/HEAD" "0123")
                           (cons "/repo/.git/worktrees/wt/commondir" "../..")
                           (cons "/repo/.git/worktrees/wt/gitdir"
                                 "/repo/.git/.worktrees/wt/.git")
                           (cons "/repo/.git/.worktrees/wt/.git"
                                 "gitdir: /repo/.git/worktrees/wt"))
              :directories (list "/repo/.git/objects"
                                 "/repo/.git/refs"
                                 "/repo/.git/.worktrees/wt/src")
              :cwd "/outside"))
           (root (resolved-root host :root root-path)))
      (expect (git-repository-common-dir (workspace-root-repository root))
              :to-equal "/repo/.git")
      (expect (boundary-verdict host root "src/new.txt")
              :to-be :git-directory)
      (let* ((alias-host
               (make-fake-workspace-boundaries
                :directories (list (concatenate (quote string) root-path "/src"))
                :symlinks (list (cons "/workspace" root-path))
                :cwd "/"))
             (alias-root (resolved-root alias-host :root "/workspace")))
        (expect (workspace-root-real alias-root) :to-equal root-path)
        (expect (boundary-verdict alias-host alias-root "src/new.txt")
                :to-be :git-directory)))))
(describe "aitools.workspace.application metadata-root verdicts"
          (it "classifies metadata names, identities, and registered worktrees"
              (let* ((safe-host (make-fake-workspace-boundaries :directories (list "/bare/src" "/checkout")))
                     (aliased-host
                      (make-fake-workspace-boundaries
                       :directories (list "/bare/hooks" "/checkout")
                       :file-identities
                       (list (cons "/bare/hooks" (cons 1 2))
                             (cons "/checkout" (cons 1 2)))))
                     (incomplete-host (make-fake-workspace-boundaries :directories (list "/bare/hooks" "/checkout") :stat-failures (list "/bare/hooks")))
                     (loop-host
                      (make-fake-workspace-boundaries
                       :symlinks (list (cons "/loop" "/loop"))))
                     (worktree-root "/bare/.worktrees/wt")
                     (worktree
                      (aitools.workspace.domain:make-git-repository
                       :top worktree-root
                       :git-dir "/bare/worktrees/wt"
                       :common-dir "/bare"))
                     (bare
                      (aitools.workspace.domain:make-git-repository
                       :top "/bare" :git-dir "/bare" :common-dir "/bare"))
                     (repositories (list bare worktree))
                     (linked-host
                      (make-fake-workspace-boundaries
                       :files
                       (list (cons "/bare/worktrees/wt/gitdir"
                                   (concatenate 'string worktree-root "/.git"))
                             (cons (concatenate 'string worktree-root "/.git")
                                   "gitdir: /bare/worktrees/wt")))))
                (expect (aitools.workspace.application::%common-root-name-verdict
                         "/bare" "/bare" "/bare/hooks" "/bare/hooks" nil nil nil)
                        :to-be :git-directory)
                (expect (aitools.workspace.application::%common-root-name-verdict
                         "/bare" "/bare" "/checkout" "/checkout" nil nil nil)
                        :to-be-falsy)
                (expect (aitools.workspace.application::%common-root-name-verdict
                         "/bare" "/bare" "/checkout" "/checkout"
                         "/bare/tmp" "/bare/tmp" "/bare/tmp/file")
                        :to-be :git-directory)
                (expect (aitools.workspace.application::%common-root-identity-verdict
                         safe-host "/bare" "/checkout" "/checkout" nil nil nil)
                        :to-be-falsy)
                (expect (aitools.workspace.application::%common-root-identity-verdict
                         aliased-host "/bare" "/checkout" "/checkout" nil nil nil)
                        :to-be :git-directory)
                (expect (aitools.workspace.application::%common-root-identity-verdict
                         incomplete-host "/bare" "/checkout" "/checkout" nil nil nil)
                        :to-be :git-directory)
                (expect (aitools.workspace.application::%registered-linked-worktree-candidate-verdict
                         linked-host "/bare" worktree-root worktree)
                        :to-be :registered-linked-worktree)
                (expect (aitools.workspace.application::%registered-linked-worktree-candidate-verdict
                         linked-host "/bare" "/bare/other" worktree)
                        :to-be :unregistered-linked-worktree)
                (expect (aitools.workspace.application::%linked-worktree-exception-verdict
                         linked-host "/bare" worktree-root "/bare" repositories)
                        :to-be :omit-common-directory)
                (expect (aitools.workspace.application::%linked-worktree-exception-verdict
                         linked-host "/bare" "/bare/other" "/bare" repositories)
                        :to-be :retain-common-directory)
                (let ((unresolved
                       (aitools.workspace.domain:make-git-repository
                        :top "/checkout" :git-dir "/meta/wt" :common-dir "/loop")))
                  (expect (aitools.workspace.application::%common-root-metadata-verdict
                           loop-host unresolved "/checkout" "/checkout" nil nil nil)
                          :to-be :git-directory)))))
