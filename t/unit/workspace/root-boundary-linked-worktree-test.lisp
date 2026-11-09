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
                  :to-be :outside-root))))

  (it "refuses roots beneath Git repository metadata entries"
    (dolist (case
             (append
              aitools.data:*git-common-root-metadata-names*
              (list (format nil "sharedindex.~A" (make-string 40 :initial-element #\a))
                    (format nil "sharedindex.~A" (make-string 64 :initial-element #\a))
                    "FETCH_HEAD" "MERGE_HEAD" "ORIG_HEAD" "CHERRY_PICK_HEAD"
                    "REVERT_HEAD" "REBASE_HEAD" "BISECT_HEAD" "CUSTOM_HEAD")
              (list (cons "config.lock" :git-directory)
                    (cons "config.worktree.lock" :git-directory)
                    (cons "gc.pid.lock" :git-directory)
                    (cons "index.lock" :git-directory)
                    (cons "packed-refs.lock" :git-directory)
                    (cons "shallow.lock" :git-directory)
                    (cons "HEAD.lock" :git-directory)
                    (cons "ORIG_HEAD.lock" :git-directory)
                    (cons "CUSTOM_HEAD.lock" :git-directory)
                    (cons "AUTO_MERGE.lock" :git-directory)
                    (cons "BISECT_EXPECTED_REV.lock" :git-directory)
                    (cons "NOTES_MERGE_PARTIAL.lock" :git-directory)
                    (cons "NOTES_MERGE_REF.lock" :git-directory)
                    (cons "MERGE_AUTOSTASH.lock" :git-directory)
                    (cons "objects.lock" :inside)
                    (cons "refs.lock" :inside)
                    (cons "FETCH_HEAD.lock" :inside)
                    (cons "MERGE_HEAD.lock" :inside)
                    (cons "sharedindex.abcdef.lock" :inside)
                    (cons "sharedindex.notes" :inside)
                    (cons "RELEASE2_HEAD" :inside))))
      (let* ((entry (if (stringp case) case (car case)))
             (expected (if (stringp case) :git-directory (cdr case)))
             (checkout (format nil "/bare/~A/checkout" entry))
             (host
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
        (expect (git-repository-git-dir repository) :to-equal "/bare/worktrees/wt")
        (expect (boundary-verdict host root "src/new.txt") :to-be expected))))

  (it "protects common-directory metadata reached through symlinks"
      (let* ((host
               (make-fake-workspace-boundaries
                :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                             (cons "/bare/worktrees/wt/HEAD" "0123")
                             (cons "/bare/worktrees/wt/commondir" "../..")
                             (cons "/bare/objects/wt/.git"
                                   "gitdir: /bare/worktrees/wt"))
                :directories (list "/bare/objects/wt/src"
                                   "/external/objects/wt/src")
                :symlinks (list (cons "/bare/objects" "/external/objects"))
                :cwd "/bare/objects/wt"))
             (root (resolved-root host))
             (repository (workspace-root-repository root)))
        (expect (workspace-root-real root) :to-equal "/external/objects/wt")
        (expect (git-repository-common-dir repository) :to-equal "/bare")
        (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory)))

  (it "refuses case-variant roots under Git metadata"
      (let* ((host
               (make-fake-workspace-boundaries
                :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                             (cons "/bare/worktrees/wt/HEAD" "0123")
                             (cons "/bare/worktrees/wt/commondir" "../..")
                             (cons "/BARE/OBJECTS/wt/.git"
                                   "gitdir: /bare/worktrees/wt"))
                :directories (list "/BARE/OBJECTS/wt/src"
                                   "/bare/objects/wt/src")
                :cwd "/BARE/OBJECTS/wt"))
             (root (resolved-root host))
             (repository (workspace-root-repository root)))
        (expect (workspace-root-real root) :to-equal "/BARE/OBJECTS/wt")
        (expect (git-repository-git-dir repository) :to-equal "/bare/worktrees/wt")
        (expect (git-repository-common-dir repository) :to-equal "/bare")
        (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory)))

  (it "allows a sibling with case-variant Git paths"
      (let* ((checkout "/bare/.worktrees/wt")
             (host
               (make-fake-workspace-boundaries
                :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                             (cons "/BARE/worktrees/wt/HEAD" "0123")
                             (cons "/BARE/worktrees/wt/commondir" "../..")
                             (cons (format nil "~A/.git" checkout)
                                   "gitdir: /BARE/worktrees/wt"))
                :directories (list (format nil "~A/src" checkout))
                :cwd checkout))
             (root (resolved-root host))
             (repository (workspace-root-repository root)))
        (expect (git-repository-git-dir repository) :to-equal "/BARE/worktrees/wt")
        (expect (git-repository-common-dir repository) :to-equal "/BARE")
        (expect (boundary-verdict host root "src/new.txt") :to-be :inside)))

  (it "refuses writes when the common directory is the workspace root"
      (let* ((host
               (make-fake-workspace-boundaries
                :files (list (cons "/bare/.git" "gitdir: /meta/worktrees/wt")
                             (cons "/meta/worktrees/wt/HEAD" "0123")
                             (cons "/meta/worktrees/wt/commondir" "/bare"))
                :directories (list "/bare/src")
                :cwd "/bare"))
             (root (resolved-root host :root "/bare"))
             (repository (workspace-root-repository root)))
        (expect (workspace-root-real root) :to-equal "/bare")
        (expect (git-repository-git-dir repository) :to-equal "/meta/worktrees/wt")
        (expect (git-repository-common-dir repository) :to-equal "/bare")
        (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory)))

  (it "refuses writes when ROOT and common dir are Unicode-normalization aliases"
    (let* ((nfc-root (concatenate (quote string) "/bare/Caf" (string (code-char #xE9)) "Repo"))
           (nfd-root (concatenate (quote string) "/bare/Cafe" (string (code-char #x301)) "Repo"))
           (git-directory "/meta/worktrees/wt")
           (host
             (make-fake-workspace-boundaries
              :files (list (cons (concatenate (quote string) nfd-root "/.git")
                                 "gitdir: /meta/worktrees/wt")
                           (cons (concatenate (quote string) git-directory "/HEAD") "0123")
                           (cons (concatenate (quote string) git-directory "/commondir")
                                 nfc-root))
              :directories (list (concatenate (quote string) nfd-root "/src"))
              :cwd nfd-root))
           (root (resolved-root host :root nfd-root))
           (repository (workspace-root-repository root)))
      (expect (workspace-root-real root) :to-equal nfd-root)
      (expect (git-repository-common-dir repository) :to-equal nfc-root)
      (expect (git-metadata-path-inside-p nfc-root nfd-root) :to-be-truthy)
      (expect (git-metadata-path-inside-p nfd-root nfc-root) :to-be-truthy)
      (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory)))

  (it "refuses metadata reached through canonical Unicode aliases"
      (let* ((nfc-common (concatenate (quote string) "/bare/Caf" (string (code-char #xE9)) "Repo"))
             (nfd-common (concatenate (quote string) "/bare/Cafe" (string (code-char #x301)) "Repo"))
             (git-directory (concatenate (quote string) nfc-common "/worktrees/wt"))
             (checkout (concatenate (quote string) nfd-common "/objects/wt"))
             (host
               (make-fake-workspace-boundaries
                :files (list (cons (concatenate (quote string) git-directory "/HEAD") "0123")
                             (cons (concatenate (quote string) git-directory "/commondir") "../..")
                             (cons (concatenate (quote string) checkout "/.git")
                                   (format nil "gitdir: ~A" git-directory)))
                :directories (list (concatenate (quote string) checkout "/src")
                                   (concatenate (quote string) nfc-common "/objects/wt/src"))
                :cwd checkout))
             (root (resolved-root host))
             (repository (workspace-root-repository root)))
        (expect (workspace-root-real root) :to-equal checkout)
        (expect (git-repository-common-dir repository) :to-equal nfc-common)
        (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory)
        (expect (git-metadata-path-inside-p nfc-common checkout) :to-be-truthy)
        (expect (git-metadata-path-inside-p
                 (concatenate (quote string) nfc-common "/objects")
                 checkout)
                :to-be-truthy)))
  (it "refuses temporary targets inside Git metadata"
      (expect
       (aitools.workspace.domain:write-target-verdict
        :root "/repo"
        :real-root "/repo"
        :target ".git/aitools/workspace/tmp/file"
        :real-target "/repo/.git/aitools/workspace/tmp/file"
        :temporary-root "/repo/.git/aitools/workspace/tmp"
        :real-state-root "/repo/.git/aitools"
        :real-git-directories '("/repo/.git"))
       :to-be :git-directory))
  (it "refuses ROOT symlinks into the linked-worktree git dir"
    (let* ((checkout "/bare/.worktrees/wt") (git-dir "/bare/worktrees/wt")
           (host (make-fake-workspace-boundaries
                  :directories (list "/bare/worktrees/wt/src")
                  :symlinks (list (cons checkout git-dir)) :cwd checkout))
           (root (aitools.workspace.domain:make-workspace-root
                  :path checkout :real git-dir :source :option
                  :repository
                  (aitools.workspace.domain:make-git-repository
                   :top checkout :git-dir git-dir :common-dir "/bare"))))
      (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory)))
  (it "applies common-root metadata checks to temporary targets"
    (dolist (case (list (cons "remotes" :git-directory) (cons "aitools" :git-directory)
                         (cons "state" :git-directory)))
      (let* ((entry (car case))
             (expected (cdr case))
             (host
               (make-fake-workspace-boundaries
                :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                             (cons "/bare/worktrees/wt/HEAD" "0123")
                             (cons "/bare/worktrees/wt/commondir" "../..")
                             (cons "/bare/.worktrees/wt/.git"
                                   "gitdir: /bare/worktrees/wt"))
                :directories (list "/bare/.worktrees/wt/src")
                :cwd "/bare/.worktrees/wt"))
             (root (resolved-root host))
             (temporary-root (format nil "/bare/~A/aitools/tmp" entry))
             (state-root (format nil "/bare/~A/aitools" entry))
             (target (format nil "../../~A/aitools/tmp/file" entry)))
        (expect (boundary-verdict host root target
                                  :temporary-root temporary-root
                                  :state-root state-root)
                :to-be expected)))))

