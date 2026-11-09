(in-package #:aitools.workspace.test)

(describe "aitools.workspace.application common metadata write boundary"
          (it "refuses writes when the common directory is the workspace root"
              (let* ((host
                      (make-fake-workspace-boundaries :files
                                                      (list
                                                       (cons "/bare/.git"
                                                             "gitdir: /meta/worktrees/wt")
                                                       (cons
                                                        "/meta/worktrees/wt/HEAD"
                                                        "0123")
                                                       (cons
                                                        "/meta/worktrees/wt/commondir"
                                                        "/bare"))
                                                      :directories
                                                      (list "/bare/src")
                                                      :cwd
                                                      "/bare"))
                     (root (resolved-root host :root "/bare"))
                     (repository (workspace-root-repository root)))
                (expect (workspace-root-real root) :to-equal "/bare")
                (expect (git-repository-git-dir repository)
                        :to-equal
                        "/meta/worktrees/wt")
                (expect (git-repository-common-dir repository)
                        :to-equal
                        "/bare")
                (expect (boundary-verdict host root "src/new.txt")
                        :to-be
                        :git-directory)))
          (it
           "refuses writes when ROOT and common dir are Unicode-normalization aliases"
           (let* ((nfc-root
                   (concatenate (quote string)
                                "/bare/Caf"
                                (string (code-char #xE9))
                                "Repo"))
                  (nfd-root
                   (concatenate (quote string)
                                "/bare/Cafe"
                                (string (code-char #x301))
                                "Repo"))
                  (git-directory "/meta/worktrees/wt")
                  (host
                   (make-fake-workspace-boundaries :files
                                                   (list
                                                    (cons
                                                     (concatenate (quote string)
                                                                  nfd-root
                                                                  "/.git")
                                                     "gitdir: /meta/worktrees/wt")
                                                    (cons
                                                     (concatenate (quote string)
                                                                  git-directory
                                                                  "/HEAD")
                                                     "0123")
                                                    (cons
                                                     (concatenate (quote string)
                                                                  git-directory
                                                                  "/commondir")
                                                     nfc-root))
                                                   :directories
                                                   (list
                                                    (concatenate (quote string)
                                                                 nfd-root
                                                                 "/src"))
                                                   :cwd
                                                   nfd-root))
                  (root (resolved-root host :root nfd-root))
                  (repository (workspace-root-repository root)))
             (expect (workspace-root-real root) :to-equal nfd-root)
             (expect (git-repository-common-dir repository) :to-equal nfc-root)
             (expect (git-metadata-path-inside-p nfc-root nfd-root)
                     :to-be-truthy)
             (expect (git-metadata-path-inside-p nfd-root nfc-root)
                     :to-be-truthy)
             (expect (boundary-verdict host root "src/new.txt")
                     :to-be
                     :git-directory)))
          (it "refuses metadata reached through canonical Unicode aliases"
              (let* ((nfc-common
                      (concatenate (quote string)
                                   "/bare/Caf"
                                   (string (code-char #xE9))
                                   "Repo"))
                     (nfd-common
                      (concatenate (quote string)
                                   "/bare/Cafe"
                                   (string (code-char #x301))
                                   "Repo"))
                     (git-directory
                      (concatenate (quote string) nfc-common "/worktrees/wt"))
                     (checkout
                      (concatenate (quote string) nfd-common "/objects/wt"))
                     (host
                      (make-fake-workspace-boundaries :files
                                                      (list
                                                       (cons
                                                        (concatenate
                                                         (quote string)
                                                         git-directory
                                                         "/HEAD")
                                                        "0123")
                                                       (cons
                                                        (concatenate
                                                         (quote string)
                                                         git-directory
                                                         "/commondir")
                                                        "../..")
                                                       (cons
                                                        (concatenate
                                                         (quote string)
                                                         checkout
                                                         "/.git")
                                                        (format nil
                                                                "gitdir: ~A"
                                                                git-directory)))
                                                      :directories
                                                      (list
                                                       (concatenate
                                                        (quote string)
                                                        checkout
                                                        "/src")
                                                       (concatenate
                                                        (quote string)
                                                        nfc-common
                                                        "/objects/wt/src"))
                                                      :cwd
                                                      checkout))
                     (root (resolved-root host))
                     (repository (workspace-root-repository root)))
                (expect (workspace-root-real root) :to-equal checkout)
                (expect (git-repository-common-dir repository)
                        :to-equal
                        nfc-common)
                (expect (boundary-verdict host root "src/new.txt")
                        :to-be
                        :git-directory)
                (expect (git-metadata-path-inside-p nfc-common checkout)
                        :to-be-truthy)
                (expect
                 (git-metadata-path-inside-p
                  (concatenate (quote string) nfc-common "/objects")
                  checkout)
                 :to-be-truthy)))
          (it "refuses temporary targets inside Git metadata"
              (expect
               (aitools.workspace.domain:write-target-verdict :root
                                                              "/repo"
                                                              :real-root
                                                              "/repo"
                                                              :target
                                                              ".git/aitools/workspace/tmp/file"
                                                              :real-target
                                                              "/repo/.git/aitools/workspace/tmp/file"
                                                              :temporary-root
                                                              "/repo/.git/aitools/workspace/tmp"
                                                              :real-state-root
                                                              "/repo/.git/aitools"
                                                              :real-git-directories
                                                              '("/repo/.git"))
               :to-be
               :git-directory))
          (it "refuses ROOT symlinks into the linked-worktree git dir"
              (let* ((checkout "/bare/.worktrees/wt")
                     (git-dir "/bare/worktrees/wt")
                     (host
                      (make-fake-workspace-boundaries :directories
                                                      (list
                                                       "/bare/worktrees/wt/src")
                                                      :symlinks
                                                      (list
                                                       (cons checkout git-dir))
                                                      :cwd
                                                      checkout))
                     (root
                      (aitools.workspace.domain:make-workspace-root :path
                                                                    checkout
                                                                    :real
                                                                    git-dir
                                                                    :source
                                                                    :option
                                                                    :repository
                                                                    (aitools.workspace.domain:make-git-repository
                                                                     :top
                                                                     checkout
                                                                     :git-dir
                                                                     git-dir
                                                                     :common-dir
                                                                     "/bare"))))
                (expect (boundary-verdict host root "src/new.txt")
                        :to-be
                        :git-directory)))
          (it "applies common-root metadata checks to temporary targets"
              (dolist
                  (case (list (cons "remotes" :git-directory)
                              (cons "aitools" :git-directory)
                              (cons "state" :git-directory)))
                (let* ((entry (car case))
                       (expected (cdr case))
                       (host
                        (make-fake-workspace-boundaries :files
                                                        (list
                                                         (cons "/bare/HEAD"
                                                               "ref: refs/heads/main")
                                                         (cons
                                                          "/bare/worktrees/wt/HEAD"
                                                          "0123")
                                                         (cons
                                                          "/bare/worktrees/wt/commondir"
                                                          "../..")
                                                         (cons
                                                          "/bare/.worktrees/wt/.git"
                                                          "gitdir: /bare/worktrees/wt"))
                                                        :directories
                                                        (list
                                                         "/bare/.worktrees/wt/src")
                                                        :cwd
                                                        "/bare/.worktrees/wt"))
                       (root (resolved-root host))
                       (temporary-root
                        (format nil "/bare/~A/aitools/tmp" entry))
                       (state-root (format nil "/bare/~A/aitools" entry))
                       (target (format nil "../../~A/aitools/tmp/file" entry)))
                  (expect
                   (boundary-verdict host
                                     root
                                     target
                                     :temporary-root
                                     temporary-root
                                     :state-root
                                     state-root)
                   :to-be
                   expected)))))
