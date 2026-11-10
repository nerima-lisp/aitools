(in-package #:aitools.workspace.test)

(describe "aitools.workspace.application Git metadata write boundary"
          (it "refuses roots beneath Git repository metadata entries"
              (dolist
                  (case (append aitools.data:*git-common-root-metadata-names*
                                (list
                                 (format nil
                                         "sharedindex.~A"
                                         (make-string 40 :initial-element #\a))
                                 (format nil
                                         "sharedindex.~A"
                                         (make-string 64 :initial-element #\a))
                                 "FETCH_HEAD"
                                 "MERGE_HEAD"
                                 "ORIG_HEAD"
                                 "CHERRY_PICK_HEAD"
                                 "REVERT_HEAD"
                                 "REBASE_HEAD"
                                 "BISECT_HEAD"
                                 "CUSTOM_HEAD")
                                (list (cons "config.lock" :git-directory)
                                      (cons "config.worktree.lock"
                                            :git-directory)
                                      (cons "gc.pid.lock" :git-directory)
                                      (cons "index.lock" :git-directory)
                                      (cons "packed-refs.lock" :git-directory)
                                      (cons "shallow.lock" :git-directory)
                                      (cons "HEAD.lock" :git-directory)
                                      (cons "ORIG_HEAD.lock" :git-directory)
                                      (cons "CUSTOM_HEAD.lock" :git-directory)
                                      (cons "AUTO_MERGE.lock" :git-directory)
                                      (cons "BISECT_EXPECTED_REV.lock"
                                            :git-directory)
                                      (cons "NOTES_MERGE_PARTIAL.lock"
                                            :git-directory)
                                      (cons "NOTES_MERGE_REF.lock"
                                            :git-directory)
                                      (cons "MERGE_AUTOSTASH.lock"
                                            :git-directory)
                                      (cons "objects.lock" :inside)
                                      (cons "refs.lock" :inside)
                                      (cons "FETCH_HEAD.lock" :inside)
                                      (cons "MERGE_HEAD.lock" :inside)
                                      (cons "sharedindex.abcdef.lock" :inside)
                                      (cons "sharedindex.notes" :inside)
                                      (cons "RELEASE2_HEAD" :inside))))
                (let* ((entry
                        (if (stringp case)
                            case
                            (car case)))
                       (expected
                        (if (stringp case)
                            :git-directory
                            (cdr case)))
                       (checkout (format nil "/bare/~A/checkout" entry))
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
                                                          (format nil
                                                                  "~A/.git"
                                                                  checkout)
                                                          "gitdir: /bare/worktrees/wt"))
                                                        :directories
                                                        (list
                                                         (format nil
                                                                 "~A/src"
                                                                 checkout))
                                                        :cwd
                                                        checkout))
                       (root (resolved-root host :root checkout))
                       (repository (workspace-root-repository root)))
                  (expect (git-repository-common-dir repository)
                          :to-equal
                          "/bare")
                  (expect (git-repository-git-dir repository)
                          :to-equal
                          "/bare/worktrees/wt")
                  (expect (boundary-verdict host root "src/new.txt")
                          :to-be
                          expected))))
          (it "protects common-directory metadata reached through symlinks"
              (let* ((host
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
                                                        "/bare/objects/wt/.git"
                                                        "gitdir: /bare/worktrees/wt"))
                                                      :directories
                                                      (list
                                                       "/bare/objects/wt/src"
                                                       "/external/objects/wt/src")
                                                      :symlinks
                                                      (list
                                                       (cons "/bare/objects"
                                                             "/external/objects"))
                                                      :cwd
                                                      "/bare/objects/wt"))
                     (root (resolved-root host))
                     (repository (workspace-root-repository root)))
                (expect (workspace-root-real root)
                        :to-equal
                        "/external/objects/wt")
                (expect (git-repository-common-dir repository)
                        :to-equal
                        "/bare")
                (expect (boundary-verdict host root "src/new.txt")
                        :to-be
                        :git-directory)))
          (it "refuses case-variant roots under Git metadata"
              (let* ((host
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
                                                        "/BARE/OBJECTS/wt/.git"
                                                        "gitdir: /bare/worktrees/wt"))
                                                      :directories
                                                      (list
                                                       "/BARE/OBJECTS/wt/src"
                                                       "/bare/objects/wt/src")
                                                      :cwd
                                                      "/BARE/OBJECTS/wt"))
                     (root (resolved-root host))
                     (repository (workspace-root-repository root)))
                (expect (workspace-root-real root) :to-equal "/BARE/OBJECTS/wt")
                (expect (git-repository-git-dir repository)
                        :to-equal
                        "/bare/worktrees/wt")
                (expect (git-repository-common-dir repository)
                        :to-equal
                        "/bare")
                (expect (boundary-verdict host root "src/new.txt")
                        :to-be
                        :git-directory)))
          (it "allows a sibling with case-variant Git paths"
              (let* ((checkout "/BARE/.WORKTREES/wt")
                     (host
                      (make-fake-workspace-boundaries :files
                                                      (list
                                                       (cons "/BARE/HEAD"
                                                             "ref: refs/heads/main")
                                                       (cons
                                                        "/BARE/WORKTREES/wt/HEAD"
                                                        "0123")
                                                       (cons
                                                        "/BARE/WORKTREES/wt/commondir"
                                                        "../..")
                                                       (cons
                                                        (format nil
                                                                "~A/.git"
                                                                checkout)
                                                        "gitdir: /BARE/WORKTREES/wt"))
                                                      :directories
                                                      (list
                                                       (format nil
                                                               "~A/src"
                                                               checkout))
                                                      :cwd
                                                      checkout))
                     (root (resolved-root host))
                     (repository (workspace-root-repository root)))
                (expect (git-repository-git-dir repository)
                        :to-equal
                        "/BARE/WORKTREES/wt")
                (expect (git-repository-common-dir repository)
                        :to-equal
                        "/BARE")
                (expect (boundary-verdict host root "src/new.txt")
                        :to-be
                        :inside)))
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
             (expect (git-metadata-path-inside-p nfc-root nfd-root :name-fold-function (function aitools.workspace.application::%git-name-fold))
                     :to-be-truthy)
             (expect (git-metadata-path-inside-p nfd-root nfc-root :name-fold-function (function aitools.workspace.application::%git-name-fold))
                     :to-be-truthy)
             (expect (boundary-verdict host root "src/new.txt")
                     :to-be
                     :git-directory))))
