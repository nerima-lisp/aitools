(in-package #:aitools.workspace.test)
(defun %git-metadata-alias-verdict (name target)
  (let* ((root-path (format nil "/bare/~A" name))
         (host (make-fake-workspace-boundaries))
         (root (aitools.workspace.domain:make-workspace-root
                :path root-path
                :real root-path
                :source :option
                :repository
                (aitools.workspace.domain:make-git-repository
                 :top root-path
                 :git-dir "/bare/worktrees/wt"
                 :common-dir "/bare"))))
    (boundary-verdict host root target)))
(describe "aitools.workspace.application Unicode Git metadata aliases"
  (it "refuses the long-s spelling of hooks"
    (expect (%git-metadata-alias-verdict "hookſ" "pre-commit")
            :to-be :git-directory))
  (it "refuses writes through common-root metadata symlink aliases"
      (let* ((root-path "/bare/work-root")
             (shared-target "/external/shared-hooks")
             (host
              (make-fake-workspace-boundaries
               :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                            (cons "/external/shared-hooks/pre-commit" "existing"))
               :directories (list "/bare/objects" "/bare/refs")
               :symlinks (list (cons "/bare/hooks" shared-target)
                               (cons root-path shared-target))
               :cwd "/outside"))
             (root (resolved-root host :root root-path)))
        (expect (workspace-root-real root) :to-equal shared-target)
        (expect (boundary-verdict host root "pre-commit")
                :to-be :git-directory)))
  (it "refuses a linked worktree when common metadata aliases a nested root"
      (let* ((checkout "/bare/.worktrees/wt")
             (host
              (make-fake-workspace-boundaries
               :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                            (cons "/bare/worktrees/wt/HEAD" "0123")
                            (cons "/bare/worktrees/wt/commondir" "../..")
                            (cons "/bare/worktrees/wt/gitdir"
                                  "/bare/.worktrees/wt/.git")
                            (cons "/bare/.worktrees/wt/.git"
                                  "gitdir: /bare/worktrees/wt")
                            (cons "/bare/.worktrees/wt/pre-commit" "existing"))
               :directories (list "/bare/objects" "/bare/refs")
               :symlinks (list (cons "/bare/hooks" checkout))
               :cwd checkout))
             (root (resolved-root host)))
        (expect (git-repository-common-dir (workspace-root-repository root))
                :to-equal "/bare")
        (expect (boundary-verdict host root "pre-commit")
                :to-be :git-directory)))
  (it "refuses a registered worktree when common metadata aliases its root"
      (let* ((worktree-root "/checkout")
             (host
              (make-fake-workspace-boundaries
               :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                            (cons "/bare/worktrees/wt/HEAD" "0123")
                            (cons "/bare/worktrees/wt/commondir" "../..")
                            (cons "/bare/worktrees/wt/gitdir" "/checkout/.git")
                            (cons "/checkout/.git" "gitdir: /bare/worktrees/wt")
                            (cons "/checkout/pre-commit" "existing"))
               :directories (list "/bare/objects" "/bare/refs")
               :symlinks (list (cons "/bare/hooks" worktree-root))
               :cwd worktree-root))
             (root (resolved-root host)))
        (expect (git-repository-common-dir (workspace-root-repository root))
                :to-equal "/bare")
        (expect (boundary-verdict host root "pre-commit")
                :to-be :git-directory)))
  (it "refuses common-root inode aliases and stat failures"
    (let* ((root-path "/bare/unrelated")
           (git-directory "/bare/worktrees/wt")
           (files
             (list (cons "/bare/HEAD" "ref: refs/heads/main")
                   (cons "/bare/unrelated/.git"
                         "gitdir: /bare/worktrees/wt")
                   (cons "/bare/worktrees/wt/HEAD" "0123")
                   (cons "/bare/worktrees/wt/commondir" "../..")
                   (cons "/bare/worktrees/wt/gitdir"
                         "/bare/unrelated/.git")))
           (directories (list "/bare/hooks" "/bare/unrelated/src"))
           (file-identities
             (list (cons root-path (cons 1 2))
                   (cons "/bare/hooks" (cons 1 2))))
           (host
             (make-fake-workspace-boundaries
              :files files :directories directories
              :file-identities file-identities))
           (root (resolved-root host :root root-path)))
      (expect (aitools.workspace.application::%git-common-root-identity-hazard-p host "/bare" root-path)
              :to-be :identity-match)
      (let ((list-failing-host
             (make-fake-workspace-boundaries
              :files files
              :directories directories
              :file-identities file-identities
              :stat-failures (list "/bare/hooks"))))
        (expect (aitools.workspace.application::%git-common-root-identity-hazard-p
                 list-failing-host "/bare" root-path)
                :to-be :stat-failed)
        (expect (boundary-verdict list-failing-host root "src/new.txt")
                :to-be :git-directory))
      (let* ((failing-host
               (make-fake-workspace-boundaries
                :files files :directories directories
                :file-identities file-identities
                :stat-failures (list root-path)))
             (failing-root
               (aitools.workspace.domain:make-workspace-root
                :path root-path
                :real root-path
                :source :option
                :repository
                (aitools.workspace.domain:make-git-repository
                 :top root-path
                 :git-dir git-directory
                 :common-dir "/bare"))))
        (expect (aitools.workspace.application::%git-common-root-identity-hazard-p
                 failing-host "/bare" root-path)
                :to-be :stat-failed)
        (expect (boundary-verdict failing-host failing-root "src/new.txt")
                :to-be :git-directory))))
  (it "refuses the long-s spelling of refs"
    (expect (%git-metadata-alias-verdict "refſ" "heads/main")
            :to-be :git-directory))
  (it "refuses the ligature spelling of lost-found"
    (expect (%git-metadata-alias-verdict "loﬆ-found" "file")
            :to-be :git-directory))
  (it "refuses a real-filesystem metadata alias by device and inode"
    (aitools.test.support:call-with-temporary-directory
     (lambda (directory)
       (let* ((base (namestring directory))
              (common (concatenate 'string base "common"))
              (canonical-directory (concatenate 'string common "/lost-found"))
              (alias-directory (concatenate 'string common "/loﬆ-found"))
              (canonical-file (concatenate 'string canonical-directory "/probe"))
              (alias-file (concatenate 'string alias-directory "/probe"))
              (git-dir (concatenate 'string common "/worktrees/wt"))
              (host
               (aitools.workspace.infrastructure:make-host-workspace-boundaries
                :getenv (constantly nil)
                :home-directory (constantly base)
                :current-directory (constantly base)))
              (root
               (aitools.workspace.domain:make-workspace-root
                :path alias-directory
                :real alias-directory
                :source :option
                :repository
                (aitools.workspace.domain:make-git-repository
                 :top alias-directory :git-dir git-dir :common-dir common))))
         (sb-posix:mkdir common #o700)
         (sb-posix:mkdir canonical-directory #o700)
         (sb-posix:mkdir (concatenate 'string common "/worktrees") #o700)
         (sb-posix:mkdir git-dir #o700)
         (with-open-file (stream (sb-ext:parse-native-namestring canonical-file)
                                 :direction :output
                                 :if-exists :error)
           (write-string "probe" stream))
         (let* ((canonical-stat (sb-posix:stat canonical-file))
                (case-folded-file
                 (concatenate (quote string) common "/LOST-FOUND/probe"))
                (case-folded-stat
                 (handler-case (sb-posix:stat case-folded-file)
                   (sb-posix:syscall-error (condition)
                     (if (= (sb-posix:syscall-errno condition)
                            sb-posix:enoent)
                         nil
                         (error condition))))))
           (unless case-folded-stat
             (cl-weave:skip
              (concatenate (quote string)
                           "case-sensitive filesystem: property cannot be observed on a case-sensitive filesystem; "
                           "fake-host name-check specs cover the folding rule on every platform")))
           (let ((alias-stat (sb-posix:stat alias-file)))
             (expect (and (= (sb-posix:stat-dev canonical-stat)
                             (sb-posix:stat-dev case-folded-stat))
                          (= (sb-posix:stat-ino canonical-stat)
                             (sb-posix:stat-ino case-folded-stat)))
                     :to-be t)
             (expect (and (= (sb-posix:stat-dev canonical-stat)
                             (sb-posix:stat-dev alias-stat))
                          (= (sb-posix:stat-ino canonical-stat)
                             (sb-posix:stat-ino alias-stat)))
                     :to-be t)
             (expect (aitools.workspace.application::%git-common-root-identity-hazard-p
                      host common alias-directory)
                     :to-be :identity-match)
             (expect (boundary-verdict host root "pre-commit")
                     :to-be :git-directory))))))))
