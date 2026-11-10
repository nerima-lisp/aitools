(in-package #:aitools.workspace.test)
(describe "workspace metadata verdict branch outcomes"
  (it "checks temporary-root identity aliases and safe paths"
   (let* ((safe-host
            (make-fake-workspace-boundaries
             :directories '("/bare/hooks" "/checkout" "/tmp/safe")))
          (alias-host
            (make-fake-workspace-boundaries
             :directories '("/bare/hooks" "/checkout" "/tmp/hooks" "/tmp/safe")
             :file-identities
             (list (cons "/bare/hooks" (cons 1 2))
                   (cons "/tmp/hooks" (cons 1 2))))))
    (expect (aitools.workspace.application::%common-root-identity-verdict
             safe-host "/bare" "/checkout" "/checkout"
             "/tmp/safe" "/tmp/safe" "/outside/file")
            :to-be-falsy)
    (expect (aitools.workspace.application::%common-root-identity-verdict
             safe-host "/bare" "/checkout" "/checkout"
             "/tmp/safe" "/tmp/safe" "/tmp/safe/file")
            :to-be-falsy)
    (expect (aitools.workspace.application::%common-root-identity-verdict
             alias-host "/bare" "/checkout" "/checkout"
             "/tmp/hooks" "/tmp/hooks" "/tmp/hooks/file")
            :to-be :git-directory)
    (expect (aitools.workspace.application::%common-root-identity-verdict
             alias-host "/bare" "/checkout" "/checkout"
             "/tmp/safe" "/tmp/hooks" "/tmp/hooks/file")
            :to-be :git-directory)))
  (it "refuses missing root and metadata identities"
   (let* ((root-stat-failed
           (make-fake-workspace-boundaries
            :directories '("/bare/hooks" "/checkout")
            :stat-failures '("/checkout")))
         (root-device-missing
           (make-fake-workspace-boundaries
            :directories '("/bare/hooks" "/checkout")
            :file-identities (list (cons "/checkout" (cons nil 2)))))
         (root-inode-missing
           (make-fake-workspace-boundaries
            :directories '("/bare/hooks" "/checkout")
            :file-identities (list (cons "/checkout" (cons 1 nil)))))
         (entry-unresolved
           (make-fake-workspace-boundaries
            :directories '("/checkout")
            :symlinks (list (cons "/bare/hooks" "/loop")
                            (cons "/loop" "/loop"))))
         (entry-device-missing
           (make-fake-workspace-boundaries
            :directories '("/bare/hooks" "/checkout")
            :file-identities (list (cons "/bare/hooks" (cons nil 3)))))
         (entry-inode-missing
           (make-fake-workspace-boundaries
            :directories '("/bare/hooks" "/checkout")
            :file-identities (list (cons "/bare/hooks" (cons 1 nil)))))
         (hosts
           (list root-stat-failed root-device-missing root-inode-missing
                 entry-unresolved entry-device-missing entry-inode-missing)))
    (dolist (host hosts)
      (expect (aitools.workspace.application::%git-common-root-identity-hazard-p
               host "/bare" "/checkout")
              :to-be :stat-failed))))
  (it "retains an ordinary common directory when root equals common"
   (expect (aitools.workspace.application::%linked-worktree-exception-verdict
            nil "/bare" "/bare" "/bare" nil)
           :to-be :retain-common-directory))
  (it "classifies a common directory below and equal to the root"
   (let* ((host (make-fake-workspace-boundaries :directories '("/repo/.git")))
          (repository
            (aitools.workspace.domain:make-git-repository
             :top "/repo" :git-dir "/repo/.git" :common-dir "/repo/.git")))
    (expect (aitools.workspace.application::%repository-git-directories
             host "/repo" (list repository) repository)
            :to-equal '("/repo/.git" "/repo/.git"))
    (expect (aitools.workspace.application::%repository-git-directories
             host "/repo/.git" (list repository) repository)
            :to-equal '("/repo/.git"))))
  (it "rejects entries whose device or inode is unavailable"
   (flet ((entry (device inode)
            (aitools.workspace.domain:make-workspace-entry
             :device device :inode inode)))
    (let ((valid (entry 1 2)))
      (expect (aitools.workspace.application::%workspace-entry-same-identity-p nil valid)
              :to-be-falsy)
      (expect (aitools.workspace.application::%workspace-entry-same-identity-p valid nil)
              :to-be-falsy)
      (expect (aitools.workspace.application::%workspace-entry-same-identity-p
               (entry nil 2) valid)
              :to-be-falsy)
      (expect (aitools.workspace.application::%workspace-entry-same-identity-p
               valid (entry nil 2))
              :to-be-falsy)
      (expect (aitools.workspace.application::%workspace-entry-same-identity-p
               (entry 1 nil) valid)
              :to-be-falsy)
      (expect (aitools.workspace.application::%workspace-entry-same-identity-p
               valid (entry 1 nil))
              :to-be-falsy)
      (expect (aitools.workspace.application::%workspace-entry-same-identity-p
               valid (entry 1 2))
              :to-be-truthy)))))
 (describe "workspace POSIX directory-name error branches"
  (it "marks nil directory names as incomplete"
    (aitools.test.support:call-with-temporary-directory
     (lambda (directory)
       (let ((path (namestring directory)))
         (let ((stream (sb-posix:opendir path)))
           (unwind-protect
               (expect
                (multiple-value-list
                 (aitools.workspace.infrastructure::%next-directory-name
                  stream
                  (lambda (dirent)
                    (declare (ignore dirent))
                    nil)))
                :to-equal '(nil :invalid))
             (sb-posix:closedir stream)))
         (let ((stream (sb-posix:opendir path)))
           (unwind-protect
               (expect
                (multiple-value-list
                 (aitools.workspace.infrastructure::%list-directory-entries
                  stream path
                  (lambda (dirent)
                    (declare (ignore dirent))
                    nil)))
                :to-equal '(nil t nil))
             (sb-posix:closedir stream)))))))
  (it "marks directory-name errors as incomplete"
    (aitools.test.support:call-with-temporary-directory
     (lambda (directory)
       (let ((path (namestring directory)))
         (let ((stream (sb-posix:opendir path)))
           (unwind-protect
               (expect
                (multiple-value-list
                 (aitools.workspace.infrastructure::%next-directory-name
                  stream
                  (lambda (dirent)
                    (declare (ignore dirent))
                    (error "mock dirent-name failure"))))
                :to-equal '(nil :invalid))
             (sb-posix:closedir stream)))
         (let ((stream (sb-posix:opendir path)))
           (unwind-protect
               (expect
                (multiple-value-list
                 (aitools.workspace.infrastructure::%list-directory-entries
                  stream path
                  (lambda (dirent)
                    (declare (ignore dirent))
                    (error "mock dirent-name failure"))))
                :to-equal '(nil t nil))
             (sb-posix:closedir stream))))))))
 (describe "workspace metadata symlink descendants"
  (it "refuses common metadata targets below a linked worktree root"
    (let* ((root-path "/bare/.worktrees/wt")
           (host
            (make-fake-workspace-boundaries
             :files
             (list (cons "/bare/HEAD" "ref: refs/heads/main")
                   (cons "/bare/worktrees/wt/HEAD" "0123")
                   (cons "/bare/worktrees/wt/commondir" "../..")
                   (cons "/bare/worktrees/wt/gitdir"
                         "/bare/.worktrees/wt/.git")
                   (cons "/bare/.worktrees/wt/.git"
                         "gitdir: /bare/worktrees/wt"))
             :directories
             (list "/bare/objects"
                   "/bare/refs"
                   "/bare/.worktrees/wt/hookdir"
                   "/bare/.worktrees/wt/src")
             :symlinks
             (list (cons "/bare/.worktrees/wt/git-dir"
                         "/bare/worktrees/wt")
                   (cons "/bare/hooks"
                         "/bare/.worktrees/wt/hookdir"))
             :cwd root-path))
           (root (resolved-root host)))
      (expect (boundary-verdict host root "hookdir/pre-commit")
              :to-be :git-directory))))
