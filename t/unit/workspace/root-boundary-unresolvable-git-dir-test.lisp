(in-package #:aitools.workspace.test)
(describe "aitools.workspace.application unresolved git directory boundary"
  (it "refuses writes when the repository git directory cannot be resolved"
    (let* ((root-path "/bare/.worktrees/wt")
           (common-dir "/bare")
           (git-dir "/bare/worktrees/wt")
           (loop-dir "/bare/worktrees/loop")
           (host
            (make-fake-workspace-boundaries
             :directories (list root-path)
             :symlinks (list (cons git-dir loop-dir)
                             (cons loop-dir git-dir))))
           (root
            (aitools.workspace.domain:make-workspace-root
             :path root-path
             :real root-path
             :source :option
             :repository
             (aitools.workspace.domain:make-git-repository
              :top root-path :git-dir git-dir :common-dir common-dir))))
      (expect (aitools.workspace.application::resolve-real-path host git-dir)
              :to-be nil)
      (expect (aitools.workspace.application::resolve-real-path host common-dir)
              :to-equal common-dir)
      (expect (boundary-verdict host root "git-rebase-todo")
              :to-be :git-directory))))
