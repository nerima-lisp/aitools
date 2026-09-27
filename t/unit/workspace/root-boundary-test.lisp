;;;; t/unit/workspace/root-boundary-test.lisp
;;;;
;;;; Root resolution and the write boundary, over the in-memory host.
(in-package #:aitools.workspace.test)

(defun boundary-verdict (host root target &key temporary-root state-root)
  (call-with-workspace-boundary/k host root target
                                  :temporary-root temporary-root
                                  :state-root state-root
                                  :on-inside (lambda (path verdict) (declare (ignore path)) verdict)
                                  :on-outside (lambda (verdict lexical real)
                                                (declare (ignore lexical real))
                                                verdict)))

(describe "aitools.workspace.domain path syntax"
  (it "normalizes lexically"
    (expect (normalize-path "/a/./b//c/../d/") :to-equal "/a/b/d")
    (expect (normalize-path "/../..") :to-equal "/")
    (expect (normalize-path "a/../../b") :to-equal "../b")
    (expect (normalize-path "./") :to-equal ""))

  (it "joins, splits, and takes parents"
    (expect (join-path "/a" "b") :to-equal "/a/b")
    (expect (join-path "/a/" "b") :to-equal "/a/b")
    (expect (join-path "" "b") :to-equal "b")
    (expect (join-path "/a" "/b") :to-equal "/b")
    (expect (path-parent "/a/b") :to-equal "/a")
    (expect (path-parent "/a") :to-equal "/")
    (expect (path-parent "/") :to-be-falsy)
    (expect (path-parent "a") :to-equal "")
    (expect (path-basename "a/b.c") :to-equal "b.c")))

(describe "aitools.workspace.application root resolution"
  (it "uses --root relative to the working directory"
    (let* ((host (make-fake-host :directories '("/w/project") :cwd "/w"))
           (root (resolved-root host :root "project")))
      (expect (workspace-root-path root) :to-equal "/w/project")
      (expect (workspace-root-source root) :to-be :option)))

  (it "reports a missing or non-directory --root"
    (let ((host (make-fake-host :files '(("/w/file" . "x")) :cwd "/w")))
      (expect (call-with-resolved-root/k host :root "missing"
                                              :on-resolved (lambda (root) root)
                                              :on-error (lambda (reason path) (list reason path)))
              :to-equal '(:not-found "/w/missing"))
      (expect (call-with-resolved-root/k host :root "file"
                                              :on-resolved (lambda (root) root)
                                              :on-error (lambda (reason path) (list reason path)))
              :to-equal '(:not-a-directory "/w/file"))))

  (it "finds the git top level above the working directory"
    (let* ((host (make-fake-host :files '(("/repo/.git/HEAD" . "ref: refs/heads/main"))
                                 :directories '("/repo/a/b") :cwd "/repo/a/b"))
           (root (resolved-root host)))
      (expect (workspace-root-path root) :to-equal "/repo")
      (expect (workspace-root-source root) :to-be :git)
      (expect (git-repository-git-dir (workspace-root-repository root)) :to-equal "/repo/.git")))

  (it "follows a linked worktree's gitdir file and commondir"
    (let* ((host (make-fake-host
                  :files '(("/main/.git/HEAD" . "ref: refs/heads/main")
                           ("/main/.git/worktrees/wt/HEAD" . "0123")
                           ("/main/.git/worktrees/wt/commondir" . "../..")
                           ("/wt/.git" . "gitdir: /main/.git/worktrees/wt"))
                  :cwd "/wt"))
           (repository (workspace-root-repository (resolved-root host))))
      (expect (git-repository-top repository) :to-equal "/wt")
      (expect (git-repository-git-dir repository) :to-equal "/main/.git/worktrees/wt")
      (expect (git-repository-common-dir repository) :to-equal "/main/.git")))

  (it "falls back to the working directory outside git"
    (let ((root (resolved-root (make-fake-host :directories '("/plain") :cwd "/plain"))))
      (expect (workspace-root-source root) :to-be :cwd)
      (expect (workspace-root-repository root) :to-be-falsy)))

  (it "ignores a .git directory without HEAD"
    (let ((root (resolved-root (make-fake-host :directories '("/x/.git" "/x/sub") :cwd "/x/sub"))))
      (expect (workspace-root-source root) :to-be :cwd))))

(describe "aitools.workspace.application write boundary"
  (it "accepts a path inside the root and reports its relative form"
    (let* ((host (make-fake-host :directories '("/ws/src")))
           (root (resolved-root host :root "/ws")))
      (expect (boundary-verdict host root "src/new.lisp") :to-be :inside)
      (call-with-workspace-boundary/k
       host root "/ws/src/new.lisp"
       :on-inside (lambda (path verdict)
                    (declare (ignore verdict))
                    (expect (aitools.kernel.domain:workspace-path-relative path) :to-equal "src/new.lisp"))
       :on-outside (lambda (&rest args) (fail (format nil "unexpected ~S" args))))))

  (it "refuses a path that leaves the root"
    (let* ((host (make-fake-host :directories '("/ws" "/other")))
           (root (resolved-root host :root "/ws")))
      (expect (boundary-verdict host root "../other/x") :to-be :outside-root)
      (expect (boundary-verdict host root "/other/x") :to-be :outside-root)))

  (it "refuses a symlink inside the root that leads outside"
    (let* ((host (make-fake-host :directories '("/ws" "/etc") :symlinks '(("/ws/link" . "/etc"))))
           (root (resolved-root host :root "/ws")))
      (expect (boundary-verdict host root "link/passwd") :to-be :symlink-escape)))

  (it "refuses anything under a .git directory, at any depth"
    (let* ((host (make-fake-host :files '(("/ws/.git/HEAD" . "x")) :directories '("/ws/sub/.git")))
           (root (resolved-root host :root "/ws")))
      (expect (boundary-verdict host root ".git/config") :to-be :git-directory)
      (expect (boundary-verdict host root ".git") :to-be :git-directory)
      (expect (boundary-verdict host root "sub/.git/hooks/x") :to-be :git-directory)
      (expect (boundary-verdict host root ".gitignore") :to-be :inside)))

  (it "lets the mktemp area through as the only outside exception"
    (let* ((host (make-fake-host :directories '("/ws" "/state/ws-id/tmp")))
           (root (resolved-root host :root "/ws")))
      (expect (boundary-verdict host root "/state/ws-id/tmp/a.txt" :temporary-root "/state/ws-id/tmp")
              :to-be :temporary)
      (expect (boundary-verdict host root "/state/ws-id/other" :temporary-root "/state/ws-id/tmp")
              :to-be :outside-root)
      (expect (boundary-verdict host root "/state/ws-id/tmp/a.txt") :to-be :outside-root)))

  (it "refuses a symlink in the mktemp area that escapes it"
    (let* ((host (make-fake-host :directories '("/ws" "/state/tmp" "/secret")
                                 :symlinks '(("/state/tmp/out" . "/secret"))))
           (root (resolved-root host :root "/ws")))
      (expect (boundary-verdict host root "/state/tmp/out/x" :temporary-root "/state/tmp")
              :to-be :outside-root)))

  (it "accepts a root reached through a symlinked prefix"
    (let* ((host (make-fake-host :directories '("/private/tmp/ws") :symlinks '(("/tmp" . "private/tmp"))))
           (root (resolved-root host :root "/tmp/ws")))
      (expect (workspace-root-real root) :to-equal "/private/tmp/ws")
      (expect (boundary-verdict host root "/private/tmp/ws/a") :to-be :inside)
      (expect (boundary-verdict host root "/tmp/ws/a") :to-be :inside)))

  (it "refuses the state directory under the root except its mktemp area"
    ;; Root $HOME with the default state home inside it: a forged intent
    ;; record under commit/ would be rolled forward by the next command.
    (let* ((host (make-fake-host :directories '("/home/u/.local/state/aitools/ws-id/tmp"
                                                "/home/u/.local/state/aitools/other-id/tmp")
                                 :symlinks '(("/home/u/st" . ".local/state"))))
           (root (resolved-root host :root "/home/u"))
           (state "/home/u/.local/state/aitools")
           (tmp "/home/u/.local/state/aitools/ws-id/tmp"))
      (flet ((verdict (target)
               (boundary-verdict host root target :temporary-root tmp :state-root state)))
        (expect (verdict ".local/state/aitools/ws-id/commit/op.json") :to-be :state-directory)
        (expect (verdict ".local/state/aitools/ws-id/lock") :to-be :state-directory)
        (expect (verdict ".local/state/aitools") :to-be :state-directory)
        (expect (verdict "st/aitools/ws-id/journal/ops.jsonl") :to-be :state-directory)
        (expect (verdict ".local/state/aitools/other-id/tmp/x") :to-be :state-directory)
        (expect (verdict ".local/state/aitools/ws-id/tmp") :to-be :state-directory)
        (expect (verdict ".local/state/aitools/ws-id/tmp/a.txt") :to-be :temporary)
        (expect (verdict ".local/state/other.txt") :to-be :inside))))

  (it "refuses a separate git directory and common directory by their real paths"
    ;; `git init --separate-git-dir=store`: the git directory has no `.git`
    ;; component, so only its resolved path identifies it.
    (let* ((host (make-fake-host
                  :files '(("/ws/.git" . "gitdir: store")
                           ("/ws/store/HEAD" . "ref: refs/heads/main")
                           ("/ws/store/commondir" . "../common")
                           ("/ws/common/config" . ""))
                  :directories '("/ws/src" "/ws/store/hooks")
                  :symlinks '(("/ws/alias" . "store"))))
           (root (resolved-root host :root "/ws")))
      (expect (boundary-verdict host root "store/hooks/evil") :to-be :git-directory)
      (expect (boundary-verdict host root "store") :to-be :git-directory)
      (expect (boundary-verdict host root "alias/hooks/evil") :to-be :git-directory)
      (expect (boundary-verdict host root "common/config") :to-be :git-directory)
      (expect (boundary-verdict host root "src/a.txt") :to-be :inside)
      (expect (boundary-verdict host root "storefront.txt") :to-be :inside)))

  (it "treats a symlink loop as unresolvable"
    (let* ((host (make-fake-host :directories '("/ws") :symlinks '(("/ws/a" . "b") ("/ws/b" . "a"))))
           (root (resolved-root host :root "/ws")))
      (expect (boundary-verdict host root "a/x") :to-be :unresolvable))))
