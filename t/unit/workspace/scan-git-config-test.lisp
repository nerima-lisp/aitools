(in-package #:aitools.workspace.test)

(describe "aitools.workspace.application scan: git config sources"
  (it "reads core.excludesFile from GIT_CONFIG_COUNT pairs, ignoring a non-numeric count"
    (flet ((paths (count)
             (let ((host (make-fake-host
                          :files '(("/r/.git/HEAD" . "x") ("/r/ignore-list" . "*.secret")
                                   ("/r/a.secret" . "") ("/r/b.txt" . ""))
                          :environment `(("GIT_CONFIG_COUNT" . ,count)
                                         ("GIT_CONFIG_KEY_0" . "user.name")
                                         ("GIT_CONFIG_KEY_1" . "core.excludesFile")
                                         ("GIT_CONFIG_VALUE_1" . "/r/ignore-list"))
                          :cwd "/r")))
               (scan-paths host (resolved-root host)))))
      (expect (paths "2") :to-equal '("b.txt" "ignore-list"))
      (expect (paths "two") :to-equal '("a.secret" "b.txt" "ignore-list"))))

  (it "reads GIT_CONFIG_GLOBAL instead of ~/.gitconfig"
    (let ((host (make-fake-host :files `(("/r/.git/HEAD" . "x")
                                         ("/cfg/global" . ,(format nil "[core]~%~AexcludesFile = /cfg/ignore" #\Tab))
                                         ("/cfg/ignore" . "*.a")
                                         ("/home/user/.gitconfig" . ,(format nil "[core]~%~AexcludesFile = /cfg/other" #\Tab))
                                         ("/cfg/other" . "*.b")
                                         ("/r/x.a" . "") ("/r/x.b" . ""))
                                :environment '(("GIT_CONFIG_GLOBAL" . "/cfg/global"))
                                :cwd "/r")))
      (expect (scan-paths host (resolved-root host)) :to-equal '("x.b"))))

  (it "reads config.worktree only under extensions.worktreeConfig"
    (flet ((paths (enabled)
             (let ((host (make-fake-host
                          :files `(("/r/.git/HEAD" . "x")
                                   ("/r/.git/config" . ,(format nil "[extensions]~%~Aworktreeconfig = ~A" #\Tab enabled))
                                   ("/r/.git/config.worktree" . ,(format nil "[core]~%~AexcludesFile = /w/ignore" #\Tab))
                                   ("/w/ignore" . "*.w") ("/r/x.w" . ""))
                          :cwd "/r")))
               (scan-paths host (resolved-root host)))))
      (expect (paths "true") :to-equal '())
      (expect (paths "false") :to-equal '("x.w"))))

  (it "reads a sha256 index, and ignores a malformed one instead of keeping tracked files"
    (flet ((paths (index)
             (let ((host (repo-host `((".git/config" . ,(format nil "[extensions]~%~Aobjectformat = sha256" #\Tab))
                                      (".gitignore" . "*.gen") ("x.gen" . "")
                                      (".git/index" . ,index)))))
               (scan-paths host (resolved-root host)))))
      (expect (paths (index-octets '("x.gen") :hash-size 32)) :to-equal '(".gitignore" "x.gen"))
      (expect (paths (index-octets '("x.gen"))) :to-equal '(".gitignore")))))
