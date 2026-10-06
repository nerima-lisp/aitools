;;;; t/unit/workspace/scan-test.lisp
;;;;
;;;; Ignore-rule and pruning behavior of CALL-WITH-WORKSPACE-SCAN/K over the
;;;; in-memory host. Real-git parity lives in the integration test.
(in-package #:aitools.workspace.test)

(defun repo-host (files &rest options)
  "A fake host with a git repository at /r holding FILES (relative paths)."
  (apply #'make-fake-host
         :files (append '(("/r/.git/HEAD" . "ref: refs/heads/main"))
                        (loop for (path . content) in files collect (cons (join-path "/r" path) content)))
         :cwd "/r"
         options))

(defun counting-host (host counts)
  "HOST with LIST-DIRECTORY calls counted per path in the hash table COUNTS."
  (make-workspace-host
   :list-directory (lambda (path) (incf (gethash path counts 0)) (host-list-directory host path))
   :stat (lambda (path) (host-stat host path))
   :read-link (lambda (path) (host-read-link host path))
   :read-octets (lambda (path) (host-read-octets host path))
   :getenv (lambda (name) (host-getenv host name))
   :home-directory (lambda () (host-home-directory host))
   :current-directory (lambda () (host-current-directory host))))

(describe "aitools.workspace.application scan: ignore sources"
  (it "applies .gitignore rules, nested files, and negation inside git"
    (let ((host (repo-host '((".gitignore" . "*.log
build/
!keep.log
")
                             ("a.log" . "") ("keep.log" . "") ("src/main.lisp" . "")
                             ("src/.gitignore" . "/generated.lisp") ("src/generated.lisp" . "")
                             ("src/deep/generated.lisp" . "") ("build/out" . "")))))
      (multiple-value-bind (paths source) (scan-paths host (resolved-root host))
        (expect source :to-be :gitignore)
        (expect paths :to-equal '(".gitignore" "keep.log" "src" "src/.gitignore" "src/deep"
                                  "src/deep/generated.lisp" "src/main.lisp")))))

  (it "never lists an ignored directory"
    (let* ((counts (make-hash-table :test 'equal))
           (host (counting-host (repo-host '((".gitignore" . "node_modules/") ("node_modules/x/y.js" . "")
                                             ("index.js" . "")))
                                counts)))
      (scan-paths host (resolved-root host))
      (expect (gethash "/r/node_modules" counts) :to-be-falsy)
      (expect (gethash "/r" counts) :to-be-truthy)))

  (it "uses info/exclude and the XDG global excludes file, deeper files winning"
    (let ((host (make-fake-host
                  :files '(("/r/.git/HEAD" . "x") ("/r/.git/info/exclude" . "*.tmp")
                           ("/xdg/git/ignore" . "*.bak
*.keep")
                           ("/r/a.tmp" . "") ("/r/b.bak" . "") ("/r/c.keep" . "") ("/r/sub/.gitignore" . "!c.keep")
                           ("/r/sub/c.keep" . ""))
                  :environment '(("XDG_CONFIG_HOME" . "/xdg")) :cwd "/r")))
      (expect (scan-paths host (resolved-root host)) :to-equal '("sub" "sub/.gitignore" "sub/c.keep"))))

  (it "prefers core.excludesFile (with ~/ expansion and include.path) over the XDG default"
    (let ((host (make-fake-host
                 :files '(("/r/.git/HEAD" . "x")
                          ("/r/.git/config" . "[include]
	path = ../extra.config")
                          ("/r/extra.config" . "[core]
	excludesFile = ~/my-ignore")
                          ("/home/user/my-ignore" . "*.secret")
                          ("/home/user/.config/git/ignore" . "*.txt")
                          ("/r/a.secret" . "") ("/r/b.txt" . ""))
                 :cwd "/r")))
      (expect (scan-paths host (resolved-root host)) :to-equal '("b.txt" "extra.config"))))

  (it "folds case under core.ignoreCase"
    (let ((host (make-fake-host :files '(("/r/.git/HEAD" . "x") ("/r/.git/config" . "[core]
	ignorecase = true")
                                         ("/r/.gitignore" . "*.LOG") ("/r/a.log" . ""))
                                :cwd "/r")))
      (expect (scan-paths host (resolved-root host)) :to-equal '(".gitignore"))))

  (it "keeps tracked files even when a pattern matches them"
    (let ((host (repo-host `((".gitignore" . "vendor/
*.gen")
                             ("vendor/lib.c" . "") ("vendor/untracked.c" . "") ("x.gen" . "")
                             (".git/index" . ,(index-octets '("vendor/lib.c" "x.gen")))))))
      (expect (scan-paths host (resolved-root host)) :to-equal '(".gitignore" "vendor/lib.c" "x.gen"))))

  (it "uses the builtin list outside git"
    (let ((host (make-fake-host :files '(("/p/node_modules/a.js" . "") ("/p/target/x" . "") ("/p/src/a.rs" . "")
                                         ("/p/.gitignore" . "src/"))
                                :cwd "/p")))
      (multiple-value-bind (paths source) (scan-paths host (resolved-root host))
        (expect source :to-be :builtin)
        (expect paths :to-equal '(".gitignore" "src" "src/a.rs")))))

  (it "includes ignored files with --no-ignore but still skips .git and temporaries"
    (let ((host (repo-host '((".gitignore" . "*.log") ("a.log" . "") (".aitools-op-1-0.tmp" . "")))))
      (multiple-value-bind (paths source) (scan-paths host (resolved-root host) :no-ignore t)
        (expect source :to-be :none)
        (expect paths :to-equal '(".gitignore" "a.log")))))

  (it "applies staged overlay entries and a staged .gitignore"
    (let* ((host (repo-host '((".gitignore" . "*.log") ("a.log" . "") ("old.txt" . ""))))
           (overlay (make-workspace-overlay
                     :list-directory (lambda (directory entries)
                                       (if (string= directory "")
                                           (cons (make-workspace-entry :name "new.txt" :size 3)
                                                 (remove "old.txt" entries :key #'workspace-entry-name :test #'string=))
                                           entries))
                     :read-octets (lambda (path)
                                    (if (string= path ".gitignore")
                                        (values t (string-bytes "*.txt"))
                                        (values nil nil))))))
      (expect (scan-paths host (resolved-root host) :overlay overlay) :to-equal '(".gitignore" "a.log")))))

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

(describe "aitools.workspace.application scan: order, filters, and exits"
  (it "keeps scan filter decisions in a pure reason function"
    (let ((entry (make-workspace-entry :name "a.lisp" :kind :file :size 8 :mtime 100)))
      (expect (scan-filter-reason entry "a.lisp" nil (make-glob-filter '("*.lisp"))
                                  (lambda (path) (declare (ignore path)) t) 9 99)
              :to-be nil)
      (expect (scan-filter-reason entry "a.lisp" nil (make-glob-filter '("*.lisp"))
                                  (lambda (path) (declare (ignore path)) t) 9 100)
              :to-be :too-old)
      (expect (scan-filter-reason entry "a.lisp" nil (make-glob-filter '("*.lisp"))
                                  (lambda (path) (declare (ignore path)) nil) 9 nil)
              :to-be :language)
      (expect (scan-filter-reason entry "a.txt" nil (make-glob-filter '("*.lisp"))
                                  nil 9 nil)
              :to-be :glob)
      (expect (scan-filter-reason entry "a.txt" t (make-glob-filter '("*.lisp"))
                                  nil 9 nil)
              :to-be nil)))

  (it "emits in full-path order, a directory sorting as NAME/"
    (let ((host (repo-host '(("a.txt" . "") ("a/b" . "") ("a0" . "")))))
      (expect (scan-paths host (resolved-root host)) :to-equal '("a.txt" "a" "a/b" "a0"))))

  (it "filters by glob, language predicate, and mtime"
    (let* ((host (repo-host '(("a.lisp" . "") ("b.py" . "") ("d/c.lisp" . ""))))
           (root (resolved-root host)))
      (expect (scan-paths host root :glob '("*.lisp")) :to-equal '("a.lisp" "d/c.lisp"))
      (expect (scan-paths host root :lang (lambda (path) (search ".py" path))) :to-equal '("b.py"))
      (expect (scan-paths host root :newer 1000) :to-equal '())
      (expect (length (scan-paths host root :newer 999)) :to-be 4)))

  (it "reports files above --skip-larger-than instead of emitting them"
    (let* ((host (repo-host '(("small" . "x") ("big" . "0123456789"))))
           (skipped '()))
      (expect (scan-paths host (resolved-root host) :skip-larger-than 5
                                                    :on-skip (lambda (entry reason)
                                                               (push (list (scan-entry-path entry) reason) skipped)))
              :to-equal '("small"))
      (expect skipped :to-equal '(("big" :too-large)))))

  (it "stops when EMIT returns :STOP and says so"
    (let* ((host (repo-host '(("a" . "") ("b" . "") ("c" . ""))))
           (seen '()))
      (expect (call-with-workspace-scan/k host (resolved-root host)
                                          :emit (lambda (entry result)
                                                  (declare (ignore result))
                                                  (push (scan-entry-path entry) seen)
                                                  (when (string= (scan-entry-path entry) "b") :stop))
                                          :on-complete (lambda (source stopped) (list source stopped))
                                          :on-error (lambda (&rest args) args))
              :to-equal '(:gitignore t))
      (expect (reverse seen) :to-equal '("a" "b"))))

  (it "delivers WORK results in path order whatever order the mapper runs them"
    (let* ((files (loop for i from 0 below 150 collect (cons (format nil "f~3,'0D" i) "")))
           (base (repo-host files))
           (host (make-workspace-host
                  :list-directory (lambda (path) (host-list-directory base path))
                  :stat (lambda (path) (host-stat base path))
                  :read-link (lambda (path) (host-read-link base path))
                  :read-octets (lambda (path) (host-read-octets base path))
                  :getenv (constantly nil)
                  :home-directory (constantly "/home/user")
                  :current-directory (constantly "/r")
                  :call-with-ordered-mapper
                  (lambda (thunk)
                    (funcall thunk (lambda (function items)
                                     (reverse (mapcar function (reverse items))))))))
           (results '()))
      (call-with-workspace-scan/k host (resolved-root host)
                                  :work (lambda (entry) (string-upcase (scan-entry-path entry)))
                                  :emit (lambda (entry result)
                                          (push (list (scan-entry-path entry) result) results)
                                          nil)
                                  :on-complete (constantly nil)
                                  :on-error (constantly nil))
      (setf results (nreverse results))
      (expect (length results) :to-be 150)
      (expect (first results) :to-equal '("f000" "F000"))
      (expect (car (last results)) :to-equal '("f149" "F149"))
      (expect (mapcar #'first results) :to-equal (sort (mapcar #'first results) #'string<))))

  (it "scans explicit starting points, emitting a named file even when ignored"
    (let* ((host (repo-host '((".gitignore" . "*.log") ("x/a.log" . "") ("x/b" . "") ("y/c" . ""))))
           (root (resolved-root host)))
      (expect (scan-paths host root :paths '("y" "x/a.log")) :to-equal '("x/a.log" "y/c"))
      (expect (scan-paths host root :paths '("x" "x/b")) :to-equal '("x/b"))))

  (it "refuses starting points outside the root and reports missing ones"
    (let* ((host (repo-host '(("a" . ""))))
           (root (resolved-root host)))
      (expect (call-with-workspace-scan/k host root :paths '("../elsewhere")
                                                    :emit (constantly nil) :on-complete (constantly :done)
                                                    :on-error (lambda (reason path) (list reason path)))
              :to-equal '(:outside-root "../elsewhere"))
      (expect (call-with-workspace-scan/k host root :paths '("missing")
                                                    :emit (constantly nil) :on-complete (constantly :done)
                                                    :on-error (lambda (reason path) (list reason path)))
              :to-equal '(:not-found "missing")))))

(describe "aitools.workspace.application ignored-path query"
  (it "reports a path ignored by itself or through an ignored ancestor"
    (let* ((host (repo-host '((".gitignore" . "build/
*.o") ("build/x.txt" . "") ("src/a.o" . "") ("src/a.c" . ""))))
           (root (resolved-root host)))
      (expect (workspace-path-ignored-p host root "src/a.o") :to-be-truthy)
      (expect (workspace-path-ignored-p host root "build/x.txt") :to-be-truthy)
      (expect (workspace-path-ignored-p host root "src/a.c") :to-be-falsy)
      (expect (workspace-path-ignored-p host root ".git/config") :to-be-truthy)
      (expect (workspace-path-ignored-p host root "src/a.o" :no-ignore t) :to-be-falsy))))
