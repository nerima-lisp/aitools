;;;; t/integration/workspace-gitignore-parity-test.lisp
;;;;
;;;; The scan's ignore decisions must equal
;;;; `git ls-files --cached --others --exclude-standard` on fixture
;;;; repositories built here in a scratch directory. git is the oracle only:
;;;; the scan itself reads .gitignore, info/exclude, the excludes file, git
;;;; config, and the index directly. Each fixture runs git with a private
;;;; HOME and XDG_CONFIG_HOME and no system config, and hands the scan the
;;;; same environment through the host's GETENV, so neither side sees the
;;;; developer's own configuration. Without git on PATH the tests skip.
(in-package #:cl-user)

(defpackage #:aitools.workspace.integration-test
  (:use #:cl #:aitools.workspace.integration-support)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect #:fail #:skip))

(in-package #:aitools.workspace.integration-test)

(defun %environment (scratch)
  `(("HOME" . ,(concatenate 'string scratch "/home"))
    ("XDG_CONFIG_HOME" . ,(concatenate 'string scratch "/xdg"))
    ("GIT_CONFIG_NOSYSTEM" . "1")
    ("GIT_AUTHOR_NAME" . "fixture") ("GIT_AUTHOR_EMAIL" . "fixture@example.invalid")
    ("GIT_COMMITTER_NAME" . "fixture") ("GIT_COMMITTER_EMAIL" . "fixture@example.invalid")))

(defun git (scratch directory &rest arguments)
  (multiple-value-bind (code stdout stderr)
      (run-command directory "git" arguments :environment (%environment scratch))
    (unless (zerop code)
      (error "git ~{~A~^ ~} failed (~D): ~A" arguments code stderr))
    stdout))

(defun git-listing (scratch directory)
  "Sorted paths from `git ls-files -z --cached --others --exclude-standard`."
  (let ((bytes (git scratch directory "ls-files" "-z" "--cached" "--others" "--exclude-standard"))
        (paths '()) (start 0))
    (loop for i from 0 below (length bytes)
          when (zerop (aref bytes i))
            do (push (sb-ext:octets-to-string bytes :start start :end i :external-format :utf-8) paths)
               (setf start (1+ i)))
    (sort (remove-duplicates paths :test #'string=) #'string<)))

(defun scan-listing (scratch directory)
  "Sorted non-directory paths the workspace scan emits for DIRECTORY."
  (let* ((environment (%environment scratch))
         (host (aitools.workspace.infrastructure:make-host-workspace-host
                :getenv (lambda (name) (cdr (assoc name environment :test #'string=)))
                :home-directory (lambda () (cdr (assoc "HOME" environment :test #'string=)))
                :current-directory (lambda () directory)))
         (root (aitools.workspace.application:call-with-resolved-root/k
                host :root directory
                     :on-resolved #'identity
                     :on-error (lambda (reason path) (error "root ~A ~A" reason path))))
         (paths '()))
    (aitools.workspace.application:call-with-workspace-scan/k
     host root
     :skip-larger-than nil
     :emit (lambda (entry result)
             (declare (ignore result))
             (unless (eq (aitools.workspace.application:scan-entry-kind entry) :directory)
               (push (aitools.workspace.application:scan-entry-path entry) paths))
             nil)
     :on-complete (lambda (source stopped)
                    (declare (ignore stopped))
                    (unless (eq source :gitignore) (error "expected a gitignore scan, got ~A" source)))
     :on-error (lambda (reason path) (error "scan ~A ~A" reason path)))
    (sort paths #'string<)))

(defun expect-parity (scratch directory &key (minimum 10))
  (let ((git (git-listing scratch directory))
        (scan (scan-listing scratch directory)))
    (expect (list :only-git (set-difference git scan :test #'string=)
                  :only-scan (set-difference scan git :test #'string=))
            :to-equal '(:only-git nil :only-scan nil))
    (expect (length git) :to-be-greater-than (1- minimum))))

(defparameter *root-gitignore*
  (format nil "~{~A~%~}"
          '("# comment line"
            "*.log"
            "!keep.log"
            "build/"
            "!build/important.bin"
            "/root-only.txt"
            "doc/**/*.pdf"
            "**/tmp-*"
            "nested/deep/**"
            "trailing-space.txt   "
            "escaped\\ space.txt\\ "
            "\\#hash.txt"
            "\\!bang.txt"
            "*.[oa]"
            "\\[x\\].txt"
            "link-to-build/"
            "vendor/"
            "UPPER.md")))

(defparameter *fixture-files*
  '("a.log" "keep.log" "sub/b.log" "sub/keep.log" "build/out.bin" "build/important.bin" "sub/build/x"
    "other/build" "root-only.txt" "sub/root-only.txt" "doc/a.pdf" "doc/x/y/b.pdf" "doc/c.txt" "tmp-1"
    "sub/tmp-2" "trailing-space.txt" "escaped space.txt " "#hash.txt" "!bang.txt" "lib.o" "lib.a" "lib.c"
    "nested/deep/file" "nested/other" "sub/x.txt" "sub/keep.txt" "sub/nested/keep.txt" "z.exclude"
    "g.globalignore" "special.globalignore" "[x].txt" "y.txt" "star*name.c" "日本語.txt" "upper.md"
    "vendor/untracked.c" "vendor/tracked.c" "forced.log" "plain/readme"))

(defun build-main-fixture (scratch repository)
  (git scratch scratch "init" "-q" repository)
  (write-file (concatenate 'string repository "/.gitignore") *root-gitignore*)
  (write-file (concatenate 'string repository "/sub/.gitignore") (format nil "!b.log~%*.txt~%!/keep.txt~%"))
  (write-file (concatenate 'string repository "/.git/info/exclude") (format nil "*.exclude~%!special.globalignore~%"))
  (write-file (concatenate 'string scratch "/xdg/git/ignore") (format nil "*.globalignore~%"))
  (dolist (file *fixture-files*)
    (write-file (concatenate 'string repository "/" file) file))
  (make-symlink "build" (concatenate 'string repository "/link-to-build"))
  (git scratch repository "add" "-A")
  (git scratch repository "add" "-f" "vendor/tracked.c" "forced.log")
  (git scratch repository "commit" "-q" "-m" "fixture"))

(describe "aitools workspace gitignore parity with git ls-files"
  (it "matches git on nested .gitignore, negation, dir-only, anchoring, **, escapes, excludes, and the index"
    (unless (program-path "git") (skip "git is not on PATH; parity with git ls-files not checked"))
    (with-scratch-directory (scratch)
      (let ((repository (concatenate 'string scratch "/repo")))
        (build-main-fixture scratch repository)
        (write-file (concatenate 'string repository "/new-untracked.txt") "x")
        (write-file (concatenate 'string repository "/new.log") "x")
        (expect-parity scratch repository))))

  (it "matches git with core.excludesFile set (the XDG default then unused)"
    (unless (program-path "git") (skip "git is not on PATH; parity with git ls-files not checked"))
    (with-scratch-directory (scratch)
      (let ((repository (concatenate 'string scratch "/repo")))
        (git scratch scratch "init" "-q" repository)
        (write-file (concatenate 'string scratch "/home/custom-ignore") (format nil "*.custom~%"))
        (write-file (concatenate 'string scratch "/xdg/git/ignore") (format nil "*.xdg~%"))
        (git scratch repository "config" "core.excludesFile" "~/custom-ignore")
        (dolist (file '("a.custom" "b.xdg" "c.txt" "d/e.custom" "d/f.xdg" "g/h.txt" "i.txt" "j.txt"
                        "k.txt" "l.txt" "m.txt"))
          (write-file (concatenate 'string repository "/" file) file))
        (expect-parity scratch repository :minimum 9)
        (expect (git-listing scratch repository)
                :to-equal '("b.xdg" "c.txt" "d/f.xdg" "g/h.txt" "i.txt" "j.txt" "k.txt" "l.txt" "m.txt")))))

  (it "matches git in a linked worktree, whose info/exclude lives in the common dir"
    (unless (program-path "git") (skip "git is not on PATH; parity with git ls-files not checked"))
    (with-scratch-directory (scratch)
      (let ((repository (concatenate 'string scratch "/repo"))
            (worktree (concatenate 'string scratch "/wt")))
        (build-main-fixture scratch repository)
        (git scratch repository "worktree" "add" "-q" worktree)
        (dolist (file '("wt-new.txt" "wt.log" "q.exclude" "sub/w.txt" "build/w.bin"))
          (write-file (concatenate 'string worktree "/" file) file))
        (expect-parity scratch worktree)))))

(describe "aitools workspace scan never starts git"
  (it "has no process-starting reference in the workspace context's sources"
    (let* ((root (asdf:system-source-directory "aitools"))
           (offending
             (loop for file in (directory (merge-pathnames "packages/core/workspace/src/**/*.lisp" root))
                   for text = (uiop:read-file-string file)
                   when (some (lambda (needle) (search needle text :test #'char-equal))
                              '("run-program" "process-kit" "vcs-kit" "spawn" "posix-fork" "execv"))
                     collect (file-namestring file))))
      (expect offending :to-equal nil))))
