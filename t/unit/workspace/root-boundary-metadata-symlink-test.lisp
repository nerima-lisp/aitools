(in-package #:aitools.workspace.test)

;;; Metadata paths are followed as the kernel follows them: a `..` after a
;;; symlink applies to the link's target directory. /a/link leads into
;;; repository B, so /a/link/../r.git is B's r.git, not A's.
(defun %spoof-jj-host (git-target)
  (make-fake-workspace-boundaries
   :files (list (cons "/a/r.git/HEAD" "ref: refs/heads/main")
                (cons "/b/r.git/HEAD" "ref: refs/heads/main")
                (cons "/jjrepo/store/git_target" git-target)
                (cons "/a/r.git/.worktrees/jw/.jj/repo" "/jjrepo"))
   :directories (list "/a/r.git/objects" "/a/r.git/refs" "/b/r.git/objects" "/b/r.git/refs"
                      "/b/sub" "/jjrepo/op_store" "/jjrepo/op_heads"
                      "/a/r.git/.worktrees/jw/src")
   :symlinks (list (cons "/a/link" "/b/sub") (cons "/a/loop" "loop"))
   :cwd "/a/r.git/.worktrees/jw/src"))

(defun %spoof-worktree-host (back-pointer &key (dot-git "gitdir: ../../worktrees/wt")
                                             (commondir "../.."))
  (make-fake-workspace-boundaries
   :files (list (cons "/bare/HEAD" "ref: refs/heads/main")
                (cons "/bare/worktrees/wt/HEAD" "0123")
                (cons "/bare/worktrees/wt/commondir" commondir)
                (cons "/bare/worktrees/wt/gitdir" back-pointer)
                (cons "/bare/.worktrees/wt/.git" dot-git)
                (cons "/other/.worktrees/wt/.git" "gitdir: /other/worktrees/wt"))
   :directories (list "/bare/objects" "/bare/refs" "/bare/.worktrees/wt/src" "/other/sub")
   :symlinks (list (cons "/bare/decoy" "/other/sub")
                   (cons "/bare/worktrees/wt/loop" "loop")
                   (cons "/bare/.worktrees/wt/loop" "loop"))
   :cwd "/bare/.worktrees/wt"))

(describe "aitools.workspace.application metadata paths through symlinks"
  (it "refuses a jj workspace whose git_target reaches another repository through link/.."
    (dolist (target (list "/a/link/../r.git" "/b/r.git" "../../a/link/../r.git"
                          "/a/loop/../r.git"))
      (let* ((host (%spoof-jj-host target))
             (root (resolved-root host)))
        (expect (workspace-root-path root) :to-equal "/a/r.git/.worktrees/jw")
        (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory))))
  (it "allows a jj workspace whose git_target names the enclosing repository"
    (dolist (target (list "/a/r.git" "../../a/r.git" "/a/link/../../a/r.git"))
      (let* ((host (%spoof-jj-host target))
             (root (resolved-root host)))
        (expect (boundary-verdict host root "src/new.txt") :to-be :inside))))
  (it "refuses a worktree whose gitdir back-pointer reaches another checkout through link/.."
    (let* ((host (%spoof-worktree-host "../../decoy/../.worktrees/wt/.git"))
           (root (resolved-root host)))
      (expect (workspace-root-path root) :to-equal "/bare/.worktrees/wt")
      (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory))
    (let* ((host (%spoof-worktree-host "../../.worktrees/wt/.git"))
           (root (resolved-root host)))
      (expect (boundary-verdict host root "src/new.txt") :to-be :inside)))
  (it "follows a .git file and commondir through link/.. as the kernel does"
    (let* ((host
             (make-fake-workspace-boundaries
              :files (list (cons "/w/.git" "gitdir: link/../meta")
                           (cons "/other/meta/HEAD" "0123")
                           (cons "/other/meta/commondir" "link/../common")
                           (cons "/w/meta/HEAD" "0123"))
              :directories (list "/other/sub" "/other/common" "/w/common")
              :symlinks (list (cons "/w/link" "/other/sub")
                              (cons "/other/meta/link" "/other/sub"))
              :cwd "/w"))
           (repository (workspace-root-repository (resolved-root host))))
      (expect (git-repository-git-dir repository) :to-equal "/other/meta")
      (expect (git-repository-common-dir repository) :to-equal "/other/common"))))

;;; Every metadata pointer kind against every pointer state. Only a pointer
;;; the kernel can follow to the bare repository grants the exception; one it
;;; cannot reach (a loop, a missing or non-directory component, even when a
;;; later `..` would fold it onto the bare repository) grants nothing. A
;;; trailing `/` or `/.` requires a directory, so it keeps a directory target
;;; valid and makes a file target (the back-pointer's `.git`) unreachable.
(defparameter *metadata-pointer-kinds*
  ;; kind, pointer file, workspace root, absolute and relative valid text,
  ;; and whether the valid target is a directory
  '((:jj-repo "/bare/.worktrees/jw/.jj/repo" "/bare/.worktrees/jw"
     "/bare/.jj/repo" "../../../.jj/repo" t)
    (:git-target "/bare/.jj/repo/store/git_target" "/bare/.worktrees/jw"
     "/bare" "../../.." t)
    (:gitdir "/bare/.worktrees/wt/.git" "/bare/.worktrees/wt"
     "/bare/worktrees/wt" "../../worktrees/wt" t)
    (:commondir "/bare/worktrees/wt/commondir" "/bare/.worktrees/wt"
     "/bare" "../.." t)
    (:back-pointer "/bare/worktrees/wt/gitdir" "/bare/.worktrees/wt"
     "/bare/.worktrees/wt/.git" "../../.worktrees/wt/.git" nil)))

(defparameter *metadata-pointer-states*
  '(:absolute :relative :loop-parent :dangling :dangling-parent :file-parent :empty
    :unreadable :trailing-slash :trailing-dot :link-trailing-slash))

(defun %pointer-state-allowed-p (state directory-target-p)
  (or (member state '(:absolute :relative))
      (and directory-target-p
           (member state '(:trailing-slash :trailing-dot :link-trailing-slash)))))

(defun %pointer-state-text (state absolute relative)
  "The pointer text for STATE; NIL content makes the pointer file unreadable."
  (ecase state
    (:absolute absolute)
    (:relative relative)
    (:loop-parent (concatenate 'string "/x/loop/../.." absolute))
    (:dangling "/x/dangle")
    (:dangling-parent (concatenate 'string "/x/dangle/../.." absolute))
    (:file-parent (concatenate 'string "/x/plain/../.." absolute))
    (:empty "")
    (:unreadable nil)
    (:trailing-slash (concatenate 'string absolute "/"))
    (:trailing-dot (concatenate 'string absolute "/."))
    (:link-trailing-slash "/x/slashlink")))

(defun %metadata-pointer-host (kind text)
  (let ((files (append (%bare-jj-files)
                       (list (cons "/bare/.worktrees/jw/.jj/repo" "/bare/.jj/repo")
                             (cons "/bare/worktrees/wt/HEAD" "0123")
                             (cons "/bare/worktrees/wt/commondir" "../..")
                             (cons "/bare/worktrees/wt/gitdir" "../../.worktrees/wt/.git")
                             (cons "/bare/.worktrees/wt/.git" "gitdir: ../../worktrees/wt")
                             (cons "/x/plain" "not a directory"))))
        (row (assoc kind *metadata-pointer-kinds*)))
    (setf (cdr (assoc (second row) files :test #'string=))
          (if (and text (eq kind :gitdir)) (concatenate 'string "gitdir: " text) text))
    (make-fake-workspace-boundaries
     :files files
     :directories (append (%bare-jj-directories)
                          (list "/bare/.worktrees/jw/.jj/working_copy" "/bare/.worktrees/jw/src"
                                "/bare/.worktrees/wt/src"))
     :symlinks (list (cons "/x/loop" "loop") (cons "/x/dangle" "/missing")
                     (cons "/x/slashlink" (concatenate 'string (fourth row) "/")))
     :cwd "/")))

(describe "aitools.workspace.application metadata pointer states"
  (it "grants the exception only to a pointer the kernel can follow"
    (dolist (row *metadata-pointer-kinds*)
      (destructuring-bind (kind file root absolute relative directory-target-p) row
        (declare (ignore file))
        (dolist (state *metadata-pointer-states*)
          (let* ((host (%metadata-pointer-host
                        kind (%pointer-state-text state absolute relative)))
                 (verdict (boundary-verdict host (resolved-root host :root root) "src/new.txt")))
            (expect (list kind state verdict)
                    :to-equal (list kind state
                                    (if (%pointer-state-allowed-p state directory-target-p)
                                        :inside
                                        :git-directory))))))))
  (it "resolves a metadata path only when every component before the last is a directory"
    (let ((host (%metadata-pointer-host :jj-repo "/bare/.jj/repo")))
      (flet ((strict (path) (aitools.workspace.application::resolve-existing-path host path)))
        (expect (strict "/x/plain") :to-equal "/x/plain")
        (expect (strict "/x/plain/..") :to-be nil)
        (expect (strict "/x/plain/") :to-be nil)
        (expect (strict "/x/plain/.") :to-be nil)
        (expect (strict "/x/slashlink") :to-equal "/bare/.jj/repo")
        (expect (strict "/bare/") :to-equal "/bare")
        (expect (strict "/x/dangle/../bare") :to-be nil)
        (expect (strict "/x/loop/../bare") :to-be nil)
        (expect (strict "/bare/./worktrees/../HEAD") :to-equal "/bare/HEAD")
        (expect (strict "/../bare/HEAD") :to-equal "/bare/HEAD"))
      (let ((broken (make-fake-workspace-boundaries :symlinks '(("/r/broken" . nil)))))
        (expect (aitools.workspace.application::resolve-existing-path broken "/r/broken")
                :to-be nil))
      ;; Write targets keep a missing tail lexical, so the lenient resolver folds it.
      (expect (aitools.workspace.application::resolve-real-path host "/x/dangle/../bare")
              :to-equal "/bare")))
  (it "keeps an unresolvable commondir unfolded, refusing rather than registering"
    (let* ((host (%metadata-pointer-host :commondir "missing/../.."))
           (root (resolved-root host :root "/bare/.worktrees/wt")))
      (expect (git-repository-common-dir (workspace-root-repository root))
              :to-equal "/bare/worktrees/wt/missing/../..")
      (expect (boundary-verdict host root "src/new.txt") :to-be :git-directory)))
  (it "checks and writes the same real path when a write target folds .. over a dangling link"
    (let* ((host (%metadata-pointer-host :commondir "../.."))
           (root (resolved-root host :root "/bare/.worktrees/wt")))
      (expect (boundary-verdict host root "src/new.txt") :to-be :inside)
      ;; The verdict is the one for the folded path, which is also where the
      ;; write lands, and that path is refused.
      (loop for (target folded) in '(("/x/dangle/../bare/HEAD" "/bare/HEAD")
                                     ("/x/dangle/../bare/refs/x" "/bare/refs/x")
                                     ("/x/plain/../../bare/config" "/bare/config")
                                     ("src/dangle/../../../../HEAD" "/bare/HEAD"))
            do (let ((verdict (boundary-verdict host root target)))
                 (expect verdict :to-equal (boundary-verdict host root folded))
                 (expect (list target (eq verdict :inside)) :to-equal (list target nil)))))))
