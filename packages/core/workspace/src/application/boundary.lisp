;;;;
;;;; The write boundary as a two-exit flow (inside, outside; see the CPS
;;;; audit in docs/src/reference/architecture.md). Reads are never checked; only callers about to write
;;;; call this. The verdict is recomputed from the host on every call rather
;;;; than trusted from an earlier resolution, because the write protocol runs this after
;;;; taking the workspace lock.
(in-package #:aitools.workspace.application)

(defun %common-root-name-verdict (common-path common-directory root-path real-root temporary-root temporary-real real-target)
  (if (or (%git-common-root-metadata-path-p common-path root-path)
          (%git-common-root-metadata-path-p common-directory real-root)
          (and temporary-real
               real-target
               (path-inside-p temporary-real real-target)
               (or (%git-common-root-metadata-path-p common-path temporary-root)
                   (%git-common-root-metadata-path-p common-directory temporary-real)
                   (%git-metadata-path-inside-p common-directory temporary-real))))
      :git-directory
    nil))

(defun %common-root-identity-verdict (host common-directory root-path real-root temporary-root temporary-real real-target)
  (if (or (%git-common-root-identity-hazard-p host common-directory root-path real-target)
          (%git-common-root-identity-hazard-p host common-directory real-root real-target)
          (and temporary-real
               real-target
               (path-inside-p temporary-real real-target)
               (or (%git-common-root-identity-hazard-p
                    host common-directory temporary-root real-target)
                   (%git-common-root-identity-hazard-p
                    host common-directory temporary-real real-target))))
      :git-directory
    nil))

(defun %common-root-metadata-verdict (host candidate root-path real-root temporary-root temporary-real real-target)
  (let* ((common-path (git-repository-common-dir candidate))
         (common-directory
          (and common-path (resolve-real-path host common-path))))
    (if (null common-directory)
        :git-directory
      (or (%common-root-name-verdict common-path common-directory root-path real-root temporary-root temporary-real real-target)
          (%common-root-identity-verdict host common-directory root-path real-root temporary-root temporary-real real-target)))))

(defun %same-git-path-p (left right)
  (and left
       right
       (%git-metadata-path-inside-p left right)
       (%git-metadata-path-inside-p right left)))

(defun %registered-linked-worktree-candidate-verdict (host common-directory real-root worktree)
  (let* ((worktree-top (resolve-real-path host (git-repository-top worktree)))
         (worktree-git-directory
          (resolve-real-path host (git-repository-git-dir worktree)))
         (common-path (git-repository-common-dir worktree))
         (worktree-common-directory
          (and common-path (resolve-real-path host common-path)))
         (worktree-git-parent
          (and worktree-git-directory (path-parent worktree-git-directory)))
         (registered-p
          (and worktree-top
               worktree-git-directory
               worktree-common-directory
               worktree-git-parent
               (%same-git-path-p common-directory worktree-common-directory)
               (not (%same-git-path-p worktree-common-directory worktree-git-directory))
               (%same-git-path-p real-root worktree-top)
               (%same-git-path-p (join-path worktree-common-directory "worktrees")
                                 worktree-git-parent)
               (%registered-linked-worktree-p host worktree-top worktree-git-directory))))
    (if registered-p :registered-linked-worktree :unregistered-linked-worktree)))

(defun %path-below-has-jj-component-p (directory path)
  (some (lambda (component) (string= (%git-name-fold component) ".jj"))
        (nthcdr (length (path-components directory)) (path-components path))))

(defun %jj-metadata-directory (host workspace-top)
  "The real `.jj` directory of WORKSPACE-TOP when it is a jj workspace."
  (and (%jj-repository-directory host workspace-top)
       (resolve-real-path host (join-path workspace-top ".jj"))))

(defun %linked-worktree-exception-verdict (host common-directory real-root git-directory repositories)
  ;; A checkout below a bare common directory is a workspace of its own when
  ;; its metadata says so: a registered linked worktree, or a jj workspace.
  ;; Never inside a `.jj` directory, which holds jj repository state.
  (if (and (%same-git-path-p common-directory git-directory)
           (%git-metadata-path-inside-p common-directory real-root)
           (not (%git-metadata-path-inside-p real-root common-directory))
           (not (%path-below-has-jj-component-p common-directory real-root))
           (or (%jj-repository-directory host real-root)
               (some (lambda (worktree)
                       (eq (%registered-linked-worktree-candidate-verdict
                            host common-directory real-root worktree)
                           :registered-linked-worktree))
                     repositories)))
      :omit-common-directory
    :retain-common-directory))

(defun %repository-git-directories (host real-root repositories candidate)
  (let* ((git-directory
          (resolve-real-path host (git-repository-git-dir candidate)))
         (common-path (git-repository-common-dir candidate))
         (common-directory
          (and common-path (resolve-real-path host common-path)))
         (linked-worktree-verdict
          (%linked-worktree-exception-verdict
           host common-directory real-root git-directory repositories))
         (common-is-child
          (and common-directory
               (%git-metadata-path-inside-p real-root common-directory)
               (not (%git-metadata-path-inside-p common-directory real-root)))))
    (append
     (when (and git-directory
                (not (eq linked-worktree-verdict :omit-common-directory)))
       (list git-directory))
     (when common-is-child (list common-directory)))))

(defun %real-git-directories (host real-root repositories)
  (remove nil
          (append
           (loop for candidate in repositories
                 append (%repository-git-directories host real-root repositories candidate))
           (list (%jj-metadata-directory host real-root)))))

(defun %workspace-root-metadata-verdict (host root real-root real repositories temporary-root temporary-real)
  (or (and (or (%git-path-has-dot-git-component-p (workspace-root-path root))
               (%git-path-has-dot-git-component-p real-root))
           :git-directory)
      (some (lambda (candidate)
              (%common-root-metadata-verdict
               host candidate (workspace-root-path root) real-root
               (and temporary-root (normalize-path temporary-root))
               temporary-real real))
            repositories)))

(defun %unresolved-git-directory-verdict (host real-root real repositories)
  (if (and real
           real-root
           (path-inside-p real-root real)
           (some (lambda (candidate)
                   (null (resolve-real-path host (git-repository-git-dir candidate))))
                 repositories))
      :git-directory
    nil))

  (aitools.kernel.domain:define-flow
      (call-with-workspace-boundary/k host root target
        &key temporary-root state-root on-inside on-outside)
    "Check a write to TARGET against ROOT (a WORKSPACE-ROOT) and call exactly
  one continuation.

  TARGET is absolute, or relative to ROOT path. TEMPORARY-ROOT is the
  absolute mktemp area, supplied by the store context, or NIL.
  STATE-ROOT is the <state-home>/aitools path (WORKSPACE-STATE-ROOT), or NIL.
  Every target there except the mktemp area is refused as :STATE-DIRECTORY.
  The repository git directory and any common directory at or below ROOT are
  refused, except that a common directory above ROOT stays writable outside
  its metadata entries when ROOT is a registered linked worktree or a jj
  workspace. ROOT inside a Git common-directory entry is also refused as
  :GIT-DIRECTORY, as is the `.jj` directory of a jj workspace ROOT.

  ON-INSIDE receives a kernel WORKSPACE-PATH and the verdict (:INSIDE or
  :TEMPORARY). For :TEMPORARY, the path RELATIVE slot holds the real path.
  ON-OUTSIDE receives the verdict (:OUTSIDE-ROOT, :SYMLINK-ESCAPE,
  :GIT-DIRECTORY, :STATE-DIRECTORY, or :UNRESOLVABLE for a symlink loop),
  the lexical target, and the real target (NIL when unresolvable)."
    (let* ((lexical (normalize-path (join-path (workspace-root-path root) target)))
           (real (resolve-real-path host lexical))
           (temporary-real
             (and temporary-root
                  (resolve-real-path host (normalize-path temporary-root))))
           (state-real
             (and state-root
                  (resolve-real-path host (normalize-path state-root))))
           (real-root (workspace-root-real root))
           (repositories
             (remove nil
                     (append (list (workspace-root-repository root))
                             (find-git-repositories host (workspace-root-path root))
                             (find-git-repositories host real-root))))
           (metadata-verdict
             (%workspace-root-metadata-verdict host root real-root real repositories temporary-root temporary-real))
           (unresolved-git-verdict
             (%unresolved-git-directory-verdict host real-root real repositories))
           (verdict
             (or metadata-verdict
                 (and (null real) :unresolvable)
                 unresolved-git-verdict
                 (write-target-verdict
                  :root (workspace-root-path root)
                  :real-root real-root
                  :target lexical
                  :real-target real
                  :temporary-root temporary-real
                  :real-state-root state-real
                  :real-git-directories (%real-git-directories host real-root repositories)
                  :name-fold-function (function %git-name-fold)))))
      (case verdict
        (:inside
         (aitools.kernel.domain:flow-continue
          :on-inside
          (make-workspace-path
           (coerce real (quote simple-string))
           (coerce (path-relative-to real-root real) (quote simple-string)))
          verdict))
        (:temporary
         (aitools.kernel.domain:flow-continue
          :on-inside
          (make-workspace-path (coerce real (quote simple-string))
                               (coerce real (quote simple-string)))
          verdict))
        (t
         (aitools.kernel.domain:flow-continue
          :on-outside verdict lexical real)))))
