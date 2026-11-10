;;;;
;;;; The write boundary as a two-exit flow (inside, outside; see the CPS
;;;; audit in docs/src/reference/architecture.md). Reads are never checked; only callers about to write
;;;; call this. The verdict is recomputed from the host on every call rather
;;;; than trusted from an earlier resolution, because the write protocol runs this after
;;;; taking the workspace lock.
(in-package #:aitools.workspace.application)

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
refused. ROOT inside a Git common-directory entry is also refused as
:GIT-DIRECTORY.

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
         (repository (workspace-root-repository root))
         (real-root (workspace-root-real root))
         ;; Keep ancestor repositories so a nearer .git cannot hide a parent common dir.
         (repositories
           (remove nil
                   (append (list repository)
                           (find-git-repositories host (workspace-root-path root))
                           (find-git-repositories host real-root))))
         ;; TEMPORARY-ROOT bypasses ROOT, so check it against every discovered common dir.
         (common-root-metadata
           (some
            (lambda (candidate)
              (let* ((common-path (git-repository-common-dir candidate))
                     (common-directory
                       (and common-path
                            (resolve-real-path host common-path))))
                (or (null common-directory)
                    (%git-common-root-metadata-path-p common-path (workspace-root-path root))
                    (%git-common-root-metadata-path-p common-directory real-root)
                    (or (%git-common-root-identity-hazard-p host common-directory (workspace-root-path root))
                        (%git-common-root-identity-hazard-p host common-directory real-root))
                    (and temporary-real
                         real
                         (path-inside-p temporary-real real)
                         (or (%git-common-root-metadata-path-p common-path (normalize-path temporary-root))
                             (%git-common-root-metadata-path-p common-directory temporary-real)
                             (or (%git-common-root-identity-hazard-p host common-directory (normalize-path temporary-root))
                                 (%git-common-root-identity-hazard-p host common-directory temporary-real))
                             (%git-metadata-path-inside-p common-directory temporary-real))))))
            repositories))
         (git-real
           (remove nil
                   (loop for candidate in repositories
                         for git-directory =
                           (resolve-real-path host (git-repository-git-dir candidate))
                         for common-path = (git-repository-common-dir candidate)
                         for common-directory =
                           (and common-path (resolve-real-path host common-path))
                         for common-is-parent =
                           (and common-directory
                                (%git-metadata-path-inside-p common-directory real-root)
                                (not (%git-metadata-path-inside-p real-root common-directory)))
                         for common-is-child =
                           (and common-directory
                                (%git-metadata-path-inside-p real-root common-directory)
                                (not (%git-metadata-path-inside-p common-directory real-root)))
                         append
                           (append
                            ;; Only an equal bare git/common ancestor needs omission; the root-entry rule guards it.
                            (unless
                                (and common-is-parent
                                     git-directory
                                     common-directory
                                     (%git-metadata-path-inside-p common-directory git-directory)
                                     (%git-metadata-path-inside-p git-directory common-directory)
                                     (some
                                      (lambda (worktree)
                                        (let* ((worktree-top (resolve-real-path host (git-repository-top worktree)))
                                               (worktree-git-directory
                                                (resolve-real-path host (git-repository-git-dir worktree)))
                                               (common-path (git-repository-common-dir worktree))
                                               (worktree-common-directory
                                                (and common-path (resolve-real-path host common-path)))
                                               (worktree-git-parent
                                                (and worktree-git-directory (path-parent worktree-git-directory))))
                                          (and worktree-top
                                               worktree-git-directory
                                               worktree-common-directory
                                               worktree-git-parent
                                               (and (%git-metadata-path-inside-p common-directory worktree-common-directory)
                                                    (%git-metadata-path-inside-p worktree-common-directory common-directory))
                                               (not (and (%git-metadata-path-inside-p worktree-common-directory worktree-git-directory)
                                                         (%git-metadata-path-inside-p worktree-git-directory worktree-common-directory)))
                                               (and (%git-metadata-path-inside-p real-root worktree-top)
                                                    (%git-metadata-path-inside-p worktree-top real-root))
                                               (and (%git-metadata-path-inside-p (join-path worktree-common-directory "worktrees") worktree-git-parent)
                                                    (%git-metadata-path-inside-p worktree-git-parent (join-path worktree-common-directory "worktrees")))
                                               (%registered-linked-worktree-p host worktree-top worktree-git-directory))))
                                      repositories))
                              (when git-directory (list git-directory)))
                            (when common-is-child (list common-directory)))))))
    (cond
      ((or (%git-path-has-dot-git-component-p (workspace-root-path root))
       (%git-path-has-dot-git-component-p real-root)
       common-root-metadata)
       (aitools.kernel.domain:flow-continue
        :on-outside :git-directory lexical real))
      ((null real)
       (aitools.kernel.domain:flow-continue
        :on-outside :unresolvable lexical nil))
      ((and real-root
            (some (lambda (candidate)
                    (null (resolve-real-path
                           host (git-repository-git-dir candidate))))
                  repositories)
            (path-inside-p real-root real))
       (aitools.kernel.domain:flow-continue
        :on-outside :git-directory lexical real))
      (t
       (let ((verdict (write-target-verdict :root (workspace-root-path root)
                       :real-root real-root
                       :target lexical
                       :real-target real
                       :temporary-root temporary-real
                       :real-state-root state-real
                       :real-git-directories git-real :name-fold-function (function %git-name-fold))))
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
             (make-workspace-path
              (coerce real (quote simple-string))
              (coerce real (quote simple-string)))
             verdict))
           (t
            (aitools.kernel.domain:flow-continue
             :on-outside verdict lexical real))))))))
