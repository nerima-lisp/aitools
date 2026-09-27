;;;; packages/core/workspace/src/application/boundary.lisp
;;;;
;;;; The write boundary as a two-exit flow (inside, outside; see the CPS
;;;; audit in docs/src/reference/architecture.md). Reads are never checked; only callers about to write
;;;; call this. The verdict is recomputed from the host on every call rather
;;;; than trusted from an earlier resolution, because the write protocol runs this after
;;;; taking the workspace lock.
(in-package #:aitools.workspace.application)

(defun call-with-workspace-boundary/k (host root target &key temporary-root state-root on-inside on-outside)
  "Check a write to TARGET against ROOT (a WORKSPACE-ROOT) and call exactly
one continuation.

TARGET is absolute, or relative to the root's path. TEMPORARY-ROOT is the
absolute path of the mktemp area, supplied by the store context, or NIL.
STATE-ROOT is the absolute path of `<state-home>/aitools`
(AITOOLS.STORE.APPLICATION:WORKSPACE-STATE-ROOT), or NIL; every target in it
except the mktemp area is refused as :STATE-DIRECTORY. The repository's git
directory and common directory (ROOT's GIT-REPOSITORY) are refused by their
real paths as :GIT-DIRECTORY.

ON-INSIDE receives a kernel WORKSPACE-PATH and the verdict (:INSIDE or
:TEMPORARY). For :TEMPORARY the path's RELATIVE slot holds the real absolute
path, since the target is not below the root.
ON-OUTSIDE receives the verdict (:OUTSIDE-ROOT, :SYMLINK-ESCAPE,
:GIT-DIRECTORY, :STATE-DIRECTORY, or :UNRESOLVABLE for a symlink loop), the
lexical target, and the real target (NIL when unresolvable)."
  (declare (type function on-inside on-outside))
  (let* ((lexical (normalize-path (join-path (workspace-root-path root) target)))
         (real (resolve-real-path host lexical))
         (temporary-real (and temporary-root
                              (resolve-real-path host (normalize-path temporary-root))))
         (state-real (and state-root (resolve-real-path host (normalize-path state-root))))
         (repository (workspace-root-repository root))
         (git-real (and repository
                        (remove nil (mapcar (lambda (directory) (resolve-real-path host directory))
                                            (list (git-repository-git-dir repository)
                                                  (git-repository-common-dir repository)))))))
    (if (null real)
        (funcall on-outside :unresolvable lexical nil)
        (let ((verdict (write-target-verdict :root (workspace-root-path root)
                                             :real-root (workspace-root-real root)
                                             :target lexical
                                             :real-target real
                                             :temporary-root temporary-real
                                             :real-state-root state-real
                                             :real-git-directories git-real)))
          (case verdict
            (:inside
             (funcall on-inside
                      (make-workspace-path (coerce lexical 'simple-string)
                                           (coerce real 'simple-string)
                                           (coerce (path-relative-to (workspace-root-real root) real)
                                                   'simple-string))
                      verdict))
            (:temporary
             (funcall on-inside
                      (make-workspace-path (coerce lexical 'simple-string)
                                           (coerce real 'simple-string)
                                           (coerce real 'simple-string))
                      verdict))
            (t (funcall on-outside verdict lexical real)))))))
