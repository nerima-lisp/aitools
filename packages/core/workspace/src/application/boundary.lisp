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

TARGET is absolute, or relative to ROOT's path. TEMPORARY-ROOT is the
absolute mktemp area, supplied by the store context, or NIL.
STATE-ROOT is the <state-home>/aitools path (WORKSPACE-STATE-ROOT), or NIL.
Every target there except the mktemp area is refused as :STATE-DIRECTORY.
ROOT's git directory and common directory are refused as :GIT-DIRECTORY.

ON-INSIDE receives a kernel WORKSPACE-PATH and the verdict (:INSIDE or
:TEMPORARY). For :TEMPORARY, the path's RELATIVE slot holds the real path.
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
         (git-real
           (and repository
                (remove nil
                        (mapcar
                         (lambda (directory) (resolve-real-path host directory))
                         (list (git-repository-git-dir repository)
                               (git-repository-common-dir repository)))))))
    (if (null real)
        (aitools.kernel.domain:flow-continue
         :on-outside :unresolvable lexical nil)
        (let ((verdict (write-target-verdict
                        :root (workspace-root-path root)
                        :real-root (workspace-root-real root)
                        :target lexical
                        :real-target real
                        :temporary-root temporary-real
                        :real-state-root state-real
                        :real-git-directories git-real)))
          (case verdict
            (:inside
             (aitools.kernel.domain:flow-continue
              :on-inside
              (make-workspace-path
               (coerce lexical 'simple-string)
               (coerce real 'simple-string)
               (coerce (path-relative-to (workspace-root-real root) real)
                       'simple-string))
              verdict))
            (:temporary
             (aitools.kernel.domain:flow-continue
              :on-inside
              (make-workspace-path (coerce lexical 'simple-string)
                                   (coerce real 'simple-string)
                                   (coerce real 'simple-string))
              verdict))
            (t
             (aitools.kernel.domain:flow-continue
              :on-outside verdict lexical real)))))))
