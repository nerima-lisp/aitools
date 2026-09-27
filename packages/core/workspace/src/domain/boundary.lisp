;;;; packages/core/workspace/src/domain/boundary.lisp
;;;;
;;;; The write boundary as a pure verdict over paths the application
;;;; layer has already resolved. The real (symlink-resolved) paths decide:
;;;; comparing only the lexical path would let a symlink inside the root
;;;; redirect a write outside it, and comparing only the real path would
;;;; misreport a lexical `../` escape as a symlink problem.
(in-package #:aitools.workspace.domain)

(defun %git-component-p (relative)
  (some #'git-metadata-name-p (path-components relative)))

(defun write-target-verdict (&key root real-root target real-target temporary-root real-state-root
                               real-git-directories)
  "Classify a write to TARGET. ROOT/REAL-ROOT are the workspace root's
lexical and real paths, TARGET/REAL-TARGET the target's, TEMPORARY-ROOT the
real path of the mktemp area (or NIL), REAL-STATE-ROOT the real path of
`<state-home>/aitools` (or NIL), and REAL-GIT-DIRECTORIES the real
paths of the repository's git directory and common directory. All are
absolute and normalized.

Returns :INSIDE, :TEMPORARY (inside the mktemp area, the boundary's one exception),
:STATE-DIRECTORY (anywhere else in the state root: an intent record written
there would be rolled forward by the next command), :OUTSIDE-ROOT (the path
itself leaves the root), :SYMLINK-ESCAPE (the path is inside the root but a
symlink takes it outside), or :GIT-DIRECTORY (a `.git` component below the
root, or inside a git directory found by its real path, as `git init
--separate-git-dir` places it under any name)."
  (cond
    ((and temporary-root
          (path-inside-p temporary-root real-target)
          (string/= temporary-root real-target))
     :temporary)
    ((and real-state-root (path-inside-p real-state-root real-target))
     :state-directory)
    ((not (path-inside-p real-root real-target))
     (if (path-inside-p root target) :symlink-escape :outside-root))
    ((or (%git-component-p (path-relative-to real-root real-target))
         (and (path-inside-p root target)
              (%git-component-p (path-relative-to root target)))
         (some (lambda (directory) (path-inside-p directory real-target)) real-git-directories))
     :git-directory)
    (t :inside)))
