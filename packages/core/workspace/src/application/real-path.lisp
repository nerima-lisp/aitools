;;;; packages/core/workspace/src/application/real-path.lisp
;;;;
;;;; realpath(3) over the WORKSPACE-HOST port, one lstat per component. The
;;;; nonexistent tail of a path is kept lexically, so the boundary check can
;;;; resolve the real location of a file that a write is about to create.
(in-package #:aitools.workspace.application)

(defparameter *symlink-hop-limit* 40
  "Linux's MAXSYMLINKS; a longer chain is treated as a loop (ELOOP).")

(defvar *realpath-cache* nil
  "When bound to an EQUAL hash table, RESOLVE-REAL-PATH memoizes the real
path of PATH and of every directory prefix it crosses before any symlink
expansion, so one pass resolving many targets in shared directories walks
each directory once and resolves a repeated root (the mktemp area, a git
dir) once. Bind it only for the span of a single pass, never across the
workspace lock: a recheck under the lock must read the disk afresh.")

(defun %pure-path-components-p (components)
  "True when COMPONENTS hold no `.` or `..`, so each lexical prefix resolves
independently of the rest and may be memoized and reused as a seed."
  (notany (lambda (component) (or (string= component ".") (string= component ".."))) components))

(defun resolve-real-path (host path)
  "Resolve every symlink in the absolute PATH. Returns the real path, or NIL
when resolution exceeds *SYMLINK-HOP-LIMIT* hops. `..` after a resolved
symlink applies to the link's target directory, as the kernel does.

With *REALPATH-CACHE* bound the result is memoized and each directory prefix
crossed before a symlink is reused, but the resolution is otherwise
identical to the uncached walk of one LSTAT per component."
  (let ((cache *realpath-cache*))
    (when cache
      (multiple-value-bind (hit present) (gethash path cache)
        (when present (return-from resolve-real-path hit))))
    (let* ((components (path-components path))
           (pure (and cache (absolute-path-p path) (%pure-path-components-p components)))
           (resolved "/")
           (lexical "/")
           (hops 0))
      ;; Seed from the longest cached pure prefix: RESOLVE(prefix) is known,
      ;; so the walk continues from there instead of re-stat'ing it. A prefix
      ;; cached as unresolvable (NIL) makes PATH unresolvable too.
      (when pure
        (loop while components
              do (let ((candidate (join-path lexical (first components))))
                   (multiple-value-bind (hit present) (gethash candidate cache)
                     (cond
                       ((not present) (return))
                       ((null hit)
                        (when cache (setf (gethash path cache) nil))
                        (return-from resolve-real-path nil))
                       (t (setf resolved hit lexical candidate components (rest components))))))))
      (let ((pending components))
        (loop while pending
              do (let ((component (pop pending)))
                   (cond
                     ((string= component "."))
                     ((string= component "..")
                      (setf resolved (or (path-parent resolved) "/"))
                      (setf pure nil))
                     (t
                      (let* ((candidate (join-path resolved component))
                             (entry (host-stat host candidate)))
                        (if (and entry (eq (workspace-entry-kind entry) :symlink))
                            (let ((target (host-read-link host candidate)))
                              (when (or (null target) (> (incf hops) *symlink-hop-limit*))
                                (when cache (setf (gethash path cache) nil))
                                (return-from resolve-real-path nil))
                              (when (absolute-path-p target) (setf resolved "/"))
                              (setf pending (append (path-components target) pending))
                              (setf pure nil))
                            (progn
                              (setf resolved candidate)
                              (when pure
                                (setf lexical (join-path lexical component))
                                (setf (gethash lexical cache) resolved))))))))))
      (when cache (setf (gethash path cache) resolved))
      resolved)))
