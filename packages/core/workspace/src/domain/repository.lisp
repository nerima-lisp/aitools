;;;; packages/core/workspace/src/domain/repository.lisp
;;;;
;;;; Values describing where a workspace is rooted (`--root`, then
;;;; the git top level, then the working directory) and the git repository
;;;; it belongs to, plus the `gitdir: <path>` file format a linked worktree
;;;; or submodule uses for its `.git` entry.
(in-package #:aitools.workspace.domain)

(defstruct (git-repository (:copier nil))
  "TOP is the working tree's top directory. GIT-DIR holds per-worktree state
(HEAD, index); COMMON-DIR holds shared state (config, info/exclude). They
differ only for a linked worktree. All three are absolute, normalized."
  (top "" :type simple-string :read-only t)
  (git-dir "" :type simple-string :read-only t)
  (common-dir "" :type simple-string :read-only t))

(defstruct (workspace-root (:copier nil))
  "PATH is the absolute, lexically normalized root; REAL has every symlink
resolved. SOURCE is :OPTION (`--root`), :GIT, or :CWD. REPOSITORY is the
GIT-REPOSITORY containing the root, or NIL."
  (path "" :type simple-string :read-only t)
  (real "" :type simple-string :read-only t)
  (source :cwd :type (member :option :git :cwd) :read-only t)
  (repository nil :type (or null git-repository) :read-only t))

(defun parse-gitdir-file (text)
  "The path named by a `.git` file's `gitdir: <path>` line, or NIL."
  (let* ((line (string-trim '(#\Space #\Tab #\Return #\Newline)
                            (subseq text 0 (or (position #\Newline text) (length text)))))
         (prefix "gitdir:"))
    (when (and (> (length line) (length prefix))
               (string= prefix line :end2 (length prefix)))
      (let ((path (string-trim '(#\Space #\Tab) (subseq line (length prefix)))))
        (and (plusp (length path)) path)))))
