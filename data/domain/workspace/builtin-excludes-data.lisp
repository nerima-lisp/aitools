;;;; data/domain/workspace/builtin-excludes-data.lisp
;;;;
;;;; The fixed exclude list used to scan a workspace that
;;;; is not inside a git repository. Written in .gitignore syntax so the same
;;;; matcher (workspace domain gitignore.lisp) interprets it; every pattern is
;;;; unanchored, so it applies at any depth below the workspace root.
(in-package #:aitools.data)

(defparameter *workspace-builtin-exclude-patterns*
  '(".git"
    ".hg"
    ".svn"
    ".direnv"
    ".venv"
    ".tox"
    ".mypy_cache"
    ".pytest_cache"
    "__pycache__"
    "node_modules/"
    "target/"
    "dist/"
    "result"
    "result-*")
  "Gitignore lines for the builtin exclude list. `result` and
`result-*` are not directory-only because nix-build leaves them as symlinks.")

(export '*workspace-builtin-exclude-patterns*)
