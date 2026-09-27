;;;; packages/core/workspace/src/application/root.lisp
;;;;
;;;; Root resolution: `--root`, else the git top level found by
;;;; walking up from the working directory, else the working directory. The
;;;; git top level is found by the presence of a `.git` directory or
;;;; `gitdir:` file, read directly; no git process is started.
(in-package #:aitools.workspace.application)

(defun %directory-p (host path)
  (let ((real (resolve-real-path host path)))
    (and real
         (let ((entry (host-stat host real)))
           (and entry (eq (workspace-entry-kind entry) :directory))))))

(defun %read-text (host path)
  (let ((octets (host-read-octets host path)))
    (and octets (decode-git-text octets))))

(defun %repository-at (host directory)
  "The GIT-REPOSITORY whose top is DIRECTORY, or NIL when DIRECTORY has no
usable `.git` entry. A `.git` directory must contain HEAD, as git requires."
  (let* ((dot-git (join-path directory ".git"))
         (entry (host-stat host dot-git)))
    (when entry
      (let ((git-dir
              (if (and (eq (workspace-entry-kind entry) :file))
                  (let* ((text (%read-text host dot-git))
                         (target (and text (parse-gitdir-file text))))
                    (and target (normalize-path (join-path directory target))))
                  (and (%directory-p host dot-git) dot-git))))
        (when (and git-dir (host-stat host (join-path git-dir "HEAD")))
          (let* ((commondir-text (%read-text host (join-path git-dir "commondir")))
                 (common (if commondir-text
                             (normalize-path
                              (join-path git-dir (string-trim '(#\Space #\Tab #\Return #\Newline)
                                                              commondir-text)))
                             git-dir)))
            (make-git-repository :top (coerce directory 'simple-string)
                                 :git-dir (coerce git-dir 'simple-string)
                                 :common-dir (coerce common 'simple-string))))))))

(defun find-git-repository (host start)
  "The innermost GIT-REPOSITORY whose top is START or one of its ancestors,
or NIL. START is absolute and normalized."
  (loop for directory = start then (path-parent directory)
        while directory
        do (let ((repository (%repository-at host directory)))
             (when repository (return repository)))))

(defun call-with-resolved-root/k (host &key root on-resolved on-error)
  "Resolve the workspace root and call exactly one continuation.

ROOT is the `--root` value (absolute, or relative to the working directory)
or NIL. ON-RESOLVED receives a WORKSPACE-ROOT. ON-ERROR receives a reason,
:NOT-FOUND or :NOT-A-DIRECTORY, and the absolute path that failed."
  (declare (type function on-resolved on-error))
  (let ((cwd (normalize-path (host-current-directory host))))
    (multiple-value-bind (path source repository)
        (if root
            (let ((path (normalize-path (join-path cwd root))))
              (values path :option nil))
            (let ((repository (find-git-repository host cwd)))
              (if repository
                  (values (git-repository-top repository) :git repository)
                  (values cwd :cwd nil))))
      (let* ((real (resolve-real-path host path))
             (entry (and real (host-stat host real))))
        (cond
          ((null entry) (funcall on-error :not-found path))
          ((not (eq (workspace-entry-kind entry) :directory))
           (funcall on-error :not-a-directory path))
          (t
           (funcall on-resolved
                    (make-workspace-root :path (coerce path 'simple-string)
                                         :real (coerce real 'simple-string)
                                         :source source
                                         :repository (or repository (find-git-repository host path))))))))))

(defun user-path-absolute (host path)
  "PATH as the user typed it, made absolute and normalized: a relative path
is taken from the process's working directory, as the shell tools aitools
replaces take it. `--root` never changes this base; it only selects the
workspace (boundary, state, ignore rules)."
  (normalize-path (join-path (host-current-directory host) path)))

(defun resolve-user-path/k (host root path &key on-inside on-outside)
  "Resolve PATH (as typed: absolute, or relative to the working directory)
against ROOT (a WORKSPACE-ROOT) and call exactly one continuation:
ON-INSIDE (absolute relative) when it names the root or something below it,
RELATIVE being the workspace-relative path (\"\" for the root itself), the
form every output `path` uses; ON-OUTSIDE (absolute) otherwise. The path is
compared lexically with the root as given and with its real path, then
through its own real path, so a path typed through a symlinked parent of the
root still maps into the workspace."
  (declare (type function on-inside on-outside))
  (let* ((absolute (user-path-absolute host path))
         (relative (workspace-relative-path host root absolute)))
    (if relative
        (funcall on-inside absolute relative)
        (funcall on-outside absolute))))

(defun workspace-relative-path (host root absolute)
  "ABSOLUTE (a normalized absolute path) relative to ROOT (a WORKSPACE-ROOT),
\"\" for the root itself, or NIL when it lies outside. Compared lexically
with the root as given, then with the root's real path, then through
ABSOLUTE's own real path: under `--root /tmp/x` with /tmp a symlink (macOS),
the working directory, and so every path typed relative to it, is under
/private/tmp/x, and must still map into the workspace."
  (flet ((relative-to (base candidate)
           (and candidate (path-inside-p base candidate) (path-relative-to base candidate))))
    (or (relative-to (workspace-root-path root) absolute)
        (relative-to (workspace-root-real root) absolute)
        (relative-to (workspace-root-real root) (resolve-real-path host absolute)))))
