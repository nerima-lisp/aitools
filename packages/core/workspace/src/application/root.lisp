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
  "The GIT-REPOSITORY whose top is DIRECTORY, or NIL when it has neither a
usable `.git` entry nor a common Git directory layout."
  (let* ((dot-git (join-path directory ".git"))
         (entry (host-stat host dot-git)))
    (or
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
                                  :common-dir (coerce common 'simple-string))))))
     ;; Git v2.55.0 setup.c:is_git_directory checks HEAD, objects/, and refs/.
     (let ((head (host-stat host (join-path directory "HEAD"))))
       (when (and head
                  (%directory-p host (join-path directory "objects"))
                  (%directory-p host (join-path directory "refs")))
         (make-git-repository :top (coerce directory 'simple-string)
                              :git-dir (coerce directory 'simple-string)
                              :common-dir (coerce directory 'simple-string)))))))

(defun find-git-repositories (host start)
  "All repositories rooted at START or an ancestor, innermost first."
  (loop for directory = start then (path-parent directory)
        while directory
        for repository = (%repository-at host directory)
        when repository collect repository))

(defun find-git-repository (host start)
  "The innermost GIT-REPOSITORY whose top is START or one of its ancestors,
or NIL. START is absolute and normalized."
  (loop for directory = start then (path-parent directory)
        while directory
        do (let ((repository (%repository-at host directory)))
             (when repository (return repository)))))
(defun %workspace-entry-same-identity-p (left right)
  (and left right
       (workspace-entry-device left)
       (workspace-entry-device right)
       (workspace-entry-inode left)
       (workspace-entry-inode right)
       (= (workspace-entry-device left) (workspace-entry-device right))
       (= (workspace-entry-inode left) (workspace-entry-inode right))))
(defun %same-file-identity-p (host left right)
  (let ((left-real (resolve-real-path host left))
        (right-real (resolve-real-path host right)))
    (and left-real
         right-real
         (%workspace-entry-same-identity-p
          (host-stat host left-real)
          (host-stat host right-real)))))
(defun %registered-linked-worktree-p (host worktree-top git-directory)
  (let* ((text (%read-text host (join-path git-directory "gitdir")))
         (back-pointer
           (and text
                (normalize-path
                 (string-trim (list (code-char 32) (code-char 9)
                                    (code-char 10) (code-char 13))
                              text))))
         (expected (join-path worktree-top ".git")))
    (and back-pointer
         (absolute-path-p back-pointer)
         (%same-file-identity-p host back-pointer expected))))
(defun %git-name-fold (name)
  (sb-unicode:casefold (sb-unicode:normalize-string name :nfd)))
(defun %git-path-has-dot-git-component-p (path)
  (aitools.workspace.domain:git-path-has-dot-git-component-p
   path :name-fold-function (function %git-name-fold)))
(defun %git-common-root-entry-name-p (name)
  (aitools.workspace.domain:git-common-root-entry-name-p
   name :name-fold-function (function %git-name-fold)))
(defun %git-common-root-metadata-path-p (directory root)
  (aitools.workspace.domain:git-common-root-metadata-path-p
   directory root :name-fold-function (function %git-name-fold)))
(defun %git-metadata-path-inside-p (directory candidate)
  (aitools.workspace.domain:git-metadata-path-inside-p
   directory candidate :name-fold-function (function %git-name-fold)))
(defun %git-common-root-identity-hazard-p (host common-directory root)
  (multiple-value-bind (entries readable-p complete-p)
      (host-list-directory host common-directory)
    (unless (and readable-p complete-p)
      (return-from %git-common-root-identity-hazard-p :stat-failed))
    (let* ((root-real (resolve-real-path host root))
           (root-entry (and root-real (host-stat host root-real))))
      (unless (and root-entry
                   (workspace-entry-device root-entry)
                   (workspace-entry-inode root-entry))
        (return-from %git-common-root-identity-hazard-p :stat-failed))
      (dolist (entry entries)
        (when (%git-common-root-entry-name-p (workspace-entry-name entry))
          (let* ((entry-path
                   (join-path common-directory (workspace-entry-name entry)))
                 (entry-real (resolve-real-path host entry-path))
                 (entry-target (and entry-real (host-stat host entry-real))))
            (unless (and entry-target
                         (workspace-entry-device entry-target)
                         (workspace-entry-inode entry-target))
              (return-from %git-common-root-identity-hazard-p :stat-failed))
            (when (or (%workspace-entry-same-identity-p root-entry entry-target)
                      (%git-metadata-path-inside-p entry-real root-real))
              (return-from %git-common-root-identity-hazard-p :identity-match)))))
      nil)))

(aitools.kernel.domain:define-flow
    (call-with-resolved-root/k host &key root on-resolved on-error)
  "Resolve the workspace root and call exactly one continuation.
ROOT is the `--root` value (absolute, or relative to the working directory)
or NIL. ON-RESOLVED receives a WORKSPACE-ROOT. ON-ERROR receives a
reason, :NOT-FOUND or :NOT-A-DIRECTORY, and the absolute path that failed."
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
          ((null entry)
           (aitools.kernel.domain:flow-continue :on-error :not-found path))
          ((not (eq (workspace-entry-kind entry) :directory))
           (aitools.kernel.domain:flow-continue :on-error :not-a-directory path))
          (t
           (aitools.kernel.domain:flow-continue
            :on-resolved
            (make-workspace-root :path (coerce path 'simple-string)
                                 :real (coerce real 'simple-string)
                                 :source source
                                 :repository (or repository
                                                 (find-git-repository host path))))))))))

(defun user-path-absolute (host path)
  "PATH as the user typed it, made absolute and normalized: a relative path
is taken from the process's working directory, as the shell tools aitools
replaces take it. `--root` never changes this base; it only selects the
workspace (boundary, state, ignore rules)."
  (normalize-path (join-path (host-current-directory host) path)))

(aitools.kernel.domain:define-flow
    (resolve-user-path/k host root path &key on-inside on-outside)
  "Resolve PATH against ROOT and call exactly one continuation:
ON-INSIDE (absolute relative) for a path inside the workspace, with the
workspace-relative path (empty for ROOT); ON-OUTSIDE (absolute) otherwise.
PATH is absolute or relative to the working directory, as typed. A path
through a symlinked parent of ROOT still resolves inside the workspace."
  (let* ((absolute (user-path-absolute host path))
         (relative (workspace-relative-path host root absolute)))
    (if relative
        (aitools.kernel.domain:flow-continue :on-inside absolute relative)
        (aitools.kernel.domain:flow-continue :on-outside absolute))))

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
