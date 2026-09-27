;;;; packages/core/workspace/src/application/ignore-context.lisp
;;;;
;;;; Everything the ignore decision needs besides per-directory
;;;; .gitignore files, read once per scan: git config (system, global, local,
;;;; worktree, and GIT_CONFIG_COUNT pairs, with include.path), the global
;;;; excludes file, $GIT_COMMON_DIR/info/exclude, core.ignoreCase, and the
;;;; tracked paths of the index. Outside a git repository the context holds
;;;; only the builtin exclude list.
;;;;
;;;; Known gaps against git, none of which the parity fixtures exercise:
;;;; git's compiled-in system config path ($(prefix)/etc/gitconfig) is not
;;;; knowable here, so /etc/gitconfig (or $GIT_CONFIG_SYSTEM) is read;
;;;; includeIf is not evaluated; a relative core.excludesFile is resolved
;;;; against the repository top rather than git's working directory.
(in-package #:aitools.workspace.application)

(defstruct (ignore-context (:copier nil))
  "SOURCE is :GITIGNORE, :BUILTIN, or :NONE (`--no-ignore`). TOP is the
absolute directory ignore paths are relative to (the repository top, or the
workspace root outside git); PREFIX is the workspace root relative to TOP.
BASE-STACK holds the lists below every .gitignore in precedence order:
info/exclude, then the global excludes file (or the builtin list). TRACKED is
the index's sorted path vector, or NIL."
  (source :none :type (member :gitignore :builtin :none) :read-only t)
  (casefold nil :type boolean :read-only t)
  (top "" :type string :read-only t)
  (prefix "" :type string :read-only t)
  (root-path "" :type string :read-only t)
  (base-stack '() :type list :read-only t)
  (tracked nil :type (or null simple-vector) :read-only t))

(defparameter *config-include-depth-limit* 10
  "git's MAX_INCLUDE_DEPTH.")

(defun %config-entries-from-file (host path depth)
  "PATH's config entries with every include.path spliced in place."
  (let ((text (%read-text host path)))
    (when text
      (loop for entry in (parse-git-config text)
            if (and (string-equal (car entry) "include.path")
                    (stringp (cdr entry))
                    (< depth *config-include-depth-limit*))
              append (%config-entries-from-file
                      host
                      (normalize-path (join-path (or (path-parent path) "/")
                                                 (expand-config-path (cdr entry) (host-home-directory host))))
                      (1+ depth))
            else collect entry))))

(defun %xdg-git-path (host name)
  (let ((xdg (host-getenv host "XDG_CONFIG_HOME")))
    (if xdg
        (join-path (join-path xdg "git") name)
        (join-path (host-home-directory host) (join-path ".config/git" name)))))

(defun %environment-config-entries (host)
  "git's GIT_CONFIG_COUNT / GIT_CONFIG_KEY_<n> / GIT_CONFIG_VALUE_<n> pairs."
  (let* ((count-text (host-getenv host "GIT_CONFIG_COUNT"))
         (count (and count-text (every (lambda (char) (char<= #\0 char #\9)) count-text)
                     (parse-integer count-text))))
    (loop for i from 0 below (or count 0)
          for key = (host-getenv host (format nil "GIT_CONFIG_KEY_~D" i))
          when key
            collect (cons (string-downcase key)
                          (or (host-getenv host (format nil "GIT_CONFIG_VALUE_~D" i)) "")))))

(defun %repository-config (host repository)
  (let* ((home (host-home-directory host))
         (system (unless (git-config-boolean (host-getenv host "GIT_CONFIG_NOSYSTEM"))
                   (%config-entries-from-file
                    host (or (host-getenv host "GIT_CONFIG_SYSTEM") "/etc/gitconfig") 0)))
         (global-override (host-getenv host "GIT_CONFIG_GLOBAL"))
         (global (if global-override
                     (%config-entries-from-file host global-override 0)
                     (append (%config-entries-from-file host (%xdg-git-path host "config") 0)
                             (%config-entries-from-file host (join-path home ".gitconfig") 0))))
         (local (%config-entries-from-file
                 host (join-path (git-repository-common-dir repository) "config") 0))
         (worktree (when (git-config-boolean (git-config-value local "extensions.worktreeconfig"))
                     (%config-entries-from-file
                      host (join-path (git-repository-git-dir repository) "config.worktree") 0))))
    (append system global local worktree (%environment-config-entries host))))

(defun %ignore-file-list (host path source)
  (let ((octets (host-read-octets host path)))
    (and octets (parse-ignore-octets octets :source source))))

(defun %tracked-paths (host repository config)
  (let ((octets (host-read-octets host (join-path (git-repository-git-dir repository) "index")))
        (hash-size (if (equalp (git-config-value config "extensions.objectformat") "sha256") 32 20)))
    (cond ((null octets) #())
          (t (handler-case (parse-git-index-paths octets :hash-size hash-size)
               (git-index-error () nil))))))

(defun load-ignore-context (host root &key no-ignore)
  "The IGNORE-CONTEXT for scanning below ROOT (a WORKSPACE-ROOT)."
  (let ((repository (workspace-root-repository root))
        (root-path (workspace-root-path root)))
    (cond
      (no-ignore
       (make-ignore-context :source :none :top root-path :root-path root-path))
      (repository
       (let* ((config (%repository-config host repository))
              (home (host-home-directory host))
              (top (git-repository-top repository))
              (excludes-setting (git-config-value config "core.excludesfile"))
              (excludes-path (if (stringp excludes-setting)
                                 (normalize-path (join-path top (expand-config-path excludes-setting home)))
                                 (%xdg-git-path host "ignore")))
              (info (%ignore-file-list host (join-path (git-repository-common-dir repository) "info/exclude")
                                       :info-exclude))
              (global (%ignore-file-list host excludes-path :excludes-file)))
         (make-ignore-context
          :source :gitignore
          :casefold (git-config-boolean (git-config-value config "core.ignorecase"))
          :top top
          :prefix (path-relative-to top root-path)
          :root-path root-path
          :base-stack (remove nil (list info global))
          :tracked (%tracked-paths host repository config))))
      (t
       (make-ignore-context :source :builtin :top root-path :root-path root-path
                            :base-stack (list (builtin-ignore-list)))))))
