;;;;
;;;; Reading the small metadata files that tie a checkout to its repository:
;;;; pointer files whose text names a path relative to a base directory
;;;; (`gitdir`, `.jj/repo`, jj's `store/git_target`), and the jj repository
;;;; a jj workspace's `.jj/repo` leads to. Read directly; no git or jj process
;;;; is started.
(in-package #:aitools.workspace.application)

(defun %directory-p (host path)
  (let ((real (resolve-real-path host path)))
    (and real
         (let ((entry (host-stat host real)))
           (and entry (eq (workspace-entry-kind entry) :directory))))))

(defun %read-text (host path)
  (let ((octets (host-read-octets host path)))
    (and octets (decode-git-text octets))))

(defun %metadata-path (host base text)
  "TEXT, a path read from metadata, made absolute against BASE and resolved
the way the kernel follows it (RESOLVE-EXISTING-PATH): `link/../r.git` names
the directory beside the link's target, and NIL when the kernel could not
reach it (a loop, a missing or non-directory component), so an unreachable
pointer grants nothing."
  (resolve-existing-path host (join-path base text)))

(defun %read-relative-path (host file base)
  "FILE's text, trimmed of surrounding whitespace, as a path: itself when
absolute, else relative to BASE, resolved by %METADATA-PATH. NIL when FILE
cannot be read or its path cannot be resolved, so the pointer grants
nothing."
  (let ((text (%read-text host file)))
    (and text
         (%metadata-path host base
                         (string-trim (list (code-char 32) (code-char 9)
                                            (code-char 10) (code-char 13))
                                      text)))))

(defun %jj-repository-directory (host workspace-top)
  "The jj repository directory WORKSPACE-TOP's `.jj/repo` leads to, or NIL
unless it holds `store/`, `op_store/`, and `op_heads/`. `.jj/repo` is that
directory in the workspace that created the repository; in a workspace added
later it is a file naming it, absolute or relative to `.jj/`."
  (let* ((jj-directory (join-path workspace-top ".jj"))
         (repo (join-path jj-directory "repo"))
         (entry (and (%directory-p host jj-directory) (host-stat host repo)))
         (directory
           (cond ((null entry) nil)
                 ((%directory-p host repo) repo)
                 ((eq (workspace-entry-kind entry) :file)
                  (%read-relative-path host repo jj-directory)))))
    (and directory
         (every (lambda (name) (%directory-p host (join-path directory name)))
                (list "store" "op_store" "op_heads"))
         directory)))

(defun %jj-git-target (host repository-directory)
  "The Git directory REPOSITORY-DIRECTORY's `store/git_target` names, or NIL.
jj writes it relative to `store/` (`../../..` for a repository initialized
in a bare Git directory) or absolute."
  (let ((store (join-path repository-directory "store")))
    (%read-relative-path host (join-path store "git_target") store)))
