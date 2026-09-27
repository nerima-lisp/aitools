;;;; packages/core/workspace/src/domain/builtin-excludes.lisp
;;;;
;;;; The names every scan skips regardless of ignore files (`.git`,
;;;; which git itself never lists, and the write protocol's `.aitools-*.tmp` temporary
;;;; files), plus the builtin exclude list used outside git repositories.
(in-package #:aitools.workspace.domain)

(defparameter *builtin-ignore-list*
  (parse-ignore-lines aitools.data:*workspace-builtin-exclude-patterns* :source :builtin))

(defun builtin-ignore-list ()
  *builtin-ignore-list*)

(defun aitools-temporary-name-p (name)
  "True for the write protocol's in-place temporary files, `.aitools-<op_id>-<n>.tmp`."
  (let ((prefix ".aitools-") (suffix ".tmp"))
    (and (>= (length name) (+ (length prefix) (length suffix)))
         (string= prefix name :end2 (length prefix))
         (string= suffix name :start2 (- (length name) (length suffix))))))

(defun git-metadata-name-p (name)
  "True for a `.git` entry. Compared case-insensitively: on a case-insensitive
filesystem `.GIT` names the same directory, and treating it as `.git`
everywhere only ever refuses more."
  (string-equal name ".git"))
