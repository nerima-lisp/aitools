;;;;
;;;; The write boundary as a pure verdict over paths the application
;;;; layer has already resolved. The real (symlink-resolved) paths decide:
;;;; comparing only the lexical path would let a symlink inside the root
;;;; redirect a write outside it, and comparing only the real path would
;;;; misreport a lexical `../` escape as a symlink problem.
(in-package #:aitools.workspace.domain)

(defun %git-component-p (relative)
  (some #'git-metadata-name-p (path-components relative)))

(defun git-metadata-path-inside-p (directory candidate)
  "True when CANDIDATE is DIRECTORY or below it, comparing normalized
components without case. This fail-closed match protects metadata when path
resolution preserves the caller spelling on case-insensitive filesystems."
  (let ((directory-components
          (mapcar (lambda (component)
                    (sb-unicode:normalize-string component :nfd))
                  (path-components directory)))
        (candidate-components
          (mapcar (lambda (component)
                    (sb-unicode:normalize-string component :nfd))
                  (path-components candidate))))
    (and (<= (length directory-components) (length candidate-components))
         (loop for directory-component in directory-components
               for candidate-component in candidate-components
               always (string-equal directory-component candidate-component)))))
;; Literal Git names and their source links live in data/domain/workspace/git-metadata-data.lisp.
;; The predicates below classify shared-index hashes and root-level refs.
(defun %git-common-root-base-entry-name-p (name)
  (or (member name aitools.data:*git-common-root-metadata-names*
              :test (function string-equal))
      (and (>= (length name) 12)
           (string-equal name "sharedindex." :end1 12)
           (or (= (- (length name) 12) 40)
               (= (- (length name) 12) 64))
           (every (lambda (character) (digit-char-p character 16))
                  (subseq name 12)))
      (and (>= (length name) 5)
           (string-equal name "_HEAD" :start1 (- (length name) 5))
           (every (lambda (character)
                    (find (char-upcase character) "ABCDEFGHIJKLMNOPQRSTUVWXYZ-_"))
                  name))))

(defun git-common-root-entry-name-p (name)
  "True for Git metadata names and concrete lock files at a common root."
  (when (stringp name)
    (let* ((name (sb-unicode:normalize-string name :nfd))
           (lock-suffix
             (and (>= (length name) 5)
                  (string-equal name ".lock" :start1 (- (length name) 5))))
           (entry-name
             (if lock-suffix (subseq name 0 (- (length name) 5)) name)))
      (or (and (not lock-suffix)
               (%git-common-root-base-entry-name-p name))
          (and lock-suffix
               (or (find entry-name
                         #("config" "config.worktree" "gc.pid" "index"
                           "packed-refs" "shallow")
                         :test (function string-equal))
                   (find entry-name
                         #("HEAD" "AUTO_MERGE" "BISECT_EXPECTED_REV"
                           "NOTES_MERGE_PARTIAL" "NOTES_MERGE_REF"
                           "MERGE_AUTOSTASH")
                         :test (function string-equal))
                   (and (>= (length entry-name) 5)
                        (string-equal entry-name "_HEAD"
                                      :start1 (- (length entry-name) 5))
                        (not (find entry-name
                                   #("FETCH_HEAD" "MERGE_HEAD")
                                   :test (function string-equal)))
                        (every (lambda (character)
                                 (let ((character (char-upcase character)))
                                   (or (and (char<= #\A character)
                                            (char<= character #\Z))
                                       (find character "-_"))))
                               entry-name))))))))
(defun git-common-root-metadata-path-p (directory root)
  "True when ROOT is DIRECTORY or lies below one of its Git entries."
  (let ((directory-components (path-components directory))
        (root-components (path-components root)))
    (and (<= (length directory-components) (length root-components))
         (git-metadata-path-inside-p directory root)
         (let ((entry (nth (length directory-components) root-components)))
           (or (null entry) (git-common-root-entry-name-p entry))))))


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
root, or inside a git directory found by its real path). Git metadata refusal
takes precedence over the temporary-area exception."
  (cond
    ((or (and (path-inside-p real-root real-target)
              (%git-component-p (path-relative-to real-root real-target)))
         (and (path-inside-p root target)
              (%git-component-p (path-relative-to root target)))
         (some (lambda (directory)
                 (and (or (path-inside-p real-root real-target)
                          (and temporary-root
                               (path-inside-p temporary-root real-target)))
                      (git-metadata-path-inside-p directory real-target)))
               real-git-directories))
     :git-directory)
    ((and temporary-root
          (path-inside-p temporary-root real-target)
          (string/= temporary-root real-target))
     :temporary)
    ((and real-state-root (path-inside-p real-state-root real-target))
     :state-directory)
    ((not (path-inside-p real-root real-target))
     (if (path-inside-p root target) :symlink-escape :outside-root))
    (t :inside)))
