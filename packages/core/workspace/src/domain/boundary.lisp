;;;;
;;;; The write boundary as a pure verdict over paths the application
;;;; layer has already resolved. The real (symlink-resolved) paths decide:
;;;; comparing only the lexical path would let a symlink inside the root
;;;; redirect a write outside it, and comparing only the real path would
;;;; misreport a lexical `../` escape as a symlink problem.
(in-package #:aitools.workspace.domain)

(defun %git-name-fold (name)
  (string-downcase name))
(defun %git-name-equal-p (left right name-fold-function)
  (string= (funcall name-fold-function left)
           (funcall name-fold-function right)))
(defun git-path-has-dot-git-component-p (path &key (name-fold-function (function %git-name-fold)))
  "True when PATH contains a `.git` component, using NAME-FOLD-FUNCTION."
  (some (lambda (component)
          (git-metadata-name-p (funcall name-fold-function component)))
        (path-components path)))

(defun git-metadata-path-inside-p (directory candidate &key (name-fold-function (function %git-name-fold)))
  "True when CANDIDATE lies below DIRECTORY after folding each component."
  (let ((directory-components
          (mapcar (lambda (component) (funcall name-fold-function component))
                  (path-components directory)))
        (candidate-components
          (mapcar (lambda (component) (funcall name-fold-function component))
                  (path-components candidate))))
    (and (<= (length directory-components) (length candidate-components))
         (loop for directory-component in directory-components
               for candidate-component in candidate-components
               always (string= directory-component candidate-component)))))
;; Literal Git names and their source links live in data/domain/workspace/git-metadata-data.lisp.
;; The predicates below classify shared-index hashes and root-level refs.
(defun %git-common-root-base-entry-name-p (name name-fold-function)
  (flet ((name-equal-p (left right)
           (%git-name-equal-p left right name-fold-function)))
    (let ((name (funcall name-fold-function name)))
      (or (member name aitools.data:*git-common-root-metadata-names*
                  :test (function name-equal-p))
          (and (>= (length name) 12)
               (string= name "sharedindex." :end1 12)
               (or (= (- (length name) 12) 40)
                   (= (- (length name) 12) 64))
               (every (lambda (character) (digit-char-p character 16))
                      (subseq name 12)))
          (and (>= (length name) 5)
               (string= name "_head" :start1 (- (length name) 5))
               (every (lambda (character)
                        (find (char-upcase character) "ABCDEFGHIJKLMNOPQRSTUVWXYZ-_"))
                      name))))))

(defun git-common-root-entry-name-p (name &key (name-fold-function (function %git-name-fold)))
  "True for Git metadata names and concrete lock files at a common root."
  (when (stringp name)
    (flet ((name-equal-p (left right)
             (%git-name-equal-p left right name-fold-function)))
      (let* ((name (funcall name-fold-function name))
             (lock-suffix
               (and (>= (length name) 5)
                    (string= name ".lock" :start1 (- (length name) 5))))
             (entry-name
               (if lock-suffix (subseq name 0 (- (length name) 5)) name)))
        (or (and (not lock-suffix)
                 (%git-common-root-base-entry-name-p name name-fold-function))
            (and lock-suffix
                 (or (find entry-name
                           #("config" "config.worktree" "gc.pid" "index"
                             "packed-refs" "shallow")
                           :test (function name-equal-p))
                     (find entry-name
                           #("HEAD" "AUTO_MERGE" "BISECT_EXPECTED_REV"
                             "NOTES_MERGE_PARTIAL" "NOTES_MERGE_REF"
                             "MERGE_AUTOSTASH")
                           :test (function name-equal-p))
                     (and (>= (length entry-name) 5)
                          (string= entry-name "_head"
                                   :start1 (- (length entry-name) 5))
                          (not (find entry-name
                                     #("FETCH_HEAD" "MERGE_HEAD")
                                     :test (function name-equal-p)))
                          (every (lambda (character)
                                   (find (char-upcase character)
                                         "ABCDEFGHIJKLMNOPQRSTUVWXYZ-_"))
                                 entry-name)))))))))
(defun git-common-root-metadata-path-p (directory root &key (name-fold-function (function %git-name-fold)))
  "True for a common-directory metadata entry or a path below one."
  (let ((directory-components (path-components directory))
        (root-components (path-components root)))
    (and (<= (length directory-components) (length root-components))
         (git-metadata-path-inside-p
          directory root :name-fold-function name-fold-function)
         (let ((entry (nth (length directory-components) root-components)))
           (or (null entry)
               (git-common-root-entry-name-p
                entry :name-fold-function name-fold-function))))))


(defun write-target-verdict (&key root real-root target real-target temporary-root real-state-root
                               real-git-directories
                               (name-fold-function (function %git-name-fold)))
  "Classify a write target using the lexical and real workspace paths.
ROOT and REAL-ROOT are the lexical and real paths of the workspace root.
TARGET and REAL-TARGET are the target paths. TEMPORARY-ROOT is the real
mktemp path or NIL. REAL-STATE-ROOT is the real state directory path or NIL.
REAL-GIT-DIRECTORIES contains the real Git and common-directory paths. Paths
are absolute and normalized.

Returns :INSIDE, :TEMPORARY (inside mktemp, the boundary exception),
:STATE-DIRECTORY (under state root, where intent records roll forward),
:OUTSIDE-ROOT, :SYMLINK-ESCAPE, or :GIT-DIRECTORY (a .git component below
the root or a real path inside Git metadata). Git metadata refusal takes
precedence over the temporary-area exception."
  (cond
    ((or (and (path-inside-p real-root real-target)
              (git-path-has-dot-git-component-p
               (path-relative-to real-root real-target)
               :name-fold-function name-fold-function))
         (and (path-inside-p root target)
              (git-path-has-dot-git-component-p
               (path-relative-to root target)
               :name-fold-function name-fold-function))
         (some (lambda (directory)
                 (and (or (path-inside-p real-root real-target)
                          (and temporary-root
                               (path-inside-p temporary-root real-target)))
                      (git-metadata-path-inside-p
                       directory real-target
                       :name-fold-function name-fold-function)))
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
