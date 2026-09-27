;;;; packages/core/workspace/src/domain/path-syntax.lisp
;;;;
;;;; Lexical POSIX path arithmetic on plain strings. Paths never go through
;;;; CL pathname parsing here: SBCL's namestring syntax treats `*`, `?`, `[`
;;;; and `\` specially, and a workspace may contain files with those bytes in
;;;; their names. Symlink resolution is I/O and lives in the application
;;;; layer; everything here is purely textual.
(in-package #:aitools.workspace.domain)

(defun absolute-path-p (path)
  (and (plusp (length path)) (char= (char path 0) #\/)))

(defun path-components (path)
  "PATH split on `/`, without empty components."
  (let ((components '()) (start 0) (length (length path)))
    (loop for i from 0 to length
          do (when (or (= i length) (char= (char path i) #\/))
               (when (> i start)
                 (push (subseq path start i) components))
               (setf start (1+ i))))
    (nreverse components)))

(defun normalize-path (path)
  "Lexically normalize PATH: drop empty and `.` components and fold `..`
into its parent. An absolute result starts with `/` and has no trailing
separator (except `/` itself); `..` above the filesystem root stays at the
root. A relative result keeps leading `..` components and is \"\" when
nothing remains."
  (let ((absolute (absolute-path-p path))
        (stack '()))
    (dolist (component (path-components path))
      (cond ((string= component "."))
            ((string= component "..")
             (cond ((and stack (not (string= (first stack) ".."))) (pop stack))
                   (absolute)
                   (t (push ".." stack))))
            (t (push component stack))))
    (let ((joined (format nil "~{~A~^/~}" (reverse stack))))
      (if absolute (concatenate 'string "/" joined) joined))))

(defun join-path (directory name)
  "NAME appended to DIRECTORY with one `/`. An absolute NAME replaces
DIRECTORY; an empty DIRECTORY yields NAME unchanged."
  (cond ((absolute-path-p name) name)
        ((zerop (length directory)) name)
        ((zerop (length name)) directory)
        ((char= (char directory (1- (length directory))) #\/)
         (concatenate 'string directory name))
        (t (concatenate 'string directory "/" name))))

(defun path-parent (path)
  "The lexical parent of a normalized PATH: \"/a/b\" -> \"/a\", \"/a\" ->
\"/\", \"a/b\" -> \"a\", \"a\" -> \"\". NIL for \"/\" and \"\"."
  (let ((slash (position #\/ path :from-end t)))
    (cond ((or (string= path "/") (string= path "")) nil)
          ((null slash) "")
          ((zerop slash) "/")
          (t (subseq path 0 slash)))))

(defun path-basename (path)
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))
