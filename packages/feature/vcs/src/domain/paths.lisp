;;;; packages/feature/vcs/src/domain/paths.lisp
;;;;
;;;; User paths are relative to the process's working directory; git paths
;;;; are relative to the repository top, where every git command runs. These
;;;; convert between the two on `/`-separated absolute directory strings, so
;;;; a command given from a subdirectory names the same file git reports.
(in-package #:aitools.vcs.domain)

(defun %path-segments (path)
  "The segments of absolute PATH with `.`, `..`, and empty segments resolved."
  (let ((segments nil) (start 0) (length (length path)))
    (loop while (<= start length)
          do (let* ((end (or (position #\/ path :start start) length))
                    (segment (subseq path start end)))
               (cond ((or (string= segment "") (string= segment ".")))
                     ((string= segment "..") (pop segments))
                     (t (push segment segments)))
               (setf start (1+ end))))
    (nreverse segments)))

(defun %join-segments (segments)
  (if segments (format nil "~{~A~^/~}" segments) "."))

(defun %absolute-segments (path directory)
  (%path-segments (if (and (plusp (length path)) (char= (char path 0) #\/))
                      path
                      (concatenate 'string directory "/" path))))

(defun repository-relative-path (path directory top)
  "PATH (absolute, or relative to DIRECTORY) relative to the repository TOP,
`.` for TOP itself, or NIL when PATH lies outside TOP."
  (let ((target (%absolute-segments path directory))
        (top-segments (%path-segments top)))
    (when (and (<= (length top-segments) (length target))
               (every #'string= top-segments target))
      (%join-segments (nthcdr (length top-segments) target)))))

(defun path-from-directory (repository-path top directory)
  "REPOSITORY-PATH (relative to the repository TOP) as a path relative to
DIRECTORY, using `..` where DIRECTORY lies below or beside it."
  (let* ((target (%absolute-segments repository-path top))
         (base (%path-segments directory))
         (common (or (mismatch target base :test #'string=) (length target))))
    (%join-segments (append (make-list (- (length base) common) :initial-element "..")
                            (nthcdr common target)))))
