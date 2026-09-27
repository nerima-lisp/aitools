;;;; packages/core/kernel/src/domain/path.lisp
;;;;
;;;; A pure value type for the three path representations `info`
;;;; reports (absolute, real, relative) plus the string arithmetic the write
;;;; boundary needs to decide whether one path is inside another. Resolving a real
;;;; path (following symlinks) and deciding what "the workspace root" is are
;;;; both I/O; those live in the workspace context's application layer, which
;;;; calls WORKSPACE-PATH-INSIDE-P and PATH-RELATIVE-TO below with strings it
;;;; already resolved.
(in-package #:aitools.kernel.domain)

(defstruct (workspace-path
            (:constructor make-workspace-path (absolute real relative))
            (:copier nil))
  "ABSOLUTE and REAL are namestrings without a trailing separator (except the
filesystem root); REAL has had every symlink component resolved. RELATIVE is
the workspace-root-relative namestring, using `/` regardless of platform,
without a leading `/` and without a leading `./`."
  (absolute nil :type simple-string :read-only t)
  (real nil :type simple-string :read-only t)
  (relative nil :type simple-string :read-only t))

(declaim (inline %strip-trailing-separator))
(defun %strip-trailing-separator (namestring)
  (let ((length (length namestring)))
    (if (and (> length 1) (char= (char namestring (1- length)) #\/))
        (subseq namestring 0 (1- length))
        namestring)))

(defun path-inside-p (root candidate)
  "True when CANDIDATE (an absolute namestring) names ROOT itself or an entry
underneath it. Pure string comparison: ROOT and CANDIDATE must already be
normalized (no `.`/`..` components, symlinks resolved by the caller) or the
result is meaningless. This is the write boundary test, applied by workspace
application code to both the raw target path and its symlink-resolved form.
The filesystem root `/` contains every absolute path."
  (let ((root (%strip-trailing-separator root))
        (candidate (%strip-trailing-separator candidate)))
    (or (string= root candidate)
        (and (> (length candidate) (length root))
             (string= root candidate :end2 (length root))
             ;; ROOT `/` already ends in the separator the next test looks for.
             (or (string= root "/")
                 (char= (char candidate (length root)) #\/))))))

(defun path-relative-to (root absolute)
  "Return ABSOLUTE's namestring relative to ROOT, using `/` separators and no
leading `/`. Signals a simple error when ABSOLUTE is not inside ROOT (checked
with PATH-INSIDE-P) -- callers only reach this after that check has passed."
  (unless (path-inside-p root absolute)
    (error "~S is not inside ~S" absolute root))
  (let* ((root (%strip-trailing-separator root))
         (absolute (%strip-trailing-separator absolute))
         (root-length (length root)))
    (if (= (length absolute) root-length)
        ""
        (subseq absolute (if (char= (char absolute root-length) #\/)
                              (1+ root-length)
                              root-length)))))
